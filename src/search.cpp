/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2022 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <algorithm>
#include <atomic>
#include <cassert>
#include <cmath>
#include <iostream>
#include <sstream>

#include "bitboard.h"
#include "misc.h"
#include "movegen.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "uci.h"
#include "syzygy/tbprobe.h"

namespace Search {

  LimitsType Limits;
}

namespace Tablebases {

  int Cardinality;
  bool RootInTB;
  bool UseRule50;
  Depth ProbeDepth;
}

namespace TB = Tablebases;

using std::string;
using namespace Search;

namespace {

  // Basic piece values used for move-ordering
  constexpr int MVVLVA[PIECE_TYPE_NB] = { 0, 100, 300, 305, 500, 900, 0, 0 };

  // Helper used to detect a basic mate config
  bool is_basic_mate(const Position& pos) {
  
    Color us = pos.side_to_move();

    Value npm =  pos.count<KNIGHT>(us) * KnightValueMg
               + pos.count<BISHOP>(us) * BishopValueMg
               + pos.count<ROOK  >(us) * RookValueMg
               + pos.count<QUEEN >(us) * QueenValueMg;
           
    return   !more_than_one(pos.pieces(~us))
          && !pos.count<PAWN>(us)
          && (   npm == RookValueMg
              || npm == QueenValueMg
              || npm == BishopValueMg * 2
              || npm == KnightValueMg * 3
              || npm == KnightValueMg + BishopValueMg);
  }

  // Function prototypes
  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth);
  Value syzygy_search(Position& pos, int ply);

  // Global variables
  int allMoves, kingMoves;
  std::atomic<int> Movecount[MAX_PLY];

  // perft() is our utility to verify move generation. All the leaf nodes up
  // to the given depth are generated and counted, and the sum is returned.
  template<bool Root>
  uint64_t perft(Position& pos, Depth depth) {

    StateInfo st;
    uint64_t cnt, nodes = 0;
    const bool leaf = (depth == 2);

    for (const auto& m : MoveList<LEGAL>(pos))
    {
        if (Root && depth <= 1)
            cnt = 1, nodes++;
        else
        {
            pos.do_move(m, st);
            cnt = leaf ? MoveList<LEGAL>(pos).size() : perft<false>(pos, depth - 1);
            nodes += cnt;
            pos.undo_move(m);
        }

        if (Root)
            sync_cout << UCI::move(m, pos.is_chess960()) << ": " << cnt << sync_endl;
    }

    return nodes;
  }

} // namespace


/// Search::init() is called just before a new search is started,
/// and reads in some UCI options and prepares the root moves for
/// each thread.

void Search::init(Position& pos) {

  // Read UCI options
  kingMoves = Options["KingMoves"];
  allMoves  = Options["AllMoves"];

  // Initialize Movecount array
  for (int i = 0; i < MAX_PLY; i++)
      Movecount[i] = 0;

  // Analyze the root position in order to find some
  // automatic settings for the search if possible.
  const Color us = pos.side_to_move();
  const auto king = pos.square<KING>(~us);
  const Bitboard kingRing = pos.attacks_from<KING>(king);

  // Prepare the root moves
  RootMoves searchMoves;
  StateInfo rootSt;

  for (const auto& m : MoveList<LEGAL>(pos))
      if (   Limits.searchmoves.empty()
          || std::count(Limits.searchmoves.begin(), Limits.searchmoves.end(), m))
          searchMoves.emplace_back(m);

  // Now we rank the root moves for the mate search.
  // First, try ranking by TBs.
  TB::rank_root_moves(pos, searchMoves);

  if (TB::RootInTB)
      pos.this_thread()->tbHits = searchMoves.size();

  else
  {
      for (auto& rm : searchMoves)
      {
          rm.tbRank = 0;

          // Check bonuses
          if (pos.gives_check(rm.pv[0]))
          {
              rm.tbRank += 6000;

              // Bonus for a knight check
              if (type_of(pos.moved_piece(rm.pv[0])) == KNIGHT)
                  rm.tbRank += 400;

              // Bonus for queen/rook contact checks
              else if (  (type_of(pos.moved_piece(rm.pv[0])) == QUEEN
                       || type_of(pos.moved_piece(rm.pv[0])) == ROOK)
                       && distance(king, to_sq(rm.pv[0])) == 1)
                   rm.tbRank += 500;
          }

          // Bonus for captures by MVV
          if (pos.capture(rm.pv[0]))
              rm.tbRank += MVVLVA[type_of(pos.piece_on(to_sq(rm.pv[0])))];

          // Bonus for a move freeing a potential promotion square
          Bitboard ourPawns = pos.pieces(us, PAWN);
    
          if (   (us == WHITE && shift<NORTH>(ourPawns) & Rank8BB & from_sq(rm.pv[0]))
              || (us == BLACK && shift<SOUTH>(ourPawns) & Rank1BB & from_sq(rm.pv[0])))
              rm.tbRank += 500;          

          // Bonus for a knight eventually able to give check on the next move
          if (type_of(pos.moved_piece(rm.pv[0])) == KNIGHT)
          {
              if (pos.attacks_from<KNIGHT>(to_sq(rm.pv[0])) & pos.check_squares(KNIGHT))
                  rm.tbRank += 600;
          }

          // Bonus for a queen eventually able to give check on the next move
          else if (type_of(pos.moved_piece(rm.pv[0])) == QUEEN)
          {
              if (pos.attacks_from<QUEEN>(to_sq(rm.pv[0])) & pos.check_squares(QUEEN))
                  rm.tbRank += 500;

              rm.tbRank += 8 * (PseudoAttacks[QUEEN][to_sq(rm.pv[0])] & kingRing);
          }

          // Bonus for a rook eventually able to give check on the next move
          else if (type_of(pos.moved_piece(rm.pv[0])) == ROOK)
          {
              if (pos.attacks_from<ROOK>(to_sq(rm.pv[0])) & pos.check_squares(ROOK))
                  rm.tbRank += 400;

              rm.tbRank += 32 * (PseudoAttacks[ROOK][to_sq(rm.pv[0])] & kingRing);
          }

          // Bonus for a bishop eventually able to give check on the next move
          else if (type_of(pos.moved_piece(rm.pv[0])) == BISHOP)
          {
              if (pos.attacks_from<BISHOP>(to_sq(rm.pv[0])) & pos.check_squares(BISHOP))
                  rm.tbRank += 300;
          }

          // Bonus for pawns
          if (type_of(pos.moved_piece(rm.pv[0])) == PAWN)
              rm.tbRank +=   64 * edge_distance(file_of(to_sq(rm.pv[0])))
                          + 128 * relative_rank(us, to_sq(rm.pv[0]));

          // R-Mobility (kind of ?)
          pos.do_move(rm.pv[0], rootSt);
          rm.tbRank -= 10 * int(MoveList<LEGAL>(pos).size());
          pos.undo_move(rm.pv[0]);
      }
  }

  // Now, sort the moves by their rank
  std::stable_sort(searchMoves.begin(), searchMoves.end(),
      [](const RootMove &a, const RootMove &b) { return a.tbRank > b.tbRank; });

  // If requested, print out the root moves and their ranking
  if (Options["RootMoveStats"])
      for (const auto& rm : searchMoves)
          std::cout << "Root move: " << UCI::move(rm.pv[0], pos.is_chess960()) << "   Rank: " << rm.tbRank << std::endl;
  
  // Finally, distribute the ranked root moves among all available threads
  auto it = searchMoves.begin();

  while (it < searchMoves.end())
  {
      for (Thread* th : Threads)
      {
          th->rootMoves.emplace_back(*it);
          it++;

          if (it == searchMoves.end())
              break;
      }
  }

  assert(Threads.nodes_searched() == 0);
}


/// Search::clear() resets search state to its initial value

void Search::clear() {

  Threads.main()->wait_for_search_finished();

  Threads.clear();
  Tablebases::init(Options["SyzygyPath"]); // Free mapped files
}


/// MainThread::search() is started when the program receives the UCI 'go'
/// command. It searches from the root position and outputs the "bestmove".

void MainThread::search() {

  // Special case 1: 'go perft x' 
  if (Limits.perft)
  {
      nodes = perft<true>(rootPos, Limits.perft);
      sync_cout << "\nNodes searched: " << nodes << "\n" << sync_endl;

      return;
  }

  // Special case 2: no move(s) to search
  if (rootMoves.empty())
  {
      // Must be mate or stalemate
      sync_cout << "info depth 0 score "
                << UCI::value(rootPos.checkers() ? -VALUE_MATE : VALUE_DRAW)
                << std::endl;
      std::cout << "bestmove " << UCI::move(MOVE_NULL, rootPos.is_chess960()) << sync_endl;

      return;
  }

  Time.init(Limits, rootPos.side_to_move(), rootPos.game_ply());

  for (Thread* th : Threads)
      if (th != this)
          th->start_searching();

  Thread::search(); // Let's start searching!

  while (!Threads.stop && Limits.infinite)
  {} // Busy wait for a stop

  // Wait until all threads have finished
  for (Thread* th : Threads)
      if (th != this)
          th->wait_for_search_finished();

  // Stop the threads if not already stopped
  Threads.stop = true;

  Thread* bestThread = this;

  for (Thread* th : Threads)
      if (  !th->rootMoves.empty()
          && th->rootMoves[0].score > bestThread->rootMoves[0].score)
          bestThread = th;

  if (bestThread->rootMoves[0].score < VALUE_MATE_IN_MAX_PLY)
      sync_cout << "info string Failure! No mate found!" << sync_endl;

  // Send best move and ponder move (if available)
  sync_cout << "bestmove " << UCI::move(bestThread->rootMoves[0].pv[0], bestThread->rootPos.is_chess960());

  if (bestThread->rootMoves[0].pv.size() > 1)
      std::cout << " ponder " << UCI::move(bestThread->rootMoves[0].pv[1], bestThread->rootPos.is_chess960());

  std::cout << sync_endl;
}


/// Thread::search() is the main iterative deepening loop. It calls search()
/// repeatedly with increasing depth until the allocated thinking time has been
/// consumed, the user stops the search, or the maximum search depth is reached.

void Thread::search() {

  // Do we have a basic endgame mate like KQK, KRK,
  // KBBK, KBNK or KNNNK? Then we don't need to search
  // but we can get a mate line using the syzygy dtz tables.
  if (   TB::RootInTB
      && is_basic_mate(rootPos))
  {
      if (   this == Threads.main()
          && rootMoves[0].tbRank > 900)
      {
          StateInfo rootSt;
          rootMoves[0].pv.resize(1);

          rootPos.do_move(rootMoves[0].pv[0], rootSt);
          rootMoves[0].score = -syzygy_search(rootPos, 1);
          rootPos.undo_move(rootMoves[0].pv[0]);

          rootDepth = rootMoves[0].selDepth;
          sync_cout << UCI::pv(rootPos, rootDepth) << sync_endl;
      }

      return;
  }
  
  Stack stack[MAX_PLY+1], *ss = stack;
  StateInfo rootSt;
  Value alpha, beta, bestValue, value;
  
  for (int i = 0; i <= MAX_PLY; ++i)
      (ss+i)->ply = i;

  ss->pv.clear();

  targetDepth = Limits.mate ? 2 * Limits.mate - 1 : MAX_PLY;
  fullDepth = std::max(targetDepth - 4, 1);
  size_t multiPV = rootMoves.size();

  // Setting alpha, beta and bestValue such that we achieve
  // many beta cutoffs on odd plies.
  alpha = VALUE_MATE - 2 * Limits.mate;
  beta  = VALUE_INFINITE;
  bestValue = VALUE_MATE_IN_MAX_PLY - 1;

  while (rootDepth <= targetDepth)
  {
      for (pvIdx = 0; pvIdx < multiPV; ++pvIdx)
      {
          // Only search winning moves
          if (   TB::RootInTB
              && rootMoves[pvIdx].tbRank <= 0)
              continue;

          if (  !TB::RootInTB
              && rootDepth == 1
              && rootMoves[pvIdx].tbRank < 5000)
              continue;

          ++Movecount[rootDepth];

          if (Time.elapsed() > 300 && this == Threads.main())
              sync_cout << "info currmove "  << UCI::move(rootMoves[pvIdx].pv[0], rootPos.is_chess960())
                        << " currmovenumber " << Movecount[rootDepth].load() << sync_endl;

          selDepth = 0;

          assert(is_ok(rootMoves[pvIdx].pv[0]));
          
          // Make, search and undo the root move
          rootPos.do_move(rootMoves[pvIdx].pv[0], rootSt);
          nodes++;

          value = -::search(rootPos, ss+1, -beta, -alpha, rootDepth-1);

          rootPos.undo_move(rootMoves[pvIdx].pv[0]);

          if (value > bestValue)
          {
              bestValue = value;

              // Assign the search value, selective search depth
              // and the pv to this root move.
              rootMoves[pvIdx].score = value;
              rootMoves[pvIdx].selDepth = selDepth;
              rootMoves[pvIdx].pv.resize(1);

              // Append child pv
              for (auto& m : (ss+1)->pv)
                  rootMoves[pvIdx].pv.push_back(m);

              // Sort the PV lines searched so far
              std::stable_sort(rootMoves.begin(), rootMoves.begin() + pvIdx + 1);
          }

          // Have we found a "mate in x" within the specified limit?
          if (bestValue >= alpha)
          {
              Threads.stop = true;

              sync_cout << "info string Success! Mate in "
                        << (VALUE_MATE - rootMoves[0].score + 1) / 2 << " found!" << sync_endl;
              sync_cout << UCI::pv(rootPos, rootDepth) << sync_endl;
          }

          if (Threads.stop.load())
              break;
      }

      if (Threads.stop.load())
          break;

      if (this == Threads.main())
      {
          rootMoves[0].selDepth = rootDepth;
          sync_cout << UCI::pv(rootPos, rootDepth) << sync_endl;
      }

      rootDepth += 2;
  }
}


namespace {

  // search<>() is the main search function

  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth) {

    StateInfo st;
    Value bestValue, value;
    bool inCheck = !!pos.checkers();
    int moveCount;
    Depth extension;
    Color us = pos.side_to_move();
    Thread* thisThread = pos.this_thread();

    assert(-VALUE_INFINITE <= alpha && alpha < beta && beta <= VALUE_INFINITE);

    // Start with a fresh pv
    ss->pv.clear();

    // Check for aborted search or maximum ply reached
    if (   Threads.stop.load()
        || ss->ply == MAX_PLY)
        return VALUE_ZERO;

    // At the leafs, we simply either return a mate score
    // or zero. No evaluation needed!
    if (depth == 0)
    {
        thisThread->selDepth = std::max(thisThread->selDepth, ss->ply);

        if (inCheck && !MoveList<LEGAL>(pos).size())
            return mated_in(ss->ply);
        else
            return VALUE_DRAW;
    }

    if (ss->ply & 1)
    {
        if (   kingMoves < 8
            && int(MoveList<LEGAL, KING>(pos).size()) > kingMoves)
            return VALUE_DRAW;

        if (   allMoves < 250
            && int(MoveList<LEGAL>(pos).size()) > allMoves)
            return VALUE_DRAW;
    }

    // Check for draw by repetition
    if (pos.is_draw(ss->ply))
        return VALUE_DRAW;

    // TODO: Tablebase probe
    // For the root color we can immediately return on
    // TB draws or losses

    bestValue = -VALUE_INFINITE;
    moveCount = 0;
    auto rankThisMove = 0;

    std::vector<RankedMove> legalMoves;
    legalMoves.reserve(64);

    [[maybe_unused]] Bitboard ourPawns = pos.pieces(us, PAWN);

    // Score the moves! VERY IMPORTANT!!!
    for (const auto& m : MoveList<LEGAL>(pos))
    {
        // Checking moves get a high enough rank for both sides
        if (pos.gives_check(m))
            rankThisMove += 6000;

        if (pos.capture(m))
            rankThisMove += MVVLVA[type_of(pos.piece_on(to_sq(m)))];

        if (ss->ply & 1) // Side to get mated
        {
            if (inCheck)
            {
                // Rank moves first which capture the checking piece
                if (pos.capture(m))
                    rankThisMove += 1000;
            }
        }
        else
        {
            if (rankThisMove >= 6000) // Checking move
            {
                // Bonus for a knight check
                if (type_of(pos.moved_piece(m)) == KNIGHT)
                    rankThisMove += 400;

                // Bonus for queen/rook contact checks
                else if (  (type_of(pos.moved_piece(m)) == QUEEN
                         || type_of(pos.moved_piece(m)) == ROOK)
                         && distance(pos.square<KING>(~us), to_sq(m)) == 1)
                   rankThisMove += 500;
            }

            // Bonus for a move freeing a potential promotion square
            if (   (us == WHITE && shift<NORTH>(ourPawns) & Rank8BB & from_sq(m))
                || (us == BLACK && shift<SOUTH>(ourPawns) & Rank1BB & from_sq(m)))
                rankThisMove += 500;          

            // Bonus for a knight eventually able to give check on the next move
            if (   type_of(pos.moved_piece(m)) == KNIGHT
                && pos.attacks_from<KNIGHT>(to_sq(m)) & pos.check_squares(KNIGHT))
                rankThisMove += 600;

            // Bonus for a queen eventually able to give check on the next move
            else if (   type_of(pos.moved_piece(m)) == QUEEN
                && pos.attacks_from<QUEEN>(to_sq(m)) & pos.check_squares(QUEEN))
                rankThisMove += 500;

            // Bonus for a rook eventually able to give check on the next move
            else if (   type_of(pos.moved_piece(m)) == ROOK
                && pos.attacks_from<ROOK>(to_sq(m)) & pos.check_squares(ROOK))
                rankThisMove += 400;

            // Bonus for a bishop eventually able to give check on the next move
            else if (   type_of(pos.moved_piece(m)) == BISHOP
                && pos.attacks_from<BISHOP>(to_sq(m)) & pos.check_squares(BISHOP))
                rankThisMove += 300;
        }

        // Add this ranked move
        legalMoves.emplace_back(RankedMove(m, rankThisMove));

        rankThisMove = 0;
    }

    std::sort(legalMoves.begin(), legalMoves.end());

    // Search all legal moves
    for (auto& lm : legalMoves)
    {
        extension = 0;

        // Extensions
        // Not more than one extension and not in the last iteration.
        if (   depth == 1
            && Limits.mate > 2
            && ss->ply < thisThread->rootDepth
            && thisThread->rootDepth < thisThread->targetDepth)
        {
            // Check extension.
            // Always extend up to the specified mate limit.
            if (lm.rank >= 6000)
                extension = thisThread->targetDepth - thisThread->rootDepth;

            // Extend knight moves by 2 plies if the opponent king is caged
            // by own pieces, or we do not have any major piece.
            else if (type_of(pos.moved_piece(lm.move)) == KNIGHT)
            {
                assert(pos.piece_on(from_sq(lm.move)) == make_piece(us, KNIGHT));
                
                const bool cagedKing = popcount(pos.attacks_from<KING>(pos.square<KING>(~us)) & ~pos.pieces(~us)) < 2;
                const int majorsCount = pos.count<QUEEN>(us) + pos.count<ROOK>(us);

                if (cagedKing || majorsCount == 0)
                    extension = 2;
            }
        }

        // At frontier nodes we can skip all non-checking moves.
        // Since checking moves are ranked first, simply break from the
        // moves loop as soon as we hit the first non-checking move.
        if (    depth == 1
            && !extension
            &&  lm.rank < 6000)
        {
            assert(!pos.gives_check(lm.move));

            continue;
        }

        // At interior nodes beyond the nominal search depth,
        // do the same for the root side-to-move, because this can only
        // mean we're in a check extension search. 
        if (   (ss->ply > thisThread->rootDepth
            || (thisThread->rootDepth < thisThread->fullDepth && ss->ply > thisThread->fullDepth))
            && !(ss->ply & 1)
            &&  lm.rank < 6000)
        {
            assert(!pos.gives_check(lm.move));

            break;
        }

        moveCount++;

        assert(is_ok(lm.move));
        
        pos.do_move(lm.move, st);
        thisThread->nodes++;
        
        value = -search(pos, ss+1, -beta, -alpha, depth-1+extension);
        
        pos.undo_move(lm.move);

        // Do we have a new best value?
        if (value > bestValue)
        {
            // Beta-cutoff?
            if (value >= beta)
                return value;

            bestValue = value;

            if (value > alpha)
            {
                // Update alpha
                alpha = value;

                // Reset PV and insert current best move
                ss->pv.clear();
                ss->pv.push_back(lm.move);

                // Append child pv
                for (auto& m : (ss+1)->pv)
                    ss->pv.push_back(m);
            }
        }

        // If we have found a mate within the specified limit,
        // we can immediately break from the moves loop.
        // Note: this can only happen for the root color!
        if (bestValue > VALUE_MATE - 2 * Limits.mate)
            break;
    }

    // No moves? Must be Mate or Stalemate!
    if (!moveCount)
        bestValue = inCheck ? mated_in(ss->ply) // Checkmate!
                            : VALUE_DRAW;

    assert(-VALUE_INFINITE <= bestValue && bestValue < VALUE_INFINITE);

    return bestValue;
  }


  // tb_sequence() tries to build a mating sequence if the
  // root position is a winning TB position. It repeatedly
  // calls itself until we hit a mate.

  Value syzygy_search(Position& pos, int ply) {

    StateInfo st;
    Move bestMove;
    Value bestValue;
    RootMoves legalMoves;

    bestMove = MOVE_NONE;
    bestValue = -VALUE_INFINITE;

    // No legal moves? Must be mate!
    if (!MoveList<LEGAL>(pos).size())
    {
        pos.this_thread()->rootMoves[0].pv.resize(ply);
        pos.this_thread()->rootMoves[0].selDepth = ply;

        return mated_in(ply);
    }

    // Insert legal moves
    for (const auto& m : MoveList<LEGAL>(pos))
        legalMoves.emplace_back(m);

    // Rank moves strictly by dtz and pick the best
    Tablebases::rank_root_moves(pos, legalMoves);
    pos.this_thread()->tbHits += legalMoves.size();

    bestMove = legalMoves[0].pv[0];

    pos.do_move(bestMove, st);
    pos.this_thread()->nodes++;
    bestValue = -syzygy_search(pos, ply+1);
    pos.undo_move(bestMove);

    pos.this_thread()->rootMoves[0].pv[ply] = bestMove;

    return bestValue;
  }

} // namespace


/// MainThread::check_time() is used to print debug info and, more importantly,
/// to detect when we are out of available time and thus stop the search.

void MainThread::check_time() {

  if (--callsCnt > 0)
      return;

  // When using nodes, ensure checking rate is not lower than 0.1% of nodes
  callsCnt = Limits.nodes ? std::min(1024, int(Limits.nodes / 1024)) : 1024;

  static TimePoint lastInfoTime = now();

  TimePoint elapsed = Time.elapsed();
  TimePoint tick = Limits.startTime + elapsed;

  if (tick - lastInfoTime >= 1000)
  {
      lastInfoTime = tick;
      dbg_print();
  }

  if (   (Limits.use_time_management() && elapsed > Time.maximum() - 10)
      || (Limits.movetime && elapsed >= Limits.movetime)
      || (Limits.nodes && Threads.nodes_searched() >= (uint64_t)Limits.nodes))
      Threads.stop = true;
}


/// UCI::pv() formats PV information according to the UCI protocol. UCI requires
/// that all (if any) unsearched PV lines are sent using a previous search score.

string UCI::pv(const Position& pos, Depth depth) {

  std::stringstream ss;
  TimePoint elapsed = Time.elapsed() + 1;
  const RootMoves& rootMoves = pos.this_thread()->rootMoves;
  uint64_t nodesSearched = Threads.nodes_searched();
  uint64_t tbHits = Threads.tb_hits();

      if (ss.rdbuf()->in_avail()) // Not at first line
          ss << "\n";

      ss << "info"
         << " depth "    << depth
         << " seldepth " << rootMoves[0].selDepth
         << " multipv "  << 1
         << " score "    << UCI::value(rootMoves[0].score);

      ss << " nodes "    << nodesSearched
         << " nps "      << nodesSearched * 1000 / elapsed;

      ss << " tbhits "   << tbHits
         << " time "     << elapsed
         << " pv";

      for (Move m : rootMoves[0].pv)
          ss << " " << UCI::move(m, pos.is_chess960());

  return ss.str();
}


void Tablebases::rank_root_moves(Position& pos, Search::RootMoves& rootMoves) {

    RootInTB = false;
    UseRule50 = Options["Syzygy50MoveRule"];
    ProbeDepth = Options["SyzygyProbeDepth"];
    Cardinality = Options["SyzygyProbeLimit"];

    // Tables with fewer pieces than SyzygyProbeLimit are searched with
    // ProbeDepth == DEPTH_ZERO
    if (Cardinality > MaxCardinality)
        Cardinality = MaxCardinality;

    if (rootMoves.size() == 0)
        return;

    if (    Cardinality >= pos.count<ALL_PIECES>()
        && !pos.can_castle(ANY_CASTLING))
    {
        // Rank moves using DTZ tables
        RootInTB = root_probe(pos, rootMoves);

        // DTZ tables are missing; try to rank moves using WDL tables
        if (!RootInTB)
            RootInTB = root_probe_wdl(pos, rootMoves);
    }

    // Clean up if both, root_probe() and root_probe_wdl() have failed!
    if (!RootInTB)
        for (auto& rm : rootMoves)
            rm.tbRank = 0;
}
