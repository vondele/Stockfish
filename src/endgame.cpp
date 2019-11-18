/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2019 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include <cassert>

#include "bitboard.h"
#include "endgame.h"
#include "movegen.h"

using std::string;

namespace {

  // Table used to drive the king towards the edge of the board
  // in KX vs K and KQ vs KR endgames.
  constexpr int PushToEdges[SQUARE_NB] = {
    100, 90, 80, 70, 70, 80, 90, 100,
     90, 70, 60, 50, 50, 60, 70,  90,
     80, 60, 40, 30, 30, 40, 60,  80,
     70, 50, 30, 20, 20, 30, 50,  70,
     70, 50, 30, 20, 20, 30, 50,  70,
     80, 60, 40, 30, 30, 40, 60,  80,
     90, 70, 60, 50, 50, 60, 70,  90,
    100, 90, 80, 70, 70, 80, 90, 100
  };

  // Table used to drive the king towards a corner square of the
  // right color in KBN vs K endgames.
  constexpr int PushToCorners[SQUARE_NB] = {
     6400, 6080, 5760, 5440, 5120, 4800, 4480, 4160,
     6080, 5760, 5440, 5120, 4800, 4480, 4160, 4480,
     5760, 5440, 4960, 4480, 4480, 4000, 4480, 4800,
     5440, 5120, 4480, 3840, 3520, 4480, 4800, 5120,
     5120, 4800, 4480, 3520, 3840, 4480, 5120, 5440,
     4800, 4480, 4000, 4480, 4480, 4960, 5440, 5760,
     4480, 4160, 4480, 4800, 5120, 5440, 5760, 6080,
     4160, 4480, 4800, 5120, 5440, 5760, 6080, 6400
  };

  // Tables used to drive a piece towards or away from another piece
  constexpr int PushClose[8] = { 0, 0, 100, 80, 60, 40, 20, 10 };
  constexpr int PushAway [8] = { 0, 5, 20, 40, 60, 80, 90, 100 };

  // Pawn Rank based scaling factors used in KRPPKRP endgame
  constexpr int KRPPKRPScaleFactors[RANK_NB] = { 0, 9, 10, 14, 21, 44, 0, 0 };

#ifndef NDEBUG
  bool verify_material(const Position& pos, Color c, Value npm, int pawnsCnt) {
    return pos.non_pawn_material(c) == npm && pos.count<PAWN>(c) == pawnsCnt;
  }
#endif

  // Map the square as if strongSide is white and strongSide's only pawn
  // is on the left half of the board.
  Square normalize(const Position& pos, Color strongSide, Square sq) {

    assert(pos.count<PAWN>(strongSide) == 1);

    if (file_of(pos.square<PAWN>(strongSide)) >= FILE_E)
        sq = Square(int(sq) ^ 7); // Mirror SQ_H1 -> SQ_A1

    return strongSide == WHITE ? sq : ~sq;
  }

} // namespace


namespace Endgames {

  std::pair<Map<Value>, Map<ScaleFactor>> maps;

  void init() {

    add<KPK>("KPK");
    add<KNNK>("KNNK");
    add<KBNK>("KBNK");
    add<KRKP>("KRKP");
    add<KRKB>("KRKB");
    add<KRKN>("KRKN");
    add<KQKP>("KQKP");
    add<KQKR>("KQKR");
    add<KNNKP>("KNNKP");

    add<KNPK>("KNPK");
    add<KNPKB>("KNPKB");
    add<KRPKR>("KRPKR");
    add<KRPKB>("KRPKB");
    add<KBPKB>("KBPKB");
    add<KBPKN>("KBPKN");
    add<KBPPKB>("KBPPKB");
    add<KRPPKRP>("KRPPKRP");


    for (int i = 0; i < TB_STAT_COUNT; ++i)
    {
        TBstats[i].init_ratios();

        StateInfo st;
        Key key = Position().set(TBstats[i].code, WHITE, &st).material_key();

        // Don't overwrite existing endgames like "KRPPKRP"
        if (!probe<ScaleFactor>(key) && !probe<Value>(key))
            add<KTKT>(TBstats[i].code, &TBstats[i]);
    }
  }
}


/// Mate with KX vs K. This function is used to evaluate positions with
/// king and plenty of material vs a lone king. It simply gives the
/// attacking side a bonus for driving the defending king towards the edge
/// of the board, and for keeping the distance between the two kings small.
template<>
Value Endgame<KXK>::operator()(const Position& pos) const {

  assert(verify_material(pos, weakSide, VALUE_ZERO, 0));
  assert(!pos.checkers()); // Eval is never called when in check

  // Stalemate detection with lone king
  if (pos.side_to_move() == weakSide && !MoveList<LEGAL>(pos).size())
      return VALUE_DRAW;

  Square winnerKSq = pos.square<KING>(strongSide);
  Square loserKSq = pos.square<KING>(weakSide);

  Value result =  pos.non_pawn_material(strongSide)
                + pos.count<PAWN>(strongSide) * PawnValueEg
                + PushToEdges[loserKSq]
                + PushClose[distance(winnerKSq, loserKSq)];

  if (   pos.count<QUEEN>(strongSide)
      || pos.count<ROOK>(strongSide)
      ||(pos.count<BISHOP>(strongSide) && pos.count<KNIGHT>(strongSide))
      || (   (pos.pieces(strongSide, BISHOP) & ~DarkSquares)
          && (pos.pieces(strongSide, BISHOP) &  DarkSquares)))
      result = std::min(result + VALUE_KNOWN_WIN, VALUE_MATE_IN_MAX_PLY - 1);

  return strongSide == pos.side_to_move() ? result : -result;
}


/// Mate with KBN vs K. This is similar to KX vs K, but we have to drive the
/// defending king towards a corner square that our bishop attacks.
template<>
Value Endgame<KBNK>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, KnightValueMg + BishopValueMg, 0));
  assert(verify_material(pos, weakSide, VALUE_ZERO, 0));

  Square winnerKSq = pos.square<KING>(strongSide);
  Square loserKSq = pos.square<KING>(weakSide);
  Square bishopSq = pos.square<BISHOP>(strongSide);

  // If our Bishop does not attack A1/H8, we flip the enemy king square
  // to drive to opposite corners (A8/H1).

  Value result =  VALUE_KNOWN_WIN
                + PushClose[distance(winnerKSq, loserKSq)]
                + PushToCorners[opposite_colors(bishopSq, SQ_A1) ? ~loserKSq : loserKSq];

  assert(abs(result) < VALUE_MATE_IN_MAX_PLY);
  return strongSide == pos.side_to_move() ? result : -result;
}


/// KP vs K. This endgame is evaluated with the help of a bitbase.
template<>
Value Endgame<KPK>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, VALUE_ZERO, 1));
  assert(verify_material(pos, weakSide, VALUE_ZERO, 0));

  // Assume strongSide is white and the pawn is on files A-D
  Square wksq = normalize(pos, strongSide, pos.square<KING>(strongSide));
  Square bksq = normalize(pos, strongSide, pos.square<KING>(weakSide));
  Square psq  = normalize(pos, strongSide, pos.square<PAWN>(strongSide));

  Color us = strongSide == pos.side_to_move() ? WHITE : BLACK;

  if (!Bitbases::probe(wksq, psq, bksq, us))
      return VALUE_DRAW;

  Value result = VALUE_KNOWN_WIN + PawnValueEg + Value(rank_of(psq));

  return strongSide == pos.side_to_move() ? result : -result;
}


/// KR vs KP. This is a somewhat tricky endgame to evaluate precisely without
/// a bitbase. The function below returns drawish scores when the pawn is
/// far advanced with support of the king, while the attacking king is far
/// away.
template<>
Value Endgame<KRKP>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, RookValueMg, 0));
  assert(verify_material(pos, weakSide, VALUE_ZERO, 1));

  Square wksq = relative_square(strongSide, pos.square<KING>(strongSide));
  Square bksq = relative_square(strongSide, pos.square<KING>(weakSide));
  Square rsq  = relative_square(strongSide, pos.square<ROOK>(strongSide));
  Square psq  = relative_square(strongSide, pos.square<PAWN>(weakSide));

  Square queeningSq = make_square(file_of(psq), RANK_1);
  Value result;

  // If the stronger side's king is in front of the pawn, it's a win
  if (forward_file_bb(WHITE, wksq) & psq)
      result = RookValueEg - distance(wksq, psq);

  // If the weaker side's king is too far from the pawn and the rook,
  // it's a win.
  else if (   distance(bksq, psq) >= 3 + (pos.side_to_move() == weakSide)
           && distance(bksq, rsq) >= 3)
      result = RookValueEg - distance(wksq, psq);

  // If the pawn is far advanced and supported by the defending king,
  // the position is drawish
  else if (   rank_of(bksq) <= RANK_3
           && distance(bksq, psq) == 1
           && rank_of(wksq) >= RANK_4
           && distance(wksq, psq) > 2 + (pos.side_to_move() == strongSide))
      result = Value(80) - 8 * distance(wksq, psq);

  else
      result =  Value(200) - 8 * (  distance(wksq, psq + SOUTH)
                                  - distance(bksq, psq + SOUTH)
                                  - distance(psq, queeningSq));

  return strongSide == pos.side_to_move() ? result : -result;
}


/// KR vs KB. This is very simple, and always returns drawish scores. The
/// score is slightly bigger when the defending king is close to the edge.
template<>
Value Endgame<KRKB>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, RookValueMg, 0));
  assert(verify_material(pos, weakSide, BishopValueMg, 0));

  Value result = Value(PushToEdges[pos.square<KING>(weakSide)]);
  return strongSide == pos.side_to_move() ? result : -result;
}


/// KR vs KN. The attacking side has slightly better winning chances than
/// in KR vs KB, particularly if the king and the knight are far apart.
template<>
Value Endgame<KRKN>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, RookValueMg, 0));
  assert(verify_material(pos, weakSide, KnightValueMg, 0));

  Square bksq = pos.square<KING>(weakSide);
  Square bnsq = pos.square<KNIGHT>(weakSide);
  Value result = Value(PushToEdges[bksq] + PushAway[distance(bksq, bnsq)]);
  return strongSide == pos.side_to_move() ? result : -result;
}


/// KQ vs KP. In general, this is a win for the stronger side, but there are a
/// few important exceptions. A pawn on 7th rank and on the A,C,F or H files
/// with a king positioned next to it can be a draw, so in that case, we only
/// use the distance between the kings.
template<>
Value Endgame<KQKP>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, QueenValueMg, 0));
  assert(verify_material(pos, weakSide, VALUE_ZERO, 1));

  Square winnerKSq = pos.square<KING>(strongSide);
  Square loserKSq = pos.square<KING>(weakSide);
  Square pawnSq = pos.square<PAWN>(weakSide);

  Value result = Value(PushClose[distance(winnerKSq, loserKSq)]);

  if (   relative_rank(weakSide, pawnSq) != RANK_7
      || distance(loserKSq, pawnSq) != 1
      || !((FileABB | FileCBB | FileFBB | FileHBB) & pawnSq))
      result += QueenValueEg - PawnValueEg;

  return strongSide == pos.side_to_move() ? result : -result;
}


/// KQ vs KR.  This is almost identical to KX vs K:  We give the attacking
/// king a bonus for having the kings close together, and for forcing the
/// defending king towards the edge. If we also take care to avoid null move for
/// the defending side in the search, this is usually sufficient to win KQ vs KR.
template<>
Value Endgame<KQKR>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, QueenValueMg, 0));
  assert(verify_material(pos, weakSide, RookValueMg, 0));

  Square winnerKSq = pos.square<KING>(strongSide);
  Square loserKSq = pos.square<KING>(weakSide);

  Value result =  QueenValueEg
                - RookValueEg
                + PushToEdges[loserKSq]
                + PushClose[distance(winnerKSq, loserKSq)];

  return strongSide == pos.side_to_move() ? result : -result;
}


/// KNN vs KP. Simply push the opposing king to the corner
template<>
Value Endgame<KNNKP>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, 2 * KnightValueMg, 0));
  assert(verify_material(pos, weakSide, VALUE_ZERO, 1));

  Value result =  2 * KnightValueEg
                - PawnValueEg
                + PushToEdges[pos.square<KING>(weakSide)];

  return strongSide == pos.side_to_move() ? result : -result;
}


/// Some cases of trivial draws
template<> Value Endgame<KNNK>::operator()(const Position&) const { return VALUE_DRAW; }


/// KB and one or more pawns vs K. It checks for draws with rook pawns and
/// a bishop of the wrong color. If such a draw is detected, SCALE_FACTOR_DRAW
/// is returned. If not, the return value is SCALE_FACTOR_NONE, i.e. no scaling
/// will be used.
template<>
ScaleFactor Endgame<KBPsK>::operator()(const Position& pos) const {

  assert(pos.non_pawn_material(strongSide) == BishopValueMg);
  assert(pos.count<PAWN>(strongSide) >= 1);

  // No assertions about the material of weakSide, because we want draws to
  // be detected even when the weaker side has some pawns.

  Bitboard pawns = pos.pieces(strongSide, PAWN);
  File pawnsFile = file_of(lsb(pawns));

  // All pawns are on a single rook file?
  if (    (pawnsFile == FILE_A || pawnsFile == FILE_H)
      && !(pawns & ~file_bb(pawnsFile)))
  {
      Square bishopSq = pos.square<BISHOP>(strongSide);
      Square queeningSq = relative_square(strongSide, make_square(pawnsFile, RANK_8));
      Square kingSq = pos.square<KING>(weakSide);

      if (   opposite_colors(queeningSq, bishopSq)
          && distance(queeningSq, kingSq) <= 1)
          return SCALE_FACTOR_DRAW;
  }

  // If all the pawns are on the same B or G file, then it's potentially a draw
  if (    (pawnsFile == FILE_B || pawnsFile == FILE_G)
      && !(pos.pieces(PAWN) & ~file_bb(pawnsFile))
      && pos.non_pawn_material(weakSide) == 0
      && pos.count<PAWN>(weakSide) >= 1)
  {
      // Get weakSide pawn that is closest to the home rank
      Square weakPawnSq = frontmost_sq(strongSide, pos.pieces(weakSide, PAWN));

      Square strongKingSq = pos.square<KING>(strongSide);
      Square weakKingSq = pos.square<KING>(weakSide);
      Square bishopSq = pos.square<BISHOP>(strongSide);

      // There's potential for a draw if our pawn is blocked on the 7th rank,
      // the bishop cannot attack it or they only have one pawn left
      if (   relative_rank(strongSide, weakPawnSq) == RANK_7
          && (pos.pieces(strongSide, PAWN) & (weakPawnSq + pawn_push(weakSide)))
          && (opposite_colors(bishopSq, weakPawnSq) || pos.count<PAWN>(strongSide) == 1))
      {
          int strongKingDist = distance(weakPawnSq, strongKingSq);
          int weakKingDist = distance(weakPawnSq, weakKingSq);

          // It's a draw if the weak king is on its back two ranks, within 2
          // squares of the blocking pawn and the strong king is not
          // closer. (I think this rule only fails in practically
          // unreachable positions such as 5k1K/6p1/6P1/8/8/3B4/8/8 w
          // and positions where qsearch will immediately correct the
          // problem such as 8/4k1p1/6P1/1K6/3B4/8/8/8 w)
          if (   relative_rank(strongSide, weakKingSq) >= RANK_7
              && weakKingDist <= 2
              && weakKingDist <= strongKingDist)
              return SCALE_FACTOR_DRAW;
      }
  }

  return SCALE_FACTOR_NONE;
}


/// KQ vs KR and one or more pawns. It tests for fortress draws with a rook on
/// the third rank defended by a pawn.
template<>
ScaleFactor Endgame<KQKRPs>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, QueenValueMg, 0));
  assert(pos.count<ROOK>(weakSide) == 1);
  assert(pos.count<PAWN>(weakSide) >= 1);

  Square kingSq = pos.square<KING>(weakSide);
  Square rsq = pos.square<ROOK>(weakSide);

  if (    relative_rank(weakSide, kingSq) <= RANK_2
      &&  relative_rank(weakSide, pos.square<KING>(strongSide)) >= RANK_4
      &&  relative_rank(weakSide, rsq) == RANK_3
      && (  pos.pieces(weakSide, PAWN)
          & pos.attacks_from<KING>(kingSq)
          & pos.attacks_from<PAWN>(rsq, strongSide)))
          return SCALE_FACTOR_DRAW;

  return SCALE_FACTOR_NONE;
}


/// KRP vs KR. This function knows a handful of the most important classes of
/// drawn positions, but is far from perfect. It would probably be a good idea
/// to add more knowledge in the future.
///
/// It would also be nice to rewrite the actual code for this function,
/// which is mostly copied from Glaurung 1.x, and isn't very pretty.
template<>
ScaleFactor Endgame<KRPKR>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, RookValueMg, 1));
  assert(verify_material(pos, weakSide,   RookValueMg, 0));

  // Assume strongSide is white and the pawn is on files A-D
  Square wksq = normalize(pos, strongSide, pos.square<KING>(strongSide));
  Square bksq = normalize(pos, strongSide, pos.square<KING>(weakSide));
  Square wrsq = normalize(pos, strongSide, pos.square<ROOK>(strongSide));
  Square wpsq = normalize(pos, strongSide, pos.square<PAWN>(strongSide));
  Square brsq = normalize(pos, strongSide, pos.square<ROOK>(weakSide));

  File f = file_of(wpsq);
  Rank r = rank_of(wpsq);
  Square queeningSq = make_square(f, RANK_8);
  int tempo = (pos.side_to_move() == strongSide);

  // If the pawn is not too far advanced and the defending king defends the
  // queening square, use the third-rank defence.
  if (   r <= RANK_5
      && distance(bksq, queeningSq) <= 1
      && wksq <= SQ_H5
      && (rank_of(brsq) == RANK_6 || (r <= RANK_3 && rank_of(wrsq) != RANK_6)))
      return SCALE_FACTOR_DRAW;

  // The defending side saves a draw by checking from behind in case the pawn
  // has advanced to the 6th rank with the king behind.
  if (   r == RANK_6
      && distance(bksq, queeningSq) <= 1
      && rank_of(wksq) + tempo <= RANK_6
      && (rank_of(brsq) == RANK_1 || (!tempo && distance<File>(brsq, wpsq) >= 3)))
      return SCALE_FACTOR_DRAW;

  if (   r >= RANK_6
      && bksq == queeningSq
      && rank_of(brsq) == RANK_1
      && (!tempo || distance(wksq, wpsq) >= 2))
      return SCALE_FACTOR_DRAW;

  // White pawn on a7 and rook on a8 is a draw if black's king is on g7 or h7
  // and the black rook is behind the pawn.
  if (   wpsq == SQ_A7
      && wrsq == SQ_A8
      && (bksq == SQ_H7 || bksq == SQ_G7)
      && file_of(brsq) == FILE_A
      && (rank_of(brsq) <= RANK_3 || file_of(wksq) >= FILE_D || rank_of(wksq) <= RANK_5))
      return SCALE_FACTOR_DRAW;

  // If the defending king blocks the pawn and the attacking king is too far
  // away, it's a draw.
  if (   r <= RANK_5
      && bksq == wpsq + NORTH
      && distance(wksq, wpsq) - tempo >= 2
      && distance(wksq, brsq) - tempo >= 2)
      return SCALE_FACTOR_DRAW;

  // Pawn on the 7th rank supported by the rook from behind usually wins if the
  // attacking king is closer to the queening square than the defending king,
  // and the defending king cannot gain tempi by threatening the attacking rook.
  if (   r == RANK_7
      && f != FILE_A
      && file_of(wrsq) == f
      && wrsq != queeningSq
      && (distance(wksq, queeningSq) < distance(bksq, queeningSq) - 2 + tempo)
      && (distance(wksq, queeningSq) < distance(bksq, wrsq) + tempo))
      return ScaleFactor(SCALE_FACTOR_MAX - 2 * distance(wksq, queeningSq));

  // Similar to the above, but with the pawn further back
  if (   f != FILE_A
      && file_of(wrsq) == f
      && wrsq < wpsq
      && (distance(wksq, queeningSq) < distance(bksq, queeningSq) - 2 + tempo)
      && (distance(wksq, wpsq + NORTH) < distance(bksq, wpsq + NORTH) - 2 + tempo)
      && (  distance(bksq, wrsq) + tempo >= 3
          || (    distance(wksq, queeningSq) < distance(bksq, wrsq) + tempo
              && (distance(wksq, wpsq + NORTH) < distance(bksq, wrsq) + tempo))))
      return ScaleFactor(  SCALE_FACTOR_MAX
                         - 8 * distance(wpsq, queeningSq)
                         - 2 * distance(wksq, queeningSq));

  // If the pawn is not far advanced and the defending king is somewhere in
  // the pawn's path, it's probably a draw.
  if (r <= RANK_4 && bksq > wpsq)
  {
      if (file_of(bksq) == file_of(wpsq))
          return ScaleFactor(10);
      if (   distance<File>(bksq, wpsq) == 1
          && distance(wksq, bksq) > 2)
          return ScaleFactor(24 - 2 * distance(wksq, bksq));
  }
  return SCALE_FACTOR_NONE;
}

template<>
ScaleFactor Endgame<KRPKB>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, RookValueMg, 1));
  assert(verify_material(pos, weakSide, BishopValueMg, 0));

  // Test for a rook pawn
  if (pos.pieces(PAWN) & (FileABB | FileHBB))
  {
      Square ksq = pos.square<KING>(weakSide);
      Square bsq = pos.square<BISHOP>(weakSide);
      Square psq = pos.square<PAWN>(strongSide);
      Rank rk = relative_rank(strongSide, psq);
      Direction push = pawn_push(strongSide);

      // If the pawn is on the 5th rank and the pawn (currently) is on
      // the same color square as the bishop then there is a chance of
      // a fortress. Depending on the king position give a moderate
      // reduction or a stronger one if the defending king is near the
      // corner but not trapped there.
      if (rk == RANK_5 && !opposite_colors(bsq, psq))
      {
          int d = distance(psq + 3 * push, ksq);

          if (d <= 2 && !(d == 0 && ksq == pos.square<KING>(strongSide) + 2 * push))
              return ScaleFactor(24);
          else
              return ScaleFactor(48);
      }

      // When the pawn has moved to the 6th rank we can be fairly sure
      // it's drawn if the bishop attacks the square in front of the
      // pawn from a reasonable distance and the defending king is near
      // the corner
      if (   rk == RANK_6
          && distance(psq + 2 * push, ksq) <= 1
          && (PseudoAttacks[BISHOP][bsq] & (psq + push))
          && distance<File>(bsq, psq) >= 2)
          return ScaleFactor(8);
  }

  return SCALE_FACTOR_NONE;
}

/// KRPP vs KRP. There is just a single rule: if the stronger side has no passed
/// pawns and the defending king is actively placed, the position is drawish.
template<>
ScaleFactor Endgame<KRPPKRP>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, RookValueMg, 2));
  assert(verify_material(pos, weakSide,   RookValueMg, 1));

  Square wpsq1 = pos.squares<PAWN>(strongSide)[0];
  Square wpsq2 = pos.squares<PAWN>(strongSide)[1];
  Square bksq = pos.square<KING>(weakSide);

  // Does the stronger side have a passed pawn?
  if (pos.pawn_passed(strongSide, wpsq1) || pos.pawn_passed(strongSide, wpsq2))
      return SCALE_FACTOR_NONE;

  Rank r = std::max(relative_rank(strongSide, wpsq1), relative_rank(strongSide, wpsq2));

  if (   distance<File>(bksq, wpsq1) <= 1
      && distance<File>(bksq, wpsq2) <= 1
      && relative_rank(strongSide, bksq) > r)
  {
      assert(r > RANK_1 && r < RANK_7);
      return ScaleFactor(KRPPKRPScaleFactors[r]);
  }
  return SCALE_FACTOR_NONE;
}


/// K and two or more pawns vs K. There is just a single rule here: If all pawns
/// are on the same rook file and are blocked by the defending king, it's a draw.
template<>
ScaleFactor Endgame<KPsK>::operator()(const Position& pos) const {

  assert(pos.non_pawn_material(strongSide) == VALUE_ZERO);
  assert(pos.count<PAWN>(strongSide) >= 2);
  assert(verify_material(pos, weakSide, VALUE_ZERO, 0));

  Square ksq = pos.square<KING>(weakSide);
  Bitboard pawns = pos.pieces(strongSide, PAWN);

  // If all pawns are ahead of the king, on a single rook file and
  // the king is within one file of the pawns, it's a draw.
  if (   !(pawns & ~forward_ranks_bb(weakSide, ksq))
      && !((pawns & ~FileABB) && (pawns & ~FileHBB))
      &&  distance<File>(ksq, lsb(pawns)) <= 1)
      return SCALE_FACTOR_DRAW;

  return SCALE_FACTOR_NONE;
}


/// KBP vs KB. There are two rules: if the defending king is somewhere along the
/// path of the pawn, and the square of the king is not of the same color as the
/// stronger side's bishop, it's a draw. If the two bishops have opposite color,
/// it's almost always a draw.
template<>
ScaleFactor Endgame<KBPKB>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, BishopValueMg, 1));
  assert(verify_material(pos, weakSide,   BishopValueMg, 0));

  Square pawnSq = pos.square<PAWN>(strongSide);
  Square strongBishopSq = pos.square<BISHOP>(strongSide);
  Square weakBishopSq = pos.square<BISHOP>(weakSide);
  Square weakKingSq = pos.square<KING>(weakSide);

  // Case 1: Defending king blocks the pawn, and cannot be driven away
  if (   file_of(weakKingSq) == file_of(pawnSq)
      && relative_rank(strongSide, pawnSq) < relative_rank(strongSide, weakKingSq)
      && (   opposite_colors(weakKingSq, strongBishopSq)
          || relative_rank(strongSide, weakKingSq) <= RANK_6))
      return SCALE_FACTOR_DRAW;

  // Case 2: Opposite colored bishops
  if (opposite_colors(strongBishopSq, weakBishopSq))
      return SCALE_FACTOR_DRAW;

  return SCALE_FACTOR_NONE;
}


/// KBPP vs KB. It detects a few basic draws with opposite-colored bishops
template<>
ScaleFactor Endgame<KBPPKB>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, BishopValueMg, 2));
  assert(verify_material(pos, weakSide,   BishopValueMg, 0));

  Square wbsq = pos.square<BISHOP>(strongSide);
  Square bbsq = pos.square<BISHOP>(weakSide);

  if (!opposite_colors(wbsq, bbsq))
      return SCALE_FACTOR_NONE;

  Square ksq = pos.square<KING>(weakSide);
  Square psq1 = pos.squares<PAWN>(strongSide)[0];
  Square psq2 = pos.squares<PAWN>(strongSide)[1];
  Square blockSq1, blockSq2;

  if (relative_rank(strongSide, psq1) > relative_rank(strongSide, psq2))
  {
      blockSq1 = psq1 + pawn_push(strongSide);
      blockSq2 = make_square(file_of(psq2), rank_of(psq1));
  }
  else
  {
      blockSq1 = psq2 + pawn_push(strongSide);
      blockSq2 = make_square(file_of(psq1), rank_of(psq2));
  }

  switch (distance<File>(psq1, psq2))
  {
  case 0:
    // Both pawns are on the same file. It's an easy draw if the defender firmly
    // controls some square in the frontmost pawn's path.
    if (   file_of(ksq) == file_of(blockSq1)
        && relative_rank(strongSide, ksq) >= relative_rank(strongSide, blockSq1)
        && opposite_colors(ksq, wbsq))
        return SCALE_FACTOR_DRAW;
    else
        return SCALE_FACTOR_NONE;

  case 1:
    // Pawns on adjacent files. It's a draw if the defender firmly controls the
    // square in front of the frontmost pawn's path, and the square diagonally
    // behind this square on the file of the other pawn.
    if (   ksq == blockSq1
        && opposite_colors(ksq, wbsq)
        && (   bbsq == blockSq2
            || (pos.attacks_from<BISHOP>(blockSq2) & pos.pieces(weakSide, BISHOP))
            || distance<Rank>(psq1, psq2) >= 2))
        return SCALE_FACTOR_DRAW;

    else if (   ksq == blockSq2
             && opposite_colors(ksq, wbsq)
             && (   bbsq == blockSq1
                 || (pos.attacks_from<BISHOP>(blockSq1) & pos.pieces(weakSide, BISHOP))))
        return SCALE_FACTOR_DRAW;
    else
        return SCALE_FACTOR_NONE;

  default:
    // The pawns are not on the same file or adjacent files. No scaling.
    return SCALE_FACTOR_NONE;
  }
}


/// KBP vs KN. There is a single rule: If the defending king is somewhere along
/// the path of the pawn, and the square of the king is not of the same color as
/// the stronger side's bishop, it's a draw.
template<>
ScaleFactor Endgame<KBPKN>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, BishopValueMg, 1));
  assert(verify_material(pos, weakSide, KnightValueMg, 0));

  Square pawnSq = pos.square<PAWN>(strongSide);
  Square strongBishopSq = pos.square<BISHOP>(strongSide);
  Square weakKingSq = pos.square<KING>(weakSide);

  if (   file_of(weakKingSq) == file_of(pawnSq)
      && relative_rank(strongSide, pawnSq) < relative_rank(strongSide, weakKingSq)
      && (   opposite_colors(weakKingSq, strongBishopSq)
          || relative_rank(strongSide, weakKingSq) <= RANK_6))
      return SCALE_FACTOR_DRAW;

  return SCALE_FACTOR_NONE;
}


/// KNP vs K. There is a single rule: if the pawn is a rook pawn on the 7th rank
/// and the defending king prevents the pawn from advancing, the position is drawn.
template<>
ScaleFactor Endgame<KNPK>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, KnightValueMg, 1));
  assert(verify_material(pos, weakSide, VALUE_ZERO, 0));

  // Assume strongSide is white and the pawn is on files A-D
  Square pawnSq     = normalize(pos, strongSide, pos.square<PAWN>(strongSide));
  Square weakKingSq = normalize(pos, strongSide, pos.square<KING>(weakSide));

  if (pawnSq == SQ_A7 && distance(SQ_A8, weakKingSq) <= 1)
      return SCALE_FACTOR_DRAW;

  return SCALE_FACTOR_NONE;
}


/// KNP vs KB. If knight can block bishop from taking pawn, it's a win.
/// Otherwise the position is drawn.
template<>
ScaleFactor Endgame<KNPKB>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, KnightValueMg, 1));
  assert(verify_material(pos, weakSide, BishopValueMg, 0));

  Square pawnSq = pos.square<PAWN>(strongSide);
  Square bishopSq = pos.square<BISHOP>(weakSide);
  Square weakKingSq = pos.square<KING>(weakSide);

  // King needs to get close to promoting pawn to prevent knight from blocking.
  // Rules for this are very tricky, so just approximate.
  if (forward_file_bb(strongSide, pawnSq) & pos.attacks_from<BISHOP>(bishopSq))
      return ScaleFactor(distance(weakKingSq, pawnSq));

  return SCALE_FACTOR_NONE;
}


/// KP vs KP. This is done by removing the weakest side's pawn and probing the
/// KP vs K bitbase: If the weakest side has a draw without the pawn, it probably
/// has at least a draw with the pawn as well. The exception is when the stronger
/// side's pawn is far advanced and not on a rook file; in this case it is often
/// possible to win (e.g. 8/4k3/3p4/3P4/6K1/8/8/8 w - - 0 1).
template<>
ScaleFactor Endgame<KPKP>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, VALUE_ZERO, 1));
  assert(verify_material(pos, weakSide,   VALUE_ZERO, 1));

  // Assume strongSide is white and the pawn is on files A-D
  Square wksq = normalize(pos, strongSide, pos.square<KING>(strongSide));
  Square bksq = normalize(pos, strongSide, pos.square<KING>(weakSide));
  Square psq  = normalize(pos, strongSide, pos.square<PAWN>(strongSide));

  Color us = strongSide == pos.side_to_move() ? WHITE : BLACK;

  // If the pawn has advanced to the fifth rank or further, and is not a
  // rook pawn, it's too dangerous to assume that it's at least a draw.
  if (rank_of(psq) >= RANK_5 && file_of(psq) != FILE_A)
      return SCALE_FACTOR_NONE;

  // Probe the KPK bitbase with the weakest side's pawn removed. If it's a draw,
  // it's probably at least a draw even with the pawn.
  return Bitbases::probe(wksq, psq, bksq, us) ? SCALE_FACTOR_NONE : SCALE_FACTOR_DRAW;
}

template<>
ScaleFactor Endgame<KTKT>::operator()(const Position& pos) const {
    
  assert(pos.count<ALL_PIECES>() <= 7);

  //TODO: make use of endgame statistics here

  return SCALE_FACTOR_NORMAL; //No functional change
}

#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif
TBStat TBstats[TB_STAT_COUNT] = {
  { "KBBBKBP", 497661764016, 1106412, 1007049196344, 2083717842, 81403945926,  311535747570, 398178, 371308344366, 468284076, 9740126214,  71663819712, 1615433766, 635740851978, 708234, 186126016446 },
  { "KBBBKNP", 985414012032, 29711857560, 535164150060, 2361277356, 79684140396,  494514459294, 2913682314, 187818147654, 462579372, 7344031770,  72340108626, 1898697984, 347346002406, 26798175246, 490899552738 },
  { "KBBBKPP", 779314637400, 3881304, 396305205960, 5279492112, 80736170472,  373781905572, 507444, 139496026032, 477332196, 5992236564,  74743933908, 4802159916, 256809179928, 3373860, 405532731828 },
  { "KBBBKQP", 213823734048, 646752, 331425021330, 29484341922, 818925342696,  188352225864, 458550, 240363639714, 19199382786, 245137193490,  573788149206, 10284959136, 91061381616, 188202, 25471508184 },
  { "KBBBKRP", 660066939852, 313008816, 758122149078, 2129412000, 100683148146,  424633386300, 153909150, 253988606394, 685605714, 13591392846,  87091755300, 1443806286, 504133542684, 159099666, 235433553552 },
  { "KBBNKBP", 834596490526, 60080328, 694685523468, 935416676, 103717570306,  519391230156, 24605996, 204798437718, 331406852, 14302570446,  89414999860, 604009824, 489887085750, 35474332, 315205260370 },
  { "KBBNKNP", 1225138372858, 10072775814, 339753188996, 1022078438, 102144372062,  680075206072, 1480476102, 45190908226, 301935964, 11799724804,  90344647258, 720142474, 294562280770, 8592299712, 545063166786 },
  { "KBBNKPP", 1060606508664, 2460680, 128261860724, 2901224956, 103387171488,  521558350276, 531400, 24278419556, 435578612, 6994967228,  96392204260, 2465646344, 103983441168, 1929280, 539048158388 },
  { "KBBNKQP", 271663130442, 182784, 226542426056, 12480038406, 928768659824,  243407090964, 156330, 168019188504, 9438665984, 317983149386,  610785510438, 3041372422, 58523237552, 26454, 28256039478 },
  { "KBBNKRP", 560699908084, 86651544, 855742358776, 1181583246, 149399507006,  402294828638, 46838330, 313257441744, 552513918, 22696628538,  126702878468, 629069328, 542484917032, 39813214, 158405079446 },
  { "KBBPKBB", 336670955672, 125810352, 1240475359452, 7216, 3275520124,  266268247032, 17570844, 511595757804, 64, 345208456,  2930311668, 7152, 728879601648, 108239508, 70402708640 },
  { "KBBPKBN", 510184214974, 22454373384, 1091267045104, 2858, 1830618396,  373171115624, 11132851280, 393821975232, 8, 100842056,  1729776340, 2850, 697445069872, 11321522104, 137013099350 },
  { "KBBPKBP", 620360353498, 26601886, 533352060882, 641487314, 97284130738,  392086741402, 13220084, 172951687240, 292308668, 17263048886,  80021081852, 349178646, 360400373642, 13381802, 228273612096 },
  { "KBBPKNN", 746664287292, 77777028776, 839820753044, 69469028, 276688108,  489144478680, 31990255180, 257063486708, 159708, 28403924,  248284184, 69309320, 582757266336, 45786773596, 257519808612 },
  { "KBBPKNP", 834873770582, 11177070446, 343685047502, 728313156, 93455690550,  474964590064, 2433820330, 90386926900, 271565560, 14550103426,  78905587124, 456747596, 253298120602, 8743250116, 359909180518 },
  { "KBBPKPP", 762298491484, 7454372, 95433894924, 1502344920, 125282176528,  393706325220, 2745312, 21171224508, 455150984, 18467813148,  106814363380, 1047193936, 74262670416, 4709060, 368592166264 },
  { "KBBPKQB", 121040856902, 17332004, 198361763084, 2803034158, 1084434557444,  115454940966, 16131206, 168875196072, 2582344010, 491298171946,  593136385498, 220690148, 29486567012, 1200798, 5585915936 },
  { "KBBPKQN", 195766709658, 6760650592, 145276593640, 3046461498, 1091743510452,  185353700310, 5545703174, 114568339154, 2682690830, 470076350732,  621667159720, 363770668, 30708254486, 1214947418, 10413009348 },
  { "KBBPKQP", 191605395564, 12840508, 90665922516, 4843420106, 819989528644,  177041464008, 11508262, 63639497610, 3990073066, 337924463334,  482065065310, 853347040, 27026424906, 1332246, 14563931556 },
  { "KBBPKQQ", 7603760004, 5015796, 35692193004, 6584794988, 1209819733280,  7419297356, 4788988, 34453968204, 6459977008, 729888752644,  479930980636, 124817980, 1238224800, 226808, 184462648 },
  { "KBBPKQR", 61381125930, 46850054, 210727827614, 2900230718, 1077519965460,  59469280766, 45341792, 191222633938, 2753818612, 524735709092,  552784256368, 146412106, 19505193676, 1508262, 1911845164 },
  { "KBBPKRB", 235207041872, 42356752, 949874305894, 427616604, 340591658766,  201418189690, 39231398, 534879138708, 103244398, 41786980006,  298804678760, 324372206, 414995167186, 3125354, 33788852182 },
  { "KBBPKRN", 345303385910, 21131729010, 783340728982, 114121007924, 297067498618,  283722555074, 11441150666, 414152510336, 37343447720, 31567120404,  265500378214, 76777560204, 369188218646, 9690578344, 61580830836 },
  { "KBBPKRP", 393268592462, 149663564, 584815343680, 881618316, 222333055358,  292643780800, 91383288, 255397895636, 508355914, 33965590642,  188367464716, 373262402, 329417448044, 58280276, 100624811662 },
  { "KBBPKRR", 147243008984, 109263188, 468157845348, 380399972, 840474218020,  132648267860, 90372428, 357284556872, 249150528, 287954436512,  552519781508, 131249444, 110873288476, 18890760, 14594741124 },
  { "KBNNKBP", 1023162174492, 4535919704, 515022641084, 5528754, 131400727782,  648755612380, 1109665166, 107526345462, 2084344, 21586454328,  109814273454, 3444410, 407496295622, 3426254538, 374406562112 },
  { "KBNNKNP", 1144869747470, 3663024908, 441271261644, 46075234, 128412589424,  681155269576, 912739710, 78079141736, 16500874, 18816509784,  109596079640, 29574360, 363192119908, 2750285198, 463714477894 },
  { "KBNNKPP", 1020532190576, 8934995524, 169532423784, 19416732, 125486095136,  530701533720, 868596832, 39250589660, 4874420, 11788147680,  113697947456, 14542312, 130281834124, 8066398692, 489830656856 },
  { "KBNNKQP", 245040526536, 2923217840, 248306497232, 56400572, 983259705844,  225572401726, 2183888040, 190659451468, 40843698, 360523576748,  622736129096, 15556874, 57647045764, 739329800, 19468124810 },
  { "KBNNKRP", 272520092986, 3430798022, 1128641837288, 76758862, 202572432010,  237998630486, 2409664134, 501408727224, 17496150, 37145643686,  165426788324, 59262712, 627233110064, 1021133888, 34521462500 },
  { "KBNPKBB", 403112436450, 196447940, 1156668547098, 20621054086, 45077927122,  335938777122, 60176804, 482768608656, 2012889430, 2575092068,  42502835054, 18608164656, 673899938442, 136271136, 67173659328 },
  { "KBNPKBN", 510284873661, 126442947, 1156668961079, 19191685, 3765545224,  397341582582, 82583364, 425810345244, 175545, 120857345,  3644687879, 19016140, 730858615835, 43859583, 112943291079 },
  { "KBNPKBP", 789205606137, 636059321, 364640420686, 154072808, 130040521172,  501531305890, 284968143, 87017800375, 107139566, 26677838112,  103362683060, 46933242, 277622620311, 351091178, 287674300247 },
  { "KBNPKNN", 649224060434, 264470958, 1059823974962, 82112296, 342367478,  464831306384, 155112540, 358333064138, 505446, 35555572,  306811906, 81606850, 701490910824, 109358418, 184392754050 },
  { "KBNPKNP", 873342982206, 612706238, 320711699920, 147938994, 122116610684,  520461336593, 238368872, 71804625003, 86812213, 23027909405,  99088701279, 61126781, 248907074917, 374337366, 352881645613 },
  { "KBNPKPP", 787958778388, 1085558, 59294223478, 404357934, 160858974580,  414657661292, 488842, 13089263514, 210646410, 29838256824,  131020717756, 193711524, 46204959964, 596716, 373301117096 },
  { "KBNPKQB", 153663665236, 671710460, 150107608596, 154056540, 1147189262640,  148710006925, 638956092, 128515121134, 149850793, 545341609136,  601847653504, 4205747, 21592487462, 32754368, 4953658311 },
  { "KBNPKQN", 192220612352, 959510294, 138983035815, 154375739, 1155405151520,  184635696551, 904025017, 113778949550, 147798198, 523889074764,  631516076756, 6577541, 25204086265, 55485277, 7584915801 },
  { "KBNPKQP", 190964325316, 1937008, 69114167182, 505282843, 879543440795,  178943249050, 1793986, 50126384304, 468835802, 386078788944,  493464651851, 36447041, 18987782878, 143022, 12021076266 },
  { "KBNPKQQ", 6570094480, 25372, 29257248718, 334509920, 1268672378462,  6482383146, 23058, 28477252998, 332755718, 788063129160,  480609249302, 1754202, 779995720, 2314, 87711334 },
  { "KBNPKQR", 65747565558, 1207834, 188187698446, 157157644, 1143611130174,  64399880592, 1181314, 174053546747, 153360969, 584747574458,  558863555716, 3796675, 14134151699, 26520, 1347684966 },
  { "KBNPKRB", 269203757716, 1149646, 734954028627, 98714380136, 468398423643,  240643085402, 1086101, 453955188039, 42432585132, 86323599406,  382074824237, 56281795004, 280998840588, 63545, 28560672314 },
  { "KBNPKRN", 325451593426, 78183992, 911967495623, 10522895977, 358072941306,  282827284642, 64605043, 490560888424, 2912093822, 46990672149,  311082269157, 7610802155, 421406607199, 13578949, 42624308784 },
  { "KBNPKRP", 401090149390, 182412845, 539430413686, 259017158, 293498326107,  308291078206, 111530581, 246629630973, 181034609, 60405777717,  233092548390, 77982549, 292800782713, 70882264, 92799071184 },
  { "KBNPKRR", 152713401652, 954738288, 405506877480, 1651615512, 940666862460,  142980986134, 612841184, 323469816288, 1236670870, 355055229604,  585611632856, 414944642, 82037061192, 341897104, 9732415518 },
  { "KBPPKBB", 340640924836, 326511744, 848468261920, 3073672, 60715963808,  266478582224, 77055504, 378231068076, 236916, 4777369028,  55938594780, 2836756, 470237193844, 249456240, 74162342612 },
  { "KBPPKBN", 447618586110, 227768, 753285457840, 149786, 82296241448,  327559843604, 125852, 318477069388, 4954, 3527267950,  78768973498, 144832, 434808388452, 101916, 120058742506 },
  { "KBPPKBP", 472419898284, 4023476, 366545430094, 71118, 141276871682,  314444723186, 2000468, 139021579146, 39854, 29427288914,  111849582768, 31264, 227523850948, 2023008, 157975175098 },
  { "KBPPKNN", 592655333872, 4482620, 698047344324, 8830920732, 12058237416,  398060808304, 1199264, 250949233248, 219674232, 333396700,  11724840716, 8611246500, 447098111076, 3283356, 194594525568 },
  { "KBPPKNP", 611471771010, 5442326, 268080467054, 65278, 124108055872,  372029859542, 2317936, 85712459166, 29560, 25150965364,  98957090508, 35718, 182368007888, 3124390, 239441911468 },
  { "KBPPKPP", 565217075324, 5412412, 53080680608, 2430084, 145142874548,  310785763768, 1895028, 13190545304, 569584, 32984622324,  112158252224, 1860500, 39890135304, 3517384, 254431311556 },
  { "KBPPKQB", 93642228428, 4033562, 129755068570, 126438, 897326054194,  91794157710, 3868196, 117404829184, 110438, 440361346220,  456964707974, 16000, 12350239386, 165366, 1848070718 },
  { "KBPPKQN", 132797562596, 4510138, 115472716800, 127916, 898773911138,  129787841640, 4215320, 99033127484, 102484, 420739024820,  478034886318, 25432, 16439589316, 294818, 3009720956 },
  { "KBPPKQP", 129176960496, 6655544, 57752914952, 3462414, 686427892326,  124567799864, 5876200, 43972749806, 2074032, 314347131666,  372080760660, 1388382, 13780165146, 779344, 4609160632 },
  { "KBPPKQQ", 5757601328, 1288788, 17907021412, 337756, 987545501464,  5669420684, 1256260, 17467284764, 323908, 626426026132,  361119475332, 13848, 439736648, 32528, 88180644 },
  { "KBPPKQR", 40681622760, 965386, 108662174536, 867460, 930682029850,  40030028490, 941346, 105311747500, 781380, 504220813032,  426461216818, 86080, 3350427036, 24040, 651594270 },
  { "KBPPKRB", 218613094746, 7228792, 254543215672, 48757626, 735860942228,  190152272044, 3445326, 182764617492, 28883004, 276615093882,  459245848346, 19874622, 71778598180, 3783466, 28460822702 },
  { "KBPPKRN", 287668874208, 208208, 292662753618, 166491058, 654049265540,  240241880390, 116852, 183684658224, 74267978, 225563388304,  428485877236, 92223080, 108978095394, 91356, 47426993818 },
  { "KBPPKRP", 290821003158, 5295322, 265224826658, 8033062, 386605237630,  222707666526, 2422770, 145463791006, 3852258, 114717899008,  271887338622, 4180804, 119761035652, 2872552, 68113336632 },
  { "KBPPKRR", 119971642220, 15467192, 215197297780, 886180, 821672833968,  112196789140, 10287368, 182548707248, 583888, 354807944104,  466864889864, 302292, 32648590532, 5179824, 7774853080 },
  { "KNNNKBP", 697295388888, 35221354020, 810114461136, 76939116, 165635115384,  497095737012, 15944162088, 267663241836, 29897652, 32463389820,  133171725564, 47041464, 542451219300, 19277191932, 200199651876 },
  { "KNNNKNP", 962497101222, 15806317128, 616905375780, 282451134, 156987720144,  617369006646, 5942719578, 161960004558, 103940550, 27820757076,  129166963068, 178510584, 454945371222, 9863597550, 345128094576 },
  { "KNNNKPP", 836402456040, 31493245248, 343648646136, 36603816, 137915217912,  483849041160, 6802800684, 101042079456, 9295452, 15901572960,  122013644952, 27308364, 242606566680, 24690444564, 352553414880 },
  { "KNNNKQP", 170617555764, 9211158720, 359491482948, 109150968, 974373266352,  162378093330, 7630029246, 282327239514, 78436326, 360782629992,  613590636360, 30714642, 77164243434, 1581129474, 8239462434 },
  { "KNNNKRP", 180981852672, 10517586672, 1193064790404, 47641692, 256846314456,  167960496864, 7972262106, 582669139404, 16538334, 54577991700,  202268322756, 31103358, 610395651000, 2545324566, 13021355808 },
  { "KNNPKBB", 345687573552, 187950032, 1178031965324, 49621998296, 91131288700,  286477590428, 73966360, 558322099584, 8396162784, 9070088132,  82061200568, 41225835512, 619709865740, 113983672, 59209983124 },
  { "KNNPKBN", 400808290024, 2151100, 1303110295408, 38856408, 5889784864,  315650711294, 1950750, 546481205524, 598558, 205441162,  5684343702, 38257850, 756629089884, 200350, 85157578730 },
  { "KNNPKBP", 612053207728, 8826143206, 519576002514, 7154452, 172702202762,  417145226300, 4342703680, 180661270096, 3836346, 41954046202,  130748156560, 3318106, 338914732418, 4483439526, 194907981428 },
  { "KNNPKNN", 464815095352, 4849244, 1283334421904, 91089156, 475893680,  343447781508, 3115780, 518837470468, 158776, 51380756,  424512924, 90930380, 764496951436, 1733464, 121367313844 },
  { "KNNPKNP", 630987805192, 15797506110, 544742131640, 6098102, 153886427536,  411854603324, 7712861050, 190243007484, 3064974, 34293545792,  119592881744, 3033128, 354499124156, 8084645060, 219133201868 },
  { "KNNPKPP", 678377850128, 11349612088, 152776423120, 102226720, 186593333448,  386407808280, 3021084332, 49187251740, 39249588, 39822948508,  146770384940, 62977132, 103589171380, 8328527756, 291970041848 },
  { "KNNPKQB", 113718468088, 328554880, 186932087366, 12738414, 1189778817932,  110320838462, 318963384, 168306156976, 11888838, 583382059628,  606396758304, 849576, 18625930390, 9591496, 3397629626 },
  { "KNNPKQN", 128804144936, 217082524, 198859859744, 12225228, 1198813736496,  124124576002, 207977308, 175827936744, 11192948, 562168224286,  636645512210, 1032280, 23031923000, 9105216, 4679568934 },
  { "KNNPKQP", 149804619214, 2612158028, 114303064190, 139131416, 901758210834,  142665085184, 2325031812, 93502039586, 114639548, 405500286494,  496257924340, 24491868, 20801024604, 287126216, 7139534030 },
  { "KNNPKQQ", 3630347416, 127504, 51210711000, 45090716, 1288932343524,  3586269280, 127468, 50280356904, 44318104, 808428835532,  480503507992, 772612, 930354096, 36, 44078136 },
  { "KNNPKQR", 15046901788, 88096, 232799304142, 12981736, 1188829847102,  14610288426, 88020, 220402456590, 12532766, 627314541486,  561515305616, 448970, 12396847552, 76, 436613362 },
  { "KNNPKRB", 186432631694, 991508, 654192063394, 204313685052, 565316731328,  168201293822, 935154, 462433452634, 99130793494, 132573432184,  432743299144, 105182891558, 191758610760, 56354, 18231337872 },
  { "KNNPKRN", 215419506184, 1206210, 982111004800, 17532948796, 430012807542,  189333162722, 1143318, 595216086090, 5816267940, 71973247218,  358039560324, 11716680856, 386894918710, 62892, 26086343462 },
  { "KNNPKRP", 231473482386, 6976518122, 668995396614, 23534022, 355479418580,  196973490628, 4626537384, 355876103198, 11720630, 86619230784,  268860187796, 11813392, 313119293416, 2349980738, 34499991758 },
  { "KNNPKRR", 60531652736, 1895236588, 448331566300, 154462596, 1029564940380,  56040235492, 1446148760, 383004819584, 113684928, 421735018524,  607829921856, 40777668, 65326746716, 449087828, 4491417244 },
  { "KNPPKBB", 341287225484, 418290572, 666783665680, 72224594692, 201734806736,  272916039392, 154850852, 322326334864, 33391797108, 53069136716,  148665670020, 38832797584, 344457330816, 263439720, 68371186092 },
  { "KNPPKBN", 428384428558, 15070, 795287171014, 40801778, 91782093716,  320574367340, 8286, 354603379114, 3811238, 6676592954,  85105500762, 36990540, 440683791900, 6784, 107810061218 },
  { "KNPPKBP", 450188529128, 26574954, 355752830958, 154452, 197733883130,  308030414002, 13169398, 144602542814, 59022, 53705124300,  144028758830, 95430, 211150288144, 13405556, 142158115126 },
  { "KNPPKNN", 525686558588, 4688564, 798330845888, 8314761868, 11553311240,  366808544332, 1469580, 314696741892, 125187420, 226215708,  11327095532, 8189574448, 483634103996, 3218984, 158878014256 },
  { "KNPPKNP", 537525173518, 12898234, 326214715754, 11704, 163368680298,  342590688646, 5801088, 121355100326, 2640, 42399716836,  120968963462, 9064, 204859615428, 7097146, 194934484872 },
  { "KNPPKPP", 521546085100, 5515384, 74030216028, 4016244, 184783711752,  299450714132, 2186964, 23216517228, 1403824, 51213645392,  133570066360, 2612420, 50813698800, 3328420, 222095370968 },
  { "KNPPKQB", 87458956412, 15765070, 126901723818, 2534, 938644910542,  85681931400, 15437356, 117589386342, 1940, 478571401894,  460073508648, 594, 9312337476, 327714, 1777025012 },
  { "KNPPKQN", 112878123370, 2651120, 125737344230, 12204, 940724544848,  110119453560, 2562850, 113129481796, 8344, 458606652382,  482117892466, 3860, 12607862434, 88270, 2758669810 },
  { "KNPPKQP", 113178286108, 7756700, 59654831832, 3947414, 723978741646,  109485005880, 7040056, 49403269730, 2735814, 347453258056,  376525483590, 1211600, 10251562102, 716644, 3693280228 },
  { "KNPPKQQ", 4681285112, 726680, 16440327912, 109720, 1022383148508,  4635922176, 703532, 16086469016, 106372, 661134957836,  361248190672, 3348, 353858896, 23148, 45362936 },
  { "KNPPKQR", 31070209494, 490326, 104610827586, 604336, 976639375434,  30522678498, 477298, 101794333862, 563112, 549540106162,  427099269272, 41224, 2816493724, 13028, 547530996 },
  { "KNPPKRB", 205016632296, 6527202, 251411978760, 107122056, 784824825934,  183033174774, 3820270, 184668047382, 61514098, 314091602408,  470733223526, 45607958, 66743931378, 2706932, 21983457522 },
  { "KNPPKRN", 253679143838, 116840, 266104472762, 313551096, 746744155280,  218060333294, 66408, 179930606398, 153380862, 283713771970,  463030383310, 160170234, 86173866364, 50432, 35618810544 },
  { "KNPPKRP", 248123318766, 4836600, 249983381942, 1969964, 468006566526,  198485291494, 2480386, 147562744534, 805962, 160299987160,  307706579366, 1164002, 102420637408, 2356214, 49638027272 },
  { "KNPPKRR", 96798798240, 1713594540, 200104353100, 96832, 890535131812,  92821220120, 1446725728, 176666608132, 63800, 410923541152,  479611590660, 33032, 23437744968, 266868812, 3977578120 },
  { "KPPPKBB", 299628142068, 569319420, 410670494808, 53492928, 271329532944,  235166732676, 196714620, 196651488492, 26928840, 103057041900,  168272491044, 26564088, 214019006316, 372604800, 64461409392 },
  { "KPPPKBN", 380232460434, 0, 198107343408, 545874, 427920655620,  277484540796, 0, 111147063018, 196626, 146467106088,  281453549532, 349248, 86960280390, 0, 102747919638 },
  { "KPPPKBP", 331500591222, 4082868, 164856043878, 12216, 266238396132,  227948752824, 2495658, 81977530104, 6174, 85049432784,  181188963348, 6042, 82878513774, 1587210, 103551838398 },
  { "KPPPKNN", 470928058392, 6594036, 423622748676, 50286669672, 82023946920,  319624806588, 2035032, 181437229812, 15281495136, 18753339960,  63270606960, 35005174536, 242185518864, 4559004, 151303251804 },
  { "KPPPKNP", 404052604050, 6520152, 172469900688, 10548, 202958787138,  258424437498, 3354240, 76267871724, 3312, 60282550770,  142676236368, 7236, 96202028964, 3165912, 145628166552 },
  { "KPPPKPP", 356089441548, 19772916, 65393986968, 5725764, 166567391172,  212813404188, 8892444, 25176519324, 2535672, 51824370228,  114743020944, 3190092, 40217467644, 10880472, 143276037360 },
  { "KPPPKQB", 55565316606, 1713216, 97581443070, 11484, 733233119148,  55157448402, 1697118, 94276947858, 8550, 385662804600,  347570314548, 2934, 3304495212, 16098, 407868204 },
  { "KPPPKQN", 72030330348, 2981622, 101151361842, 0, 732347734608,  71441159946, 2943384, 95744960880, 0, 367909842318,  364437892290, 0, 5406400962, 38238, 589170402 },
  { "KPPPKQP", 70147651926, 12530802, 48983004816, 5171274, 564817299426,  69235919496, 11929092, 44505261972, 4390962, 281220716022,  283596583404, 780312, 4477742844, 601710, 911732430 },
  { "KPPPKQQ", 3996904308, 1814328, 9447991548, 4824, 791717373888,  3975110604, 1800564, 9287280084, 4764, 521834710512,  269882663376, 60, 160711464, 13764, 21793704 },
  { "KPPPKQR", 24947955594, 1391076, 26602372314, 902994, 804298995594,  24756238374, 1381386, 26409917682, 888168, 483930480918,  320368514676, 14826, 192454632, 9690, 191717220 },
  { "KPPPKRB", 183257921148, 21921078, 99064122420, 184140, 668976578118,  164393771406, 12922506, 72892590048, 78222, 297799544346,  371177033772, 105918, 26171532372, 8998572, 18864149742 },
  { "KPPPKRN", 223406440320, 726918, 109248459000, 3312978, 637172753028,  192110888226, 531354, 72446433912, 1554876, 270539498160,  366633254868, 1758102, 36802025088, 195564, 31295552094 },
  { "KPPPKRP", 203617358196, 7833306, 50465581572, 1154694, 480468552024,  162199781994, 4227936, 31845845352, 676986, 200927685276,  279540866748, 477708, 18619736220, 3605370, 41417576202 },
  { "KPPPKRR", 71640466716, 4892952, 84896833512, 24, 755843747556,  69811362504, 4350912, 76040397684, 12, 389242795416,  366600952140, 12, 8856435828, 542040, 1829104212 },
  { "KQBBKBP", 1274782933010, 18323492, 154435497272, 200949962, 9107651976,  540057506802, 697614, 3099608570, 14741312, 225971278,  8881680698, 186208650, 151335888702, 17625878, 734725426208 },
  { "KQBBKNP", 1366934272146, 11818327478, 95874422574, 178983840, 7875056538,  540855865668, 86534094, 2268464328, 10394986, 177266500,  7697790038, 168588854, 93605958246, 11731793384, 826078406478 },
  { "KQBBKPP", 1089529704076, 10551308, 49882689572, 264597488, 10327702700,  407815487540, 74260, 232712616, 8542216, 67049072,  10260653628, 256055272, 49649976956, 10477048, 681714216536 },
  { "KQBBKQP", 796937649402, 119551806, 247328800092, 2888720004, 196729990616,  498094968298, 49197218, 37286265794, 510447280, 7457646986,  189272343630, 2378272724, 210042534298, 70354588, 298842681104 },
  { "KQBBKRP", 1172905631082, 8852074, 177206092070, 490294898, 21049412940,  537192595798, 1293152, 5632799578, 24904210, 546932838,  20502480102, 465390688, 171573292492, 7558922, 635713035284 },
  { "KQBNKBP", 1311935870189, 1014269641, 148191022682, 13649, 13742716097,  577010497912, 59928662, 2373570953, 160, 293064435,  13449651662, 13489, 145817451729, 954340979, 734925372277 },
  { "KQBNKNP", 1374862913772, 1221327520, 131071510292, 1122, 11863846416,  576998625759, 20124400, 2449722999, 1, 268588963,  11595257453, 1121, 128621787293, 1201203120, 797864288013 },
  { "KQBNKPP", 1139427964152, 1308520, 22001079046, 58848, 15222485542,  434577445446, 28306, 92927796, 12, 91115108,  15131370434, 58836, 21908151250, 1280214, 704850518706 },
  { "KQBNKQP", 822797030993, 12364028, 233295691305, 1847889, 224236314251,  530763721374, 5498895, 38906830416, 159094, 10060852343,  214175461908, 1688795, 194388860889, 6865133, 292033309619 },
  { "KQBNKRP", 1208222448919, 276310, 158394870232, 28426138, 41352798011,  574056373412, 18069, 4831875913, 259546, 848535182,  40504262829, 28166592, 153562994319, 258241, 634166075507 },
  { "KQBPKBB", 1208814356154, 229460572, 201355614536, 9526, 1624920634,  597395687914, 4091854, 12186313592, 952, 117398494,  1507522140, 8574, 189169300944, 225368718, 611418668240 },
  { "KQBPKBN", 1276795751729, 51640, 179383311589, 1688, 1033846676,  601430182226, 5353, 8242890770, 0, 30414457,  1003432219, 1688, 171140420819, 46287, 675365569503 },
  { "KQBPKBP", 1008717693430, 3925117, 103205645952, 88, 14283095289,  454799581366, 380694, 2089550000, 0, 263219778,  14019875511, 88, 101116095952, 3544423, 553918112064 },
  { "KQBPKNN", 1340874396150, 1325896, 155004479292, 55341730, 149391786,  603246131292, 32264, 6444250574, 51720, 13026956,  136364830, 55290010, 148560228718, 1293632, 737628264858 },
  { "KQBPKNP", 1066656089944, 3416277, 79715416487, 146975, 12090548111,  455386987061, 197819, 1519535209, 94, 246011655,  11844536456, 146881, 78195881278, 3218458, 611269102883 },
  { "KQBPKPP", 855274000214, 4550770, 21542303508, 424144, 14759273824,  340710917864, 66864, 74203628, 78, 74260970,  14685012854, 424066, 21468099880, 4483906, 514563082350 },
  { "KQBPKQB", 465711047131, 61172978, 511608178346, 4605, 260753849138,  377823808085, 37633158, 203144264816, 1150, 28697785597,  232056063541, 3455, 308463913530, 23539820, 87887239046 },
  { "KQBPKQN", 561369285957, 57034947, 477196235828, 0, 235448077714,  432703045215, 32475788, 159313507068, 0, 17654464735,  217793612979, 0, 317882728760, 24559159, 128666240742 },
  { "KQBPKQP", 594178417996, 62050466, 201080046340, 7836734, 186334481360,  403063695627, 29368473, 43764935929, 782763, 10293949046,  176040532314, 7053971, 157315110411, 32681993, 191114722369 },
  { "KQBPKQQ", 202286104934, 44931448, 165590614164, 231620, 723260323512,  186310948218, 40397090, 131691442198, 151372, 291660553928,  431599769584, 80248, 33899171966, 4534358, 15975156716 },
  { "KQBPKQR", 375972201124, 24127831, 304664124787, 6908832, 503385345808,  320428389223, 19235536, 176136945887, 2154290, 113116767870,  390268577938, 4754542, 128527178900, 4892295, 55543811901 },
  { "KQBPKRB", 1092962417370, 25421454, 115503043582, 4410887, 149124395201,  586269770956, 1166603, 16951188583, 214723, 6481151941,  142643243260, 4196164, 98551854999, 24254851, 506692646414 },
  { "KQBPKRN", 1155504799252, 278220, 109505462615, 39662539, 127390856424,  594876262767, 20241, 11249629320, 962327, 3576618151,  123814238273, 38700212, 98255833295, 257979, 560628536485 },
  { "KQBPKRP", 921632706907, 2983554, 75072119157, 1483997, 79284705323,  452049302009, 537324, 3959886460, 44291, 1142961754,  78141743569, 1439706, 71112232697, 2446230, 469583404898 },
  { "KQBPKRR", 907826696206, 78392688, 177349684564, 0, 202586670660,  557336593806, 8564836, 37333752640, 0, 15024581524,  187562089136, 0, 140015931924, 69827852, 350490102400 },
  { "KQNNKBP", 1295175318376, 4359304120, 185877311274, 2205692, 21488453170,  606476408098, 260120754, 4716776670, 94454, 302362520,  21186090650, 2111238, 181160534604, 4099183366, 688698910278 },
  { "KQNNKNP", 1353188605394, 3502792148, 177197440222, 1830002, 17147631730,  606173048794, 171202014, 5062788858, 90854, 348631976,  16798999754, 1739148, 172134651364, 3331590134, 747015556600 },
  { "KQNNKPP", 1100601164768, 8255529904, 71495161156, 4785616, 19742958732,  457714915048, 37478784, 383019104, 48984, 72758816,  19670199916, 4736632, 71112142052, 8218051120, 642886249720 },
  { "KQNNKQP", 704786434664, 8604617044, 363356006830, 61165824, 235553724478,  508320586518, 2697910686, 90769747982, 10490782, 9957026528,  225596697950, 50675042, 272586258848, 5906706358, 196465848146 },
  { "KQNNKRP", 1174573685106, 4493789714, 202034176140, 19255920, 58896613104,  599939551124, 371309652, 10402111356, 647162, 1042143202,  57854469902, 18608758, 191632064784, 4122480062, 574634133982 },
  { "KQNPKBB", 1232830470092, 280603920, 154774140400, 18778399566, 41235002648,  632549638604, 6502226, 11325596724, 790482456, 905528000,  40329474648, 17987917110, 143448543676, 274101694, 600280831488 },
  { "KQNPKBN", 1298639115842, 8920, 191408958807, 18695444, 3020439513,  636158174853, 1034, 9379030668, 63173, 40478282,  2979961231, 18632271, 182029928139, 7886, 662480940989 },
  { "KQNPKBP", 1019598783009, 19855642, 105872182413, 55703, 27002635090,  480350284843, 1324830, 2710182410, 130, 374091606,  26628543484, 55573, 103162000003, 18530812, 539248498166 },
  { "KQNPKNN", 1358345290488, 1352262, 173374786184, 67741992, 170019132,  637629746298, 32388, 7932570298, 330558, 15068468,  154950664, 67411434, 165442215886, 1319874, 720715544190 },
  { "KQNPKNP", 1072549862574, 2709501, 91376341143, 146827, 20819709730,  480819005493, 88310, 2185687626, 432, 431101958,  20388607772, 146395, 89190653517, 2621191, 591730857081 },
  { "KQNPKPP", 858672926630, 2038220, 30523305778, 957860, 21510045590,  359726484142, 22200, 164928208, 764, 96735708,  21413309882, 957096, 30358377570, 2016020, 498946442488 },
  { "KQNPKQB", 432078435234, 269703847, 558142914203, 1591, 283517452527,  368136221286, 156229536, 245530679099, 386, 31754617703,  251762834824, 1205, 312612235104, 113474311, 63942213948 },
  { "KQNPKQN", 513584461402, 37366385, 539113035375, 60, 257210026428,  418610083895, 21376626, 206774252535, 1, 20172034953,  237037991475, 59, 332338782840, 15989759, 94974377507 },
  { "KQNPKQP", 552644464845, 25598685, 244225324336, 11207255, 211039389756,  400863752164, 9207302, 68492218179, 1974011, 14068732163,  196970657593, 9233244, 175733106157, 16391383, 151780712681 },
  { "KQNPKQQ", 172747888306, 21453370, 175238435434, 1265822, 779047417950,  163866553706, 19129038, 148830200440, 793734, 332861071092,  446186346858, 472088, 26408234994, 2324332, 8881334600 },
  { "KQNPKQR", 342254945560, 11342101, 315380606853, 5672351, 562274396721,  304646945911, 8958574, 201303661634, 2005595, 139616176296,  422658220425, 3666756, 114076945219, 2383527, 37607999649 },
  { "KQNPKRB", 1110701643819, 25725014, 109752290255, 9540907, 173004743703,  619740226102, 1666247, 18090904093, 401226, 7744550342,  165260193361, 9139681, 91661386162, 24058767, 490961417717 },
  { "KQNPKRN", 1169763479134, 218366, 102585175330, 68349527, 155898091897,  628068070018, 13969, 12320769463, 2019167, 5186875393,  150711216504, 66330360, 90264405867, 204397, 541695409116 },
  { "KQNPKRP", 924956644010, 2282545, 76498588438, 787270, 100818848656,  476100157864, 252172, 5665291481, 21803, 1670160499,  99148688157, 765467, 70833296957, 2030373, 448856486146 },
  { "KQNPKRR", 871121396280, 9342955226, 202459412886, 0, 240791934930,  573878392982, 2045478240, 49231792282, 0, 20422084506,  220369850424, 0, 153227620604, 7297476986, 297243003298 },
  { "KQPPKBB", 965886817340, 507176608, 88654814836, 3112556, 54877324780,  500974148864, 10869028, 6608170680, 104496, 1745528820,  53131795960, 3008060, 82046644156, 496307580, 464912668476 },
  { "KQPPKBN", 1017412138830, 0, 47912538486, 143860, 77650351916,  503557417368, 0, 4258533412, 1762, 1522869346,  76127482570, 142098, 43654005074, 0, 513854721462 },
  { "KQPPKBP", 782518527268, 2255988, 47447070168, 526, 46578310060,  376622405178, 247056, 2104326380, 0, 468522310,  46109787750, 526, 45342743788, 2008932, 405896122090 },
  { "KQPPKNN", 1062340415488, 3847940, 89290661732, 8539513780, 11196390164,  504615976564, 84212, 4422955276, 139157396, 160648440,  11035741724, 8400356384, 84867706456, 3763728, 557724438924 },
  { "KQPPKNP", 822313268026, 3316420, 44533523136, 2401288, 33113162026,  377303334014, 221572, 1332056536, 5544, 559883258,  32553278768, 2395744, 43201466600, 3094848, 445009934012 },
  { "KQPPKPP", 639520541744, 7592612, 25022037476, 2564736, 22586639184,  280258488044, 96092, 276971352, 19924, 118723372,  22467915812, 2544812, 24745066124, 7496520, 359262053700 },
  { "KQPPKQB", 312543987268, 23936784, 430740576016, 20714, 237193500550,  277323980926, 18641646, 203562945418, 5950, 28433247948,  208760252602, 14764, 227177630598, 5295138, 35220006342 },
  { "KQPPKQN", 365666454608, 33207338, 431033442254, 0, 210090234528,  312592109956, 22075400, 179269215438, 0, 17455421094,  192634813434, 0, 251764226816, 11131938, 53074344652 },
  { "KQPPKQP", 367915808250, 64275582, 213345160604, 20525834, 188321984818,  284535114318, 30556594, 75760382840, 5880612, 18863566560,  169458418258, 14645222, 137584777764, 33718988, 83380693932 },
  { "KQPPKQQ", 122322180180, 42175148, 113503970952, 79988, 635117854620,  118892195700, 40597864, 102189630452, 45564, 288216352308,  346901502312, 34424, 11314340500, 1577284, 3429984480 },
  { "KQPPKQR", 242819698530, 22448096, 113968027298, 9686248, 582982309960,  225134487410, 19628256, 85536658284, 5409910, 198642638028,  384339671932, 4276338, 28431369014, 2819840, 17685211120 },
  { "KQPPKRB", 856912335450, 65835680, 59758585734, 2932, 152110989408,  490499468036, 5224132, 9734262742, 200, 9099866778,  143011122630, 2732, 50024322992, 60611548, 366412867414 },
  { "KQPPKRN", 898750381508, 1768424, 57741228970, 220692, 137828503178,  496098582004, 313716, 6956857192, 9642, 6283059334,  131545443844, 211050, 50784371778, 1454708, 402651799504 },
  { "KQPPKRP", 703308454576, 5691850, 26587209484, 1300574, 109061608702,  372014075386, 688218, 4013836532, 82336, 3166818452,  105894790250, 1218238, 22573372952, 5003632, 331294379190 },
  { "KQPPKRR", 539170531792, 17014316, 239138948544, 0, 238306142828,  398083034416, 8185916, 80652931988, 0, 30594669568,  207711473260, 0, 158486016556, 8828400, 141087497376 },
  { "KQQBKBP", 1306575203120, 966, 5020921564, 0, 332221262,  416657971490, 0, 113628588, 0, 9916698,  322304564, 0, 4907292976, 966, 889917231630 },
  { "KQQBKNP", 1351363476304, 100, 4464747556, 0, 235829816,  416698057192, 0, 79807198, 0, 3652386,  232177430, 0, 4384940358, 100, 934665419112 },
  { "KQQBKPP", 1050423325904, 96, 4724506228, 12908, 312575608,  313526691848, 0, 38037544, 0, 4311912,  308263696, 12908, 4686468684, 96, 736896634056 },
  { "KQQBKQP", 931235119326, 4084, 171345968222, 960598, 14805650890,  411979354934, 20, 4102964594, 1566, 699195662,  14106455228, 959032, 167243003628, 4064, 519255764392 },
  { "KQQBKRP", 1234780877900, 121974, 8046436358, 88736, 2215749296,  416467528368, 10, 212194368, 242, 101793788,  2113955508, 88494, 7834241990, 121964, 818313349532 },
  { "KQQNKBP", 1333376367286, 880, 6394511140, 2576, 405824002,  444870165202, 0, 150330766, 4, 9379776,  396444226, 2572, 6244180374, 880, 888506202084 },
  { "KQQNKNP", 1378376594106, 62, 5594355728, 10, 341462842,  444919247060, 2, 106635914, 0, 3992772,  337470070, 10, 5487719814, 60, 933457347046 },
  { "KQQNKPP", 1069847624236, 1796, 5930463220, 56908, 425364448,  334254832372, 0, 52583100, 4, 4715692,  420648756, 56904, 5877880120, 1796, 735592791864 },
  { "KQQNKQP", 928684026132, 60674, 197483775112, 3648028, 19464552146,  438752217302, 16528, 5455521972, 9658, 822110288,  18642441858, 3638370, 192028253140, 44146, 489931808830 },
  { "KQQNKRP", 1259510720816, 160752, 10658663664, 173052, 3121914952,  444614615414, 14, 282654270, 390, 132605660,  2989309292, 172662, 10376009394, 160738, 814896105402 },
  { "KQQPKBB", 1266571674808, 84515492, 2171733884, 4728, 733381488,  467192320768, 155336, 6192392, 0, 41773288,  691608200, 4728, 2165541492, 84360156, 799379354040 },
  { "KQQPKBN", 1312855786316, 0, 1358196268, 598, 535929118,  467227412738, 0, 1695698, 0, 11333348,  524595770, 598, 1356500570, 0, 845628373578 },
  { "KQQPKBP", 1014773711144, 479060, 4791409894, 14, 457973974,  350862961016, 17750, 92221452, 0, 10745830,  447228144, 14, 4699188442, 461310, 663910750128 },
  { "KQQPKNN", 1351990791472, 234536, 1508612688, 41593496, 80651640,  467230416764, 332, 2972416, 13568, 7038704,  73612936, 41579928, 1505640272, 234204, 884760374708 },
  { "KQQPKNP", 1048101673224, 436632, 3766528742, 296264, 409897142,  350899377930, 15274, 62295670, 68, 4257106,  405640036, 296196, 3704233072, 421358, 697202295294 },
  { "KQQPKPP", 808616206748, 787940, 3613385936, 406888, 587860284,  262067340120, 7568, 25048136, 12, 5148904,  582711380, 406876, 3588337800, 780372, 546548866628 },
  { "KQQPKQB", 824342272076, 14084550, 245198527530, 958, 26116316062,  442477875248, 2985480, 22155383262, 2, 2604197792,  23512118270, 956, 223043144268, 11099070, 381864396828 },
  { "KQQPKQN", 892356122624, 18210828, 223266754592, 0, 15966495380,  452556195706, 2325392, 13298061180, 0, 1383859506,  14582635874, 0, 209968693412, 15885436, 439799926918 },
  { "KQQPKQP", 729744525422, 27137114, 119005078542, 14980808, 26684325220,  346653318048, 1012518, 3447419660, 94826, 864100996,  25820224224, 14885982, 115557658882, 26124596, 383091207374 },
  { "KQQPKQQ", 452536590300, 35312680, 137403827524, 155928, 358743268224,  322803488296, 18806632, 59376506224, 65600, 85041575032,  273701693192, 90328, 78027321300, 16506048, 129733102004 },
  { "KQQPKQR", 663901991672, 21404858, 152304825666, 12432300, 225349002864,  402037093274, 7405756, 38141426302, 1816828, 27052699624,  198296303240, 10615472, 114163399364, 13999102, 261864898398 },
  { "KQQPKRB", 1196492179528, 56936144, 13942624144, 0, 4664897656,  466752555672, 416932, 132046286, 0, 355422894,  4309474762, 0, 13810577858, 56519212, 729739623856 },
  { "KQQPKRN", 1232871905140, 455922, 13944253128, 0, 3161393838,  466924522174, 3122, 103576752, 0, 212339736,  2949054102, 0, 13840676376, 452800, 765947382966 },
  { "KQQPKRP", 957828693070, 1825028, 7116123396, 417816, 4860153838,  350652136562, 44024, 160139758, 4720, 153620984,  4706532854, 413096, 6955983638, 1781004, 607176556508 },
  { "KQQPKRR", 1021221025728, 3208224, 102204760044, 0, 21949399100,  461306637044, 103104, 3972315392, 0, 1961386244,  19988012856, 0, 98232444652, 3105120, 559914388684 },
  { "KQQQKBP", 1205898987096, 1524, 305111496, 0, 71007072,  311123920290, 0, 860874, 0, 3495888,  67511184, 0, 304250622, 1524, 894775066806 },
  { "KQQQKNP", 1250135660892, 120, 226032252, 0, 49120788,  311125863900, 0, 771168, 0, 1641984,  47478804, 0, 225261084, 120, 939009796992 },
  { "KQQQKPP", 975940270164, 12, 452614512, 756, 67676436,  234562431048, 0, 4891872, 0, 1859520,  65816916, 756, 447722640, 12, 741377839116 },
  { "KQQQKQP", 1001222864958, 8010, 6806990148, 103032, 3704497248,  310541571648, 150, 325634646, 12, 261070596,  3443426652, 103020, 6481355502, 7860, 690681293310 },
  { "KQQQKRP", 1138061369448, 3840, 617465448, 78036, 711117768,  311072246736, 0, 6907692, 120, 49122504,  661995264, 77916, 610557756, 3840, 826989122712 },
  { "KQQRKBP", 1269633934196, 40838, 1120828402, 0, 217499848,  375816119910, 264, 3340002, 0, 6012972,  211486876, 0, 1117488400, 40574, 893817814286 },
  { "KQQRKNP", 1313972296548, 16590, 988274382, 0, 147422628,  375820746298, 20, 1717156, 0, 3009674,  144412954, 0, 986557226, 16570, 938151550250 },
  { "KQQRKPP", 1023742469412, 59400, 620662932, 8660, 175759172,  282639811192, 28, 4523256, 4, 3245656,  172513516, 8656, 616139676, 59372, 741102658220 },
  { "KQQRKQP", 1050982225834, 3333488, 16904865344, 498026, 8540736800,  374798632618, 31936, 576731852, 180, 450076562,  8090660238, 497846, 16328133492, 3301552, 676183593216 },
  { "KQQRKRP", 1200718346122, 96134, 1753513256, 53388, 1615221736,  375727197402, 520, 14129866, 124, 84145236,  1531076500, 53264, 1739383390, 95614, 824991148720 },
  { "KQRBKBP", 1380279146519, 4413713, 13277309069, 0, 3399711427,  501562109683, 91154, 182243908, 0, 69305847,  3330405580, 0, 13095065161, 4322559, 878717036836 },
  { "KQRBKNP", 1427731392770, 8015018, 10547776188, 0, 2809103616,  501619817874, 60660, 145179682, 0, 48692376,  2760411240, 0, 10402596506, 7954358, 926111574896 },
  { "KQRBKPP", 1107233835838, 56632, 7853095818, 1852930, 3410913266,  376531929692, 186, 65333732, 5100, 11106334,  3399806932, 1847830, 7787762086, 56446, 730701906146 },
  { "KQRBKQP", 928697152963, 1749283, 153233000993, 78422481, 120409611216,  486612785049, 186123, 12866047092, 844219, 2333888109,  118075723107, 77578262, 140366953901, 1563160, 442084367914 },
  { "KQRBKRP", 1195770567065, 2294345, 126061907670, 52510, 8240686490,  499468345142, 124111, 2075956250, 234, 269324855,  7971361635, 52276, 123985951420, 2170234, 696302221923 },
  { "KQRNKBP", 1399788585413, 28842806, 20314978717, 4173, 4674374293,  529340433577, 355616, 231515285, 153, 87650635,  4586723658, 4020, 20083463432, 28487190, 870448151836 },
  { "KQRNKNP", 1450553315821, 37645624, 14555521818, 452, 3796008551,  529390395367, 629683, 201617078, 2, 67313136,  3728695415, 450, 14353904740, 37015941, 921162920454 },
  { "KQRNKPP", 1124142607010, 208424, 10271867104, 68948, 4511759294,  396943964122, 512, 74242620, 88, 16923998,  4494835296, 68860, 10197624484, 207912, 727198642888 },
  { "KQRNKQP", 922462972894, 2096907, 169709602994, 2552387, 138088916428,  510677668897, 307293, 15835764673, 84297, 3146130106,  134942786322, 2468090, 153873838321, 1789614, 411785303997 },
  { "KQRNKRP", 1206234503720, 1099437, 139698470681, 73728, 11987565188,  526732724407, 44745, 2563251155, 358, 363934601,  11623630587, 73370, 137135219526, 1054692, 679501779313 },
  { "KQRPKBB", 1247702851986, 239092516, 111105085866, 7584, 1416965986,  555173026756, 4099672, 2898725780, 0, 67283114,  1349682872, 7584, 108206360086, 234992844, 692529825230 },
  { "KQRPKBN", 1318949771708, 20362569, 85598540619, 1005, 1083929937,  556512012616, 352391, 1612462514, 0, 18307801,  1065622136, 1005, 83986078105, 20010178, 762437759092 },
  { "KQRPKBP", 1056373279842, 1204657, 25712769126, 948369, 5292741312,  418034188004, 20103, 171682239, 47501, 117377421,  5175363891, 900868, 25541086887, 1184554, 638339091838 },
  { "KQRPKNN", 1376311035726, 5987832, 68041246114, 49605370, 116702328,  556826784514, 103132, 1304550710, 19374, 11677592,  105024736, 49585996, 66736695404, 5884700, 819484251212 },
  { "KQRPKNP", 1096929716729, 852733, 18624596093, 890051, 4080145618,  418062654547, 13086, 178124748, 39901, 82482986,  3997662632, 850150, 18446471345, 839647, 678867062182 },
  { "KQRPKPP", 850724184398, 865348, 5720486588, 1510034, 5984406188,  311647541828, 5790, 24303742, 26354, 38471786,  5945934402, 1483680, 5696182846, 859558, 539076642570 },
  { "KQRPKQB", 738367816924, 14827016, 236504959871, 12500287, 211673790616,  487931480552, 5508224, 49602790026, 3964759, 20599391761,  191074398855, 8535528, 186902169845, 9318792, 250436336372 },
  { "KQRPKQN", 817469298890, 20885251, 215071534591, 12287253, 189936270977,  510074870746, 6268647, 35099155463, 2813175, 12960027291,  176976243686, 9474078, 179972379128, 14616604, 307394428144 },
  { "KQRPKQP", 705388378936, 18986853, 96926397247, 21442332, 140478210958,  401976171106, 2475481, 11397946977, 2347715, 4944373989,  135533836969, 19094617, 85528450270, 16511372, 303412207830 },
  { "KQRPKQQ", 351749082626, 33073338, 118603530652, 42867516, 569193294062,  292184056982, 23141540, 74417001706, 31472018, 191487463076,  377705830986, 11395498, 44186528946, 9931798, 59565025644 },
  { "KQRPKQR", 562911166740, 10954265, 130692156422, 35036720, 438843036751,  413927765430, 5821455, 53116540238, 14231629, 91078776570,  347764260181, 20805091, 77575616184, 5132810, 148983401310 },
  { "KQRPKRB", 1126180054287, 26311785, 172181791746, 162107, 7671011085,  548579333292, 637889, 9025880028, 2806, 537281307,  7133729778, 159301, 163155911718, 25673896, 577600720995 },
  { "KQRPKRN", 1178786586062, 208552, 157024326208, 0, 5069580744,  551708410610, 4295, 6089282064, 0, 345438353,  4724142391, 0, 150935044144, 204257, 627078175452 },
  { "KQRPKRP", 940030884553, 1501959, 74735279439, 2505860, 22394410557,  416568331964, 56988, 1267198445, 98264, 487629607,  21906780950, 2407596, 73468080994, 1444971, 523462552589 },
  { "KQRPKRR", 932338520258, 1488688, 149366864338, 63628, 154574149722,  525035853398, 105568, 22534570470, 3840, 10572602046,  144001547676, 59788, 126832293868, 1383120, 407302666860 },
  { "KQRRKBP", 1337205956426, 77614, 4686034234, 3697898, 1612002096,  448269935186, 378, 65696798, 56964, 25248806,  1586753290, 3640934, 4620337436, 77236, 888936021240 },
  { "KQRRKNP", 1383123193596, 27550, 3651919172, 151514968, 716819846,  448301631508, 34, 49729608, 1603396, 7973586,  708846260, 149911572, 3602189564, 27516, 934821562088 },
  { "KQRRKPP", 1074723731308, 82340, 2972012124, 137648, 697969356,  336471393360, 132, 25660908, 140, 5498796,  692470560, 137508, 2946351216, 82208, 738252337948 },
  { "KQRRKQP", 1014880134604, 3468306, 102779757830, 5689478, 31298074258,  443843006040, 189748, 3550297248, 350258, 967094838,  30330979420, 5339220, 99229460582, 3278558, 571037128564 },
  { "KQRRKRP", 1263148628182, 131036, 8647803670, 31882, 4826100850,  448039364516, 1178, 143907126, 76, 177665236,  4648435614, 31806, 8503896544, 129858, 815109263666 },
  { "KRBBKBP", 1317946013520, 2594420, 191786311240, 6369430062, 31708974682,  633369429346, 256724, 17467174112, 640916532, 1188717074,  30520257608, 5728513530, 174319137128, 2337696, 684576584174 },
  { "KRBBKNP", 1421577268904, 11230029416, 125276634552, 923106950, 32941990966,  641710568384, 238838118, 9888848662, 33583550, 794655074,  32147335892, 889523400, 115387785890, 10991191298, 779866700520 },
  { "KRBBKPP", 1089323909516, 1842856, 115189673652, 538427324, 25803808220,  481352027916, 137732, 7416176856, 13251608, 184688016,  25619120204, 525175716, 107773496796, 1705124, 607971881600 },
  { "KRBBKQP", 357514193500, 932384, 511944899014, 5598745170, 478213910064,  295992704120, 735232, 287703975290, 2401496464, 66567582682,  411646327382, 3197248706, 224240923724, 197152, 61521489380 },
  { "KRBBKRP", 1001758705380, 4653408, 422391055470, 1022367208, 55751469810,  595388888156, 1763078, 53377424110, 136426658, 3761991786,  51989478024, 885940550, 369013631360, 2890330, 406369817224 },
  { "KRBNKBP", 1345630515725, 30914978, 192202208958, 582435609, 45099036336,  668422314465, 7194101, 17896704984, 52666315, 2019401605,  43079634731, 529769294, 174305503974, 23720877, 677208201260 },
  { "KRBNKNP", 1421978532760, 120121698, 154804208613, 10337351643, 40440603756,  676136163174, 19040944, 10704317078, 414236298, 1124523976,  39316079780, 9923115345, 144099891535, 101080754, 745842369586 },
  { "KRBNKPP", 1133121536162, 15395800, 86048812494, 27759220, 37807896564,  508160372090, 3637846, 6490193270, 1040198, 474777396,  37333119168, 26719022, 79558619224, 11757954, 624961164072 },
  { "KRBNKQP", 360818349667, 60156410, 463488596496, 211366310, 564425998931,  306467722010, 50683972, 277391319429, 81593793, 104406962266,  460019036665, 129772517, 186097277067, 9472438, 54350627657 },
  { "KRBNKRP", 1000299127734, 61815119, 433181248069, 27350727, 83090497309,  616288633960, 31415821, 66194021183, 607406, 5883603100,  77206894209, 26743321, 366987226886, 30399298, 384010493774 },
  { "KRBPKBB", 1122285867020, 610122504, 403577990434, 11722, 2470415678,  679625434266, 110004520, 46695674016, 960, 192424980,  2277990698, 10762, 356882316418, 500117984, 442660432754 },
  { "KRBPKBN", 1256800957178, 20592946386, 295128453462, 2066, 1610650166,  700513288264, 2000589551, 24059507397, 3, 50153527,  1560496639, 2063, 271068946065, 18592356835, 556287668914 },
  { "KRBPKBP", 1027529427180, 3279070, 131313900343, 5625597, 53808749444,  525666609719, 985255, 14136839833, 770108, 3798148681,  50010600763, 4855489, 117177060510, 2293815, 501862817461 },
  { "KRBPKNN", 1356275002320, 33876754800, 222581543878, 63474056, 208205736,  709242538520, 3558290156, 13800863294, 81800, 21764972,  186440764, 63392256, 208780680584, 30318464644, 647032463800 },
  { "KRBPKNP", 1098600624580, 4808011, 97017995230, 12627976, 49280183755,  533274759198, 1149527, 7952584699, 958949, 2373901223,  46906282532, 11669027, 89065410531, 3658484, 565325865382 },
  { "KRBPKPP", 851288776814, 4024554, 52211196712, 29444130, 51594514164,  397383441942, 527050, 5419797154, 1055426, 1602031746,  49992482418, 28388704, 46791399558, 3497504, 453905334872 },
  { "KRBPKQB", 281653056060, 10762747, 150793822374, 61547211, 922535109742,  256130186801, 9798825, 111918434796, 43595609, 358521522711,  564013587031, 17951602, 38875387578, 963922, 25522869259 },
  { "KRBPKQN", 323292599922, 10760311, 155276519369, 165013360, 912245787420,  289966205934, 9287391, 106549994575, 96419949, 330001630893,  582244156527, 68593411, 48726524794, 1472920, 33326393988 },
  { "KRBPKQP", 272360574674, 24011820, 220751507347, 205589304, 574771771509,  236685545511, 17208188, 142570565019, 111242114, 164218792764,  410552978745, 94347190, 78180942328, 6803632, 35675029163 },
  { "KRBPKQQ", 22889293346, 28178230, 146167087678, 53886774, 1038963805586,  21956497772, 27615684, 139437760458, 51580152, 565150084676,  473813720910, 2306622, 6729327220, 562546, 932795574 },
  { "KRBPKQR", 171008748039, 17359393, 199362329923, 24198141, 930560118822,  161516414993, 16290729, 167504912525, 21831562, 397564088933,  532996029889, 2366579, 31857417398, 1068664, 9492333046 },
  { "KRBPKRB", 526557480752, 730501231, 782920245213, 4531593, 164326975641,  417386824924, 392081345, 295963497857, 366654, 12880767962,  151446207679, 4164939, 486956747356, 338419886, 109170655828 },
  { "KRBPKRN", 650743735922, 224264255, 722973450694, 41371179, 135378282936,  481446539214, 105537091, 238513746637, 1881539, 6555834261,  128822448675, 39489640, 484459704057, 118727164, 169297196708 },
  { "KRBPKRP", 757639974711, 20121491, 281429111817, 3892882, 123351519795,  474538263663, 9301141, 60878577188, 884264, 8176327340,  115175192455, 3008618, 220550534629, 10820350, 283101711048 },
  { "KRBPKRR", 336771445220, 20516228, 701985296616, 93238, 365984138752,  297848450920, 7581880, 378999321084, 20346, 49768164512,  316215974240, 72892, 322985975532, 12934348, 38922994300 },
  { "KRNNKBP", 1306351652226, 3793082720, 247753203856, 8082262, 56397851936,  684394723678, 478136670, 31539021242, 797786, 2744363488,  53653488448, 7284476, 216214182614, 3314946050, 621956928548 },
  { "KRNNKNP", 1382902010236, 3135615032, 226838428798, 28446224, 45535079574,  694308877556, 322953472, 23123943650, 1539500, 1399728686,  44135350888, 26906724, 203714485148, 2812661560, 688593132680 },
  { "KRNNKPP", 1072597812896, 9006929176, 149915251968, 6959740, 47990911300,  520271491536, 514408872, 15977796092, 490092, 862299048,  47128612252, 6469648, 133937455876, 8492520304, 552326321360 },
  { "KRNNKQP", 296843677140, 4736115188, 508205376222, 36460082, 609941600576,  265527408454, 3349963840, 321306883822, 19145232, 128953641516,  480987959060, 17314850, 186898492400, 1386151348, 31316268686 },
  { "KRNNKRP", 909716759040, 5552516234, 525189940194, 179727802, 106779857082,  601103726272, 1325981900, 108707434332, 35196626, 7984703734,  98795153348, 144531176, 416482505862, 4226534334, 308613032768 },
  { "KRNPKBB", 1107843711510, 44718182154, 345147694920, 20842554586, 45273466376,  696321688538, 9134280258, 51960612360, 1746782478, 2341377296,  42932089080, 19095772108, 293187082560, 35583901896, 411522022972 },
  { "KRNPKBN", 1234986406879, 8047755230, 362340021860, 19311687, 3620715790,  722508953902, 1708599645, 37203389277, 193846, 83604260,  3537111530, 19117841, 325136632583, 6339155585, 512477452977 },
  { "KRNPKBP", 1024294665572, 24041943, 138761965599, 5895101, 75100777512,  543443318072, 7169707, 19217139838, 1293037, 6460797035,  68639980477, 4602064, 119544825761, 16872236, 480851347500 },
  { "KRNPKNN", 1335328941706, 9744738856, 302498362268, 76209744, 237930404,  735258393098, 1804397384, 24416713700, 437592, 24799156,  213131248, 75772152, 278081648568, 7940341472, 600070548608 },
  { "KRNPKNP", 1094374938984, 33655251, 110164527398, 5334651, 65864147361,  552762671538, 9325350, 11933188772, 1086853, 4423445176,  61440702185, 4247798, 98231338626, 24329901, 541612267446 },
  { "KRNPKPP", 844009861900, 5511958, 61106719948, 9031066, 68552002296,  410732564740, 1885306, 8699685346, 1032618, 3526856102,  65025146194, 7998448, 52407034602, 3626652, 433277297160 },
  { "KRNPKQB", 266764589065, 6864128, 137143009473, 41944462, 985979093194,  248390562375, 6487250, 106200380448, 33208511, 406874102346,  579104990848, 8735951, 30942629025, 376878, 18374026690 },
  { "KRNPKQN", 303906186364, 6647746, 142295156545, 38660917, 979625230998,  279701694204, 6223337, 102581131978, 30324366, 379185367045,  600439863953, 8336551, 39714024567, 424409, 24204492160 },
  { "KRNPKQP", 257343488153, 14689471, 201501804934, 41602503, 634738233686,  230396689568, 12515289, 134301060587, 28112475, 204391339770,  430346893916, 13490028, 67200744347, 2174182, 26946798585 },
  { "KRNPKQQ", 20269978546, 14476992, 118942821378, 55451552, 1103700725334,  19658627104, 14357906, 114858790074, 53998212, 626918967634,  476781757700, 1453340, 4084031304, 119086, 611351442 },
  { "KRNPKQR", 151020451188, 8032742, 187924494673, 24966550, 996876011353,  144830449170, 7792529, 164837815905, 23419801, 451805263525,  545070747828, 1546749, 23086678768, 240213, 6190002018 },
  { "KRNPKRB", 482604885809, 1931334586, 835558278751, 10011184, 189316426288,  397526932015, 1016800260, 347957170730, 736484, 15003101441,  174313324847, 9274700, 487601108021, 914534326, 85077953794 },
  { "KRNPKRN", 576283517177, 295588802, 801817719266, 71644109, 165773837820,  448644374567, 147999095, 303514513886, 3893415, 9193959967,  156579877853, 67750694, 498303205380, 147589707, 127639142610 },
  { "KRNPKRP", 691349437530, 38673513, 343625083146, 3783140, 152954007460,  459135531987, 20703092, 97188358695, 1538010, 12783585905,  140170421555, 2245130, 246436724451, 17970421, 232213905543 },
  { "KRNPKRR", 293763414802, 791997320, 689148424784, 3770962, 455935084374,  268796643386, 263788514, 411402838298, 1357196, 81040113536,  374894970838, 2413766, 277745586486, 528208806, 24966771416 },
  { "KRPPKBB", 697639782696, 910010824, 441986692732, 3338672, 61716488888,  471803355876, 262456388, 125078313196, 248084, 4521516036,  57194972852, 3090588, 316908379536, 647554436, 225836426820 },
  { "KRPPKBN", 850225033404, 73262252, 301724398128, 150814, 83279396186,  523651211138, 21346324, 74105507586, 5444, 3887819088,  79391577098, 145370, 227618890542, 51915928, 326573822266 },
  { "KRPPKBP", 769792457210, 2353368, 81404127374, 5856612, 93166840934,  421068992052, 365062, 16623860562, 2568630, 9325186106,  83841654828, 3287982, 64780266812, 1988306, 348723465158 },
  { "KRPPKNN", 933361973648, 26777176, 310088910480, 8716298944, 11503936548,  540751021016, 6834512, 60474117044, 193697736, 240219272,  11263717276, 8522601208, 249614793436, 19942664, 392610952632 },
  { "KRPPKNP", 827888789474, 1885478, 64595197550, 5816066, 75299453816,  430524725010, 243662, 10001239030, 2156210, 6492608500,  68806845316, 3659856, 54593958520, 1641816, 397364064464 },
  { "KRPPKPP", 627237877148, 3004876, 28337309236, 14972132, 81067893320,  316730726404, 564196, 4934071416, 4333296, 8506284432,  72561608888, 10638836, 23403237820, 2440680, 310507150744 },
  { "KRPPKQB", 192317163368, 2199830, 82578335694, 24191480, 797907198652,  184353256398, 1926636, 63909008302, 22430124, 353379268120,  444527930532, 1761356, 18669327392, 273194, 7963906970 },
  { "KRPPKQN", 219736583128, 3132610, 88295188998, 25451466, 791090050218,  208729168716, 2673532, 62137463360, 22754928, 330773829044,  460316221174, 2696538, 26157725638, 459078, 11007414412 },
  { "KRPPKQP", 183730262656, 5236072, 73474611748, 37911760, 580245204340,  170481264688, 3541920, 47219446692, 29686656, 229287032456,  350958171884, 8225104, 26255165056, 1694152, 13248997968 },
  { "KRPPKQQ", 15834088528, 2275288, 45965829780, 53586288, 901457548696,  15405804568, 2165228, 44855649744, 53249796, 541349020244,  360108528452, 336492, 1110180036, 110060, 428283960 },
  { "KRPPKQR", 117815242148, 1334316, 74329133200, 25212438, 839958315722,  114839094240, 1227486, 68920984516, 24487222, 417880096116,  422078219606, 725216, 5408148684, 106830, 2976147908 },
  { "KRPPKRB", 395284263682, 10544870, 594957424716, 322740, 170922260888,  322266312976, 2507188, 261848467768, 57474, 17548544174,  153373716714, 265266, 333108956948, 8037682, 73017950706 },
  { "KRPPKRN", 461581285220, 304544, 576467846790, 231844, 148599502066,  356872973270, 91984, 233510409806, 15752, 11282398768,  137317103298, 216092, 342957436984, 212560, 104708311950 },
  { "KRPPKRP", 481037239792, 5086930, 254417961734, 6640414, 171322807804,  327103920990, 1410206, 95026263114, 4097488, 24885280614,  146437527190, 2542926, 159391698620, 3676724, 153933318802 },
  { "KRPPKRR", 247007220984, 572004, 168259792520, 2716340, 693689403324,  226636583564, 190544, 116791804932, 1526320, 258235784220,  435453619104, 1190020, 51467987588, 381460, 20370637420 },
  { "KRRBKBP", 1442416854354, 6213328, 38241922806, 7872072, 11537420068,  595287980156, 376270, 1559471542, 186280, 215438244,  11321981824, 7685792, 36682451264, 5837058, 847128874198 },
  { "KRRBKNP", 1494075477134, 13680936, 33461807544, 294795220, 8500228658,  596167816788, 439954, 774027600, 2946414, 118221736,  8382006922, 291848806, 32687779944, 13240982, 897907660346 },
  { "KRRBKPP", 1159582360636, 48474892, 19692297772, 7831316, 9711435056,  446893409432, 596552, 232008008, 15380, 24990860,  9686444196, 7815936, 19460289764, 47878340, 712688951204 },
  { "KRRBKQP", 763777542062, 775150920, 263488895386, 187339158, 269440711310,  512064273440, 364240658, 65129918300, 16740250, 19488279844,  249952431466, 170598908, 198358977086, 410910262, 251713268622 },
  { "KRRBKRP", 1154234481162, 3026498, 249058343718, 3322640, 22026035962,  585187153422, 893356, 11076977378, 142854, 798285482,  21227750480, 3179786, 237981366340, 2133142, 569047327740 },
  { "KRRNKBP", 1451185116224, 53291438, 51936656700, 15619784, 15955386030,  621402920930, 2866610, 2275390052, 432830, 317629618,  15637756412, 15186954, 49661266648, 50424828, 829782195294 },
  { "KRRNKNP", 1508820864316, 70382732, 42763060616, 403808288, 11223661088,  622592142894, 2228138, 1235043184, 3668358, 166157466,  11057503622, 400139930, 41528017432, 68154594, 886228721422 },
  { "KRRNKPP", 1169039653284, 11438336, 27107605572, 409584, 12620664088,  466394317708, 291880, 449041964, 6996, 44732876,  12575931212, 402588, 26658563608, 11146456, 702645335576 },
  { "KRRNKQP", 721928387562, 107730038, 297208579286, 7721750, 305353007748,  509170697536, 59131850, 88667868062, 2029608, 26099512984,  279253494764, 5692142, 208540711224, 48598188, 212757690026 },
  { "KRRNKRP", 1146451758380, 2487876, 275806200266, 1030458, 29999520548,  608226726276, 1496068, 14498438302, 66020, 1272513374,  28727007174, 964438, 261307761964, 991808, 538225032104 },
  { "KRRPKBB", 1239637914664, 313783004, 220049220980, 9948, 2101294088,  645998727624, 14870428, 13662079568, 0, 105676448,  1995617640, 9948, 206387141412, 298912576, 593639187040 },
  { "KRRPKBN", 1337832881022, 40490122, 167818484376, 1390, 1598967674,  652635511204, 1693720, 7115164920, 0, 28984224,  1569983450, 1390, 160703319456, 38796402, 685197369818 },
  { "KRRPKBP", 1083130394690, 65781338, 52706640672, 68661682, 26647378428,  489658446442, 23840050, 2890767708, 4654010, 983520562,  25663857866, 64007672, 49815872964, 41941288, 593471948248 },
  { "KRRPKNN", 1415162682276, 11328396, 130767527448, 57628444, 163629552,  655638273660, 310852, 4124435104, 27548, 18306904,  145322648, 57600896, 126643092344, 11017544, 759524408616 },
  { "KRRPKNP", 1130107537086, 61791426, 47321529346, 1389150080, 15994106790,  491191769166, 14099888, 2012828804, 33634212, 308896702,  15685210088, 1355515868, 45308700542, 47691538, 638915767920 },
  { "KRRPKPP", 862544545080, 173521916, 37496610000, 5103724, 17573582780,  365611773992, 17918856, 1268109432, 168700, 174289464,  17399293316, 4935024, 36228500568, 155603060, 496932771088 },
  { "KRRPKQB", 390823849858, 573365288, 195886136734, 835378748, 700093382832,  328613066396, 495222588, 121042847130, 507946296, 209122271658,  490971111174, 327432452, 74843289604, 78142700, 62210783462 },
  { "KRRPKQN", 432152026768, 598556892, 315894902186, 26171263626, 549331746236,  355777858642, 487535028, 173123713610, 12065097380, 118327149408,  431004596828, 14106166246, 142771188576, 111021864, 76374168126 },
  { "KRRPKQP", 377246767950, 1164089600, 320953671974, 48164582, 318658635724,  295164558452, 809778350, 154186078150, 17796686, 43383017134,  275275618590, 30367896, 166767593824, 354311250, 82082209498 },
  { "KRRPKQQ", 68800146192, 1290207124, 183524604908, 114736060, 887530372656,  64046240836, 1245720528, 164999974368, 101622800, 429387795536,  458142577120, 13113260, 18524630540, 44486596, 4753905356 },
  { "KRRPKQR", 288810826756, 554486038, 170838660892, 58479140, 773868116818,  254008153192, 513659638, 126269015276, 44610216, 278945915746,  494922201072, 13868924, 44569645616, 40826400, 34802673564 },
  { "KRRPKRB", 1001724269136, 46233700, 394916925072, 331536, 11009790312,  615876827162, 18626894, 43063847092, 10586, 822042334,  10187747978, 320950, 351853077980, 27606806, 385847441974 },
  { "KRRPKRN", 1093053908132, 5465138, 342068322896, 0, 7391224146,  631895434562, 1398100, 27330914136, 0, 553607270,  6837616876, 0, 314737408760, 4067038, 461158473570 },
  { "KRRPKRP", 912189316492, 77098184, 146641842498, 7592702, 53486645996,  480416457904, 50008096, 10083486502, 1097192, 3010179078,  50476466918, 6495510, 136558355996, 27090088, 431772858588 },
  { "KRRPKRR", 609023874420, 120095108, 439288252252, 132032, 289486951568,  456679225220, 60477792, 173253375820, 15208, 29788260028,  259698691540, 116824, 266034876432, 59617316, 152344649200 },
  { "KRRRKBP", 1404881530614, 450893886, 13463484396, 15286098, 5771072094,  529041614526, 12416880, 305842530, 212892, 75350124,  5695721970, 15073206, 13157641866, 438477006, 875839916088 },
  { "KRRRKNP", 1454729274384, 27572466, 11075209698, 658254012, 2227663392,  529226233998, 246612, 184281702, 5596590, 19078050,  2208585342, 652657422, 10890927996, 27325854, 925503040386 },
  { "KRRRKPP", 1124592428712, 578952, 11993528700, 825168, 1949366604,  396551783796, 3816, 83703876, 1392, 9855816,  1939510788, 823776, 11909824824, 575136, 728040644916 },
  { "KRRRKQP", 858017196672, 18589974, 298104364548, 21512172, 73879959930,  491649934554, 6358716, 34280316372, 3695862, 3495131448,  70384828482, 17816310, 263824048176, 12231258, 366367262118 },
  { "KRRRKRP", 1322639233590, 111036, 22126235310, 152142, 12931462362,  528051178764, 4698, 982498380, 3222, 401751888,  12529710474, 148920, 21143736930, 106338, 794588054826 },
  { "KBBBKBB", 45601454976, 0, 450398385666, 0, 399564468,  38907809418, 0, 190854929724, 0, 79514064,  320050404, 0, 259543455942, 0, 6693645558 },
  { "KBBBKBN", 143941159527, 13140954705, 354531658950, 0, 138212520,  98129389176, 3815504382, 127873440003, 0, 23919645,  114292875, 0, 226658218947, 9325450323, 45811770351 },
  { "KBBBKNN", 283062412110, 24173497980, 217704458448, 0, 31529388,  160777602468, 3662022180, 65397296418, 0, 5332140,  26197248, 0, 152307162030, 20511475800, 122284809642 },
  { "KBBBKQB", 22080792804, 0, 94148119947, 3747931782, 318291340533,  20455690080, 0, 77609410674, 3211011732, 128566140720,  189725199813, 536920050, 16538709273, 0, 1625102724 },
  { "KBBBKQN", 63843758940, 5829221199, 52058776872, 3997140102, 324727375173,  57765873480, 3865368795, 40766735604, 3351243225, 124093032102,  200634343071, 645896877, 11292041268, 1963852404, 6077885460 },
  { "KBBBKQQ", 1922194830, 0, 22193752422, 7834316034, 357257320584,  1833336264, 0, 21305471394, 7384213026, 199319232522,  157938088062, 450103008, 888281028, 0, 88858566 },
  { "KBBBKQR", 30018944070, 106020477, 75299456391, 3551157339, 311385861261,  28239594804, 88889448, 62225004837, 3167544510, 136121219607,  175264641654, 383612829, 13074451554, 17131029, 1779349266 },
  { "KBBBKRB", 54350488131, 117310953, 411915607590, 153319350, 11904124770,  47345279361, 85368990, 181804931439, 7347708, 599325708,  11304799062, 145971642, 230110676151, 31941963, 7005208770 },
  { "KBBBKRN", 102845744367, 7909134990, 327598466301, 38147263407, 13763283525,  85433537937, 4494327426, 136902589845, 2548091706, 463706292,  13299577233, 35599171701, 190695876456, 3414807564, 17412206430 },
  { "KBBBKRR", 57810909390, 202850532, 267693286674, 0, 129504661170,  51840980370, 158714754, 158300539548, 0, 19542018534,  109962642636, 0, 109392747126, 44135778, 5969929020 },
  { "KBBNKBB", 79216381328, 4065518168, 427838588920, 275282432, 549152336,  70722389878, 2922859452, 171664240434, 638604, 77642912,  471509424, 274643828, 256174348486, 1142658716, 8493991450 },
  { "KBBNKBN", 167723087396, 38448763318, 321018125590, 92134, 107435338,  129246703440, 15921971184, 100200270690, 318, 18825648,  88609690, 91816, 220817854900, 22526792134, 38476383956 },
  { "KBBNKNN", 265074171720, 7797638988, 267603793256, 0, 41812036,  176699719558, 1719698880, 66961629044, 0, 6723798,  35088238, 0, 200642164212, 6077940108, 88374452162 },
  { "KBBNKQB", 39188173717, 0, 69811945429, 1333799362, 343479784632,  37149637027, 0, 58318851227, 1199569260, 148719713766,  194760070866, 134230102, 11493094202, 0, 2038536690 },
  { "KBBNKQN", 70518652689, 1804775530, 42476828585, 1362293542, 349839240014,  66127392687, 1383043516, 32644884385, 1201998844, 144030451848,  205808788166, 160294698, 9831944200, 421732014, 4391260002 },
  { "KBBNKQQ", 2022742486, 0, 17569338324, 2812728164, 382348292970,  1983317978, 0, 17141210608, 2716422432, 223546820262,  158801472708, 96305732, 428127716, 0, 39424508 },
  { "KBBNKQR", 20099911648, 9664120, 79051217892, 1284254356, 335461909596,  19299717920, 8759686, 68748550218, 1189455656, 156141287800,  179320621796, 94798700, 10302667674, 904434, 800193728 },
  { "KBBNKRB", 60169166408, 9411910, 365574859478, 31856813850, 36376117222,  54472865680, 7878954, 184648970736, 4014086423, 2243969487,  34132147735, 27842727427, 180925888742, 1532956, 5696300728 },
  { "KBBNKRN", 94196449608, 2156296508, 372108216094, 16957338699, 20391109755,  83495516712, 1409752734, 158334502305, 1246021347, 901978182,  19489131573, 15711317352, 213773713789, 746543774, 10700932896 },
  { "KBBNKRR", 37575069652, 18524894, 266973857050, 387410, 166189386834,  34853027042, 16043082, 177772021608, 152486, 32746527062,  133442859772, 234924, 89201835442, 2481812, 2722042610 },
  { "KBNNKBB", 103066618632, 44692070274, 374797492800, 1898384892, 1125756298,  93639661458, 26533234302, 138701928164, 61169908, 87177160,  1038579138, 1837214984, 236095564636, 18158835972, 9426957174 },
  { "KBNNKBN", 114562695484, 75260458, 426187131962, 322459, 107493125,  102615912849, 58028332, 156332835885, 2456, 16391470,  91101655, 320003, 269854296077, 17232126, 11946782635 },
  { "KBNNKNN", 140163474444, 473936838, 413458505914, 0, 56898516,  120105702948, 310148794, 138598845398, 0, 8473852,  48424664, 0, 274859660516, 163788044, 20057771496 },
  { "KBNNKQB", 47193177235, 0, 58677162957, 4076650, 361574686010,  45396444818, 0, 49871291835, 3692828, 163751741511,  197822944499, 383822, 8805871122, 0, 1796732417 },
  { "KBNNKQN", 60918548049, 38223622, 50553407414, 21869462, 368105141525,  58103133654, 34983512, 41760032946, 18781380, 159106239500,  208998902025, 3088082, 8793374468, 3240110, 2815414395 },
  { "KBNNKQQ", 1088121184, 0, 22468862696, 9393652, 394822124124,  1072005836, 0, 21904382410, 9074496, 236037708250,  158784415874, 319156, 564480286, 0, 16115348 },
  { "KBNNKQR", 1710134162, 0, 94811013042, 4201226, 353017008894,  1683532561, 0, 86076250079, 3856882, 171259531470,  181757477424, 344344, 8734762963, 0, 26601601 },
  { "KBNNKRB", 51263623782, 0, 334229688163, 63401373189, 58727083446,  47676518935, 0, 193904565520, 12249499370, 5192587167,  53534496279, 51151873819, 140325122643, 0, 3587104847 },
  { "KBNNKRN", 65897337536, 42383882, 416811719583, 6797149089, 29896220286,  60710579706, 37152710, 196103576168, 443334989, 1728527419,  28167692867, 6353814100, 220708143415, 5231172, 5186757830 },
  { "KBNNKRR", 2308465542, 0, 273886634552, 2953762, 208194571696,  2263667466, 0, 203477479260, 1385320, 53280638946,  154913932750, 1568442, 70409155292, 0, 44798076 },
  { "KNNNKBB", 55773704286, 84086400, 474718977510, 4548996936, 2092574856,  53696588772, 77842932, 216535184844, 230016864, 121554672,  1971020184, 4318980072, 258183792666, 6243468, 2077115514 },
  { "KNNNKBN", 76538025408, 49764060, 475841135784, 660621, 141334707,  73169559927, 45336399, 197428373001, 4500, 17914257,  123420450, 656121, 278412762783, 4427661, 3368465481 },
  { "KNNNKNN", 92931040008, 24444780, 472753204140, 0, 82143876,  88450984578, 19786542, 182179177092, 0, 11239872,  70904004, 0, 290574027048, 4658238, 4480055430 },
  { "KNNNKQB", 28939301340, 43574940, 80169191730, 37835295, 369897216639,  28317805692, 41322768, 71508586389, 31880352, 170761592883,  199135623756, 5954943, 8660605341, 2252172, 621495648 },
  { "KNNNKQN", 47639931789, 9798861, 66049739916, 136895814, 377438840784,  46188917709, 8974572, 57397505733, 111871683, 166953918387,  210484922397, 25024131, 8652234183, 824289, 1451014080 },
  { "KNNNKQQ", 410570850, 0, 39489900114, 32388660, 390093659124,  397754334, 0, 38418836052, 31325520, 231813272178,  158280386946, 1063140, 1071064062, 0, 12816516 },
  { "KNNNKQR", 529589367, 0, 99156211149, 14303418, 361480270482,  509306799, 0, 91550784966, 13407462, 178587688857,  182892581625, 895956, 7605426183, 0, 20282568 },
  { "KNNNKRB", 29897149182, 44852760, 319643304261, 92958183258, 76716296211,  28982672172, 42142410, 209518543836, 23430950328, 8686879338,  68029416873, 69527232930, 110124760425, 2710350, 914477010 },
  { "KNNNKRN", 49450994592, 10867509, 431494613415, 8839684266, 41286667686,  47359966107, 9421182, 219571844118, 759864813, 2960091864,  38326575822, 8079819453, 211922769297, 1446327, 2091028485 },
  { "KNNNKRR", 667496310, 0, 263061143016, 0, 232302003318,  636095406, 0, 201524933094, 0, 68500159584,  163801843734, 0, 61536209922, 0, 31400904 },
  { "KQBBKBB", 374228361070, 6478676, 72085981836, 0, 151684952,  175167896732, 14548, 4717882570, 0, 29560780,  122124172, 0, 67368099266, 6464128, 199060464338 },
  { "KQBBKBN", 398040552835, 3807489532, 59936471611, 0, 40573148,  176967705246, 174071718, 2765818461, 0, 7759205,  32813943, 0, 57170653150, 3633417814, 221072847589 },
  { "KQBBKNN", 419874163182, 8136163264, 47016923172, 0, 17749732,  177821797440, 272394100, 1818137234, 0, 3025856,  14723876, 0, 45198785938, 7863769164, 242052365742 },
  { "KQBBKQB", 175007790234, 93584766, 140753993791, 567262676, 71918655023,  126997036146, 51775710, 45309955193, 239162444, 7317425137,  64601229886, 328100232, 95444038598, 41809056, 48010754088 },
  { "KQBBKQN", 204589619610, 6417186753, 123043765858, 471566058, 66007235431,  141297434361, 1586486662, 32402407344, 144542480, 4484483783,  61522751648, 327023578, 90641358514, 4830700091, 63292185249 },
  { "KQBBKQQ", 73255125086, 47379422, 66887261032, 2955100724, 196135819030,  62402583256, 40440056, 46849643312, 2174082464, 68448605542,  127687213488, 781018260, 20037617720, 6939366, 10852541830 },
  { "KQBBKQR", 141559723832, 27134964, 117316511390, 1003166772, 110528004004,  108409268037, 19611392, 51171452309, 476828190, 19838194702,  90689809302, 526338582, 66145059081, 7523572, 33150455795 },
  { "KQBBKRB", 342044462884, 0, 81700215980, 48320038, 4720953316,  171833628002, 0, 7892782856, 789892, 188153880,  4532799436, 47530146, 73807433124, 0, 170210834882 },
  { "KQBBKRN", 365268815524, 4853430324, 53337822289, 11705712773, 5171213104,  175310145226, 349586164, 3902675486, 242511830, 110435924,  5060777180, 11463200943, 49435146803, 4503844160, 189958670298 },
  { "KQBBKRR", 303675036672, 24694212, 57839649134, 0, 43745429172,  166272457228, 1453644, 11160716896, 0, 2480726862,  41264702310, 0, 46678932238, 23240568, 137402579444 },
  { "KQBNKBB", 382561435109, 7084896, 75786194061, 187639146, 245094516,  187496423450, 73882, 4706032802, 163984, 27601706,  217492810, 187475162, 71080161259, 7011014, 195065011659 },
  { "KQBNKBN", 402599722985, 18891279, 71482746510, 61294, 38606252,  188764969846, 3587313, 3454989921, 95, 6748649,  31857603, 61199, 68027756589, 15303966, 213834753139 },
  { "KQBNKNN", 420530024100, 37420454, 66770586926, 0, 21909064,  189238888176, 5699924, 2982126198, 0, 3581526,  18327538, 0, 63788460728, 31720530, 231291135924 },
  { "KQBNKQB", 172626701098, 4652548550, 143634984509, 0, 79741993527,  132800871161, 2589967368, 48369145195, 0, 8470312100,  71271681427, 0, 95265839314, 2062581182, 39825829937 },
  { "KQBNKQN", 194341735743, 5447683191, 139700868049, 0, 73354027921,  144902131807, 2815533427, 39168270812, 0, 5344359778,  68009668143, 0, 100532597237, 2632149764, 49439603936 },
  { "KQBNKQQ", 69519089900, 166548, 64064478628, 173820, 218011717592,  62487815478, 139456, 47404894125, 105026, 82337341739,  135674375853, 68794, 16659584503, 27092, 7031274422 },
  { "KQBNKQR", 135020952648, 144700, 118050868220, 525154, 129676991434,  109736572433, 103628, 56629149912, 140906, 25864328945,  103812662489, 384248, 61421718308, 41072, 25284380215 },
  { "KQBNKRB", 349433234278, 0, 60809653151, 13774567561, 16811438422,  183986563468, 0, 7022418016, 752059638, 469254702,  16342183720, 13022507923, 53787235135, 0, 165446670810 },
  { "KQBNKRN", 367612368631, 25190949, 74468661616, 1914358152, 8631355860,  186649484693, 5833028, 5386559380, 19605579, 168813144,  8462542716, 1894752573, 69082102236, 19357921, 180962883938 },
  { "KQBNKRR", 302876467092, 1990929606, 59149248373, 0, 53583105313,  177030493661, 231260205, 11614596383, 0, 3353945575,  50229159738, 0, 47534651990, 1759669401, 125845973431 },
  { "KQNNKBB", 388624115098, 7642156, 78579265268, 1670350092, 767560230,  197396615692, 121252, 5645683512, 20382612, 28977872,  738582358, 1649967480, 72933581756, 7520904, 191227499406 },
  { "KQNNKBN", 408129922980, 0, 76825341216, 254179, 45995061,  198814349161, 0, 4271536557, 526, 5894696,  40100365, 253653, 72553804659, 0, 209315573819 },
  { "KQNNKNN", 425463052034, 3528, 72731511636, 0, 26858462,  199370745596, 8, 3716944948, 0, 4090388,  22768074, 0, 69014566688, 3520, 226092306438 },
  { "KQNNKQB", 143109830543, 3201538860, 180171137538, 2646638, 85032559221,  117413507140, 2036010258, 74947864221, 1025966, 8693373355,  76339185866, 1620672, 105223273317, 1165528602, 25696323403 },
  { "KQNNKQN", 161469381350, 1761136927, 181810689024, 2245084, 78662347635,  129284929264, 1046338181, 67024902444, 562708, 5735048343,  72927299292, 1682376, 114785786580, 714798746, 32184452086 },
  { "KQNNKQQ", 43306659860, 2559784, 97199413186, 149180646, 221799298128,  40288845766, 2535476, 78704339542, 96745976, 83999314180,  137799983948, 52434670, 18495073644, 24308, 3017814094 },
  { "KQNNKQR", 110171837278, 1334726, 140007347010, 5799924, 143424648334,  94075861218, 1236478, 78876279666, 2776830, 30135626748,  113289021586, 3023094, 61131067344, 98248, 16095976060 },
  { "KQNNKRB", 352666963529, 0, 48133762882, 24376662261, 26512989856,  192812610783, 0, 8184924225, 1338682770, 755563162,  25757426694, 23037979491, 39948838657, 0, 159854352746 },
  { "KQNNKRN", 370167232565, 0, 75977506269, 2963045131, 14405636359,  195852036806, 0, 6912858516, 40080316, 286805302,  14118831057, 2922964815, 69064647753, 0, 174315195759 },
  { "KQNNKRR", 292354682380, 3974916912, 68433383738, 0, 63698252470,  181002429698, 668279986, 17022998806, 0, 4398072450,  59300180020, 0, 51410384932, 3306636926, 111352252682 },
  { "KQQBKBB", 403245514538, 7781420, 974758154, 0, 56787684,  137715119940, 6976, 2064772, 0, 10498204,  46289480, 0, 972693382, 7774444, 265530394598 },
  { "KQQBKBN", 418942504176, 0, 680301806, 0, 14616406,  137724146074, 0, 576430, 0, 2967388,  11649018, 0, 679725376, 0, 281218358102 },
  { "KQQBKNN", 432326548308, 14172, 520870796, 0, 9901336,  137725316664, 4, 697292, 0, 1675932,  8225404, 0, 520173504, 14168, 294601231644 },
  { "KQQBKQB", 268483714854, 18124, 71403288169, 0, 6266600605,  130985611004, 1986, 6139817212, 0, 602259690,  5664340915, 0, 65263470957, 16138, 137498103850 },
  { "KQQBKQN", 287099444165, 1790, 67451274416, 0, 3790988601,  133570215093, 270, 3848451932, 0, 309022597,  3481966004, 0, 63602822484, 1520, 153529229072 },
  { "KQQBKQQ", 146294545742, 0, 60667964896, 105512, 90130404406,  94854284820, 0, 23405943314, 13540, 19467448218,  70662956188, 91972, 37262021582, 0, 51440260922 },
  { "KQQBKQR", 225023819639, 628, 76432051889, 1068902, 26789935166,  121581778961, 264, 13021980644, 127400, 3123802623,  23666132543, 941502, 63410071245, 364, 103442040678 },
  { "KQQBKRB", 383350905172, 0, 2095779622, 0, 879602686,  137631291432, 0, 12308160, 0, 84090300,  795512386, 0, 2083471462, 0, 245719613740 },
  { "KQQBKRN", 396189740655, 0, 1389911655, 0, 569676966,  137673015814, 0, 9911206, 0, 44762872,  524914094, 0, 1380000449, 0, 258516724841 },
  { "KQQBKRR", 354086347214, 28424824, 5837079504, 0, 3145292910,  137170111056, 236428, 176396974, 0, 380945434,  2764347476, 0, 5660682530, 28188396, 216916236158 },
  { "KQQNKBB", 412757357660, 9041312, 868405276, 100631804, 104034260,  147270618384, 21932, 1682920, 31988, 9963184,  94071076, 100599816, 866722356, 9019380, 265486739276 },
  { "KQQNKBN", 428440192397, 0, 736699520, 30732, 15128255,  147279134688, 0, 542750, 6, 2640964,  12487291, 30726, 736156770, 0, 281161057709 },
  { "KQQNKNN", 441808666614, 8904, 591545420, 0, 11742190,  147279569476, 0, 835954, 0, 1912978,  9829212, 0, 590709466, 8904, 294529097138 },
  { "KQQNKQB", 267740261762, 17426, 81153858581, 0, 6814112499,  139298298037, 1852, 7329263682, 0, 654754837,  6159357662, 0, 73824594899, 15574, 128441963725 },
  { "KQQNKQN", 286548987966, 194, 77061993781, 0, 4285355547,  142139361420, 48, 4787400595, 0, 355556345,  3929799202, 0, 72274593186, 146, 144409626546 },
  { "KQQNKQQ", 140990867808, 194000, 63886373468, 580, 101770213216,  97976075850, 98668, 26649468702, 264, 22656674924,  79113538292, 316, 37236904766, 95332, 43014791958 },
  { "KQQNKQR", 220386871352, 1282, 81276974538, 1398376, 36136259192,  127515991556, 530, 15380177010, 307016, 4385842296,  31750416896, 1091360, 65896797528, 752, 92870879796 },
  { "KQQNKRB", 392513792026, 0, 2291600476, 0, 1075523494,  147173818671, 0, 14805797, 0, 93693940,  981829554, 0, 2276794679, 0, 245339973355 },
  { "KQQNKRN", 405389700323, 0, 1531786533, 0, 782470936,  147213552656, 0, 13171612, 0, 55594140,  726876796, 0, 1518614921, 0, 258176147667 },
  { "KQQNKRR", 349773367972, 4266755826, 14201406226, 0, 4410242944,  146249953952, 117511130, 424116858, 0, 490736468,  3919506476, 0, 13777289368, 4149244696, 203523414020 },
  { "KQQQKBB", 369104004648, 1842096, 3758616, 0, 20331768,  102569082240, 420, 151752, 0, 3550812,  16780956, 0, 3606864, 1841676, 266534922408 },
  { "KQQQKBN", 384473948472, 0, 3155976, 0, 5413272,  102571513746, 0, 103104, 0, 1168374,  4244898, 0, 3052872, 0, 281902434726 },
  { "KQQQKNN", 397690457352, 6780, 6854904, 0, 5110908,  102571686648, 0, 247572, 0, 851004,  4259904, 0, 6607332, 6780, 295118770704 },
  { "KQQQKQB", 304272511590, 30018, 4662919617, 0, 2063255859,  102125338434, 540, 171588210, 0, 275858040,  1787397819, 0, 4491331407, 29478, 202147173156 },
  { "KQQQKQN", 319063893843, 0, 2818450584, 0, 1304459877,  102343945902, 0, 84009531, 0, 144829791,  1159630086, 0, 2734441053, 0, 216719947941 },
  { "KQQQKQQ", 205129913190, 108684, 26043149340, 0, 30764944674,  90221700936, 43920, 5586240522, 0, 6764799846,  24000144828, 0, 20456908818, 64764, 114908212254 },
  { "KQQQKQR", 273443125497, 0, 10546499544, 815700, 9101530815,  100289468931, 0, 1042072062, 18972, 1241225259,  7860305556, 796728, 9504427482, 0, 173153656566 },
  { "KQQQKRB", 350826124896, 0, 122519664, 0, 222738252,  102522157218, 0, 1803474, 0, 48824532,  173913720, 0, 120716190, 0, 248303967678 },
  { "KQQQKRN", 362779364796, 0, 61635990, 0, 153423822,  102546784428, 0, 2499072, 0, 23501724,  129922098, 0, 59136918, 0, 260232580368 },
  { "KQQQKRR", 326227138020, 0, 687157122, 0, 1027944642,  102347177784, 0, 31889970, 0, 193717470,  834227172, 0, 655267152, 0, 223879960236 },
  { "KQQRKBB", 390097822730, 3697892, 633912194, 0, 32145084,  124203778352, 3740, 846620, 0, 5797284,  26347800, 0, 633065574, 3694152, 265894044378 },
  { "KQQRKBN", 405691481840, 0, 419531604, 0, 9145048,  124208266508, 0, 258720, 0, 1900768,  7244280, 0, 419272884, 0, 281483215332 },
  { "KQQRKNN", 418997830798, 7500, 333425624, 0, 8806794,  124208516496, 0, 421594, 0, 1487906,  7318888, 0, 333004030, 7500, 294789314302 },
  { "KQQRKQB", 307069153812, 855214, 20813897138, 0, 4752451692,  122826314744, 54196, 940326807, 0, 443730249,  4308721443, 0, 19873570331, 801018, 184242839068 },
  { "KQQRKQN", 325665706630, 347926, 16172915572, 0, 2985474948,  123603040809, 10248, 363546979, 0, 243827960,  2741646988, 0, 15809368593, 337678, 202062665821 },
  { "KQQRKQQ", 188643118130, 1881412, 32450107944, 1940, 62480647234,  101987026072, 345116, 8095581380, 744, 14127472684,  48353174550, 1196, 24354526564, 1536296, 86656092058 },
  { "KQQRKQR", 269305491672, 1269668, 23821241253, 649344, 21600960391,  118118762799, 220432, 3518867367, 52134, 2572523264,  19028437127, 597210, 20302373886, 1049236, 151186728873 },
  { "KQQRKRB", 370653230439, 0, 1811171873, 3020, 344618252,  124131863548, 0, 9052094, 0, 69510354,  275107898, 3020, 1802119779, 0, 246521366891 },
  { "KQQRKRN", 383286411099, 0, 1086601770, 0, 259052511,  124164605448, 0, 7365772, 0, 38454776,  220597735, 0, 1079235998, 0, 259121805651 },
  { "KQQRKRR", 343356348150, 0, 3709483734, 0, 2514048672,  123759355256, 0, 128078620, 0, 322992120,  2191056552, 0, 3581405114, 0, 219596992894 },
  { "KQRBKBB", 404165225678, 70956622, 28470977228, 0, 88259498,  165612362642, 3854474, 605155172, 0, 16894834,  71364664, 0, 27865822056, 67102148, 238552863036 },
  { "KQRBKBN", 431497630613, 6409445142, 10217109929, 0, 23813934,  166079749203, 53778675, 99947020, 0, 4792224,  19021710, 0, 10117162909, 6355666467, 265417881410 },
  { "KQRBKNN", 450175160995, 9194272035, 1981582838, 0, 16895974,  166139510512, 80952598, 14899568, 0, 2904444,  13991530, 0, 1966683270, 9113319437, 284035650483 },
  { "KQRBKQB", 255057420229, 476792, 66012401214, 0, 53593900747,  150311523665, 106250, 11802200947, 0, 4124436260,  49469464487, 0, 54210200267, 370542, 104745896564 },
  { "KQRBKQN", 278390151514, 316056, 60107946481, 0, 48353872151,  155699456240, 56402, 8056825763, 0, 2481928717,  45871943434, 0, 52051120718, 259654, 122690695274 },
  { "KQRBKQQ", 126709477461, 695088, 54830957268, 46460, 144062421509,  95723565986, 284866, 30658654806, 11534, 39855749930,  104206671579, 34926, 24172302462, 410222, 30985911475 },
  { "KQRBKQR", 191292788888, 12312305, 78056352050, 539417, 87395460794,  127752652246, 5949343, 24741612111, 103556, 13737949866,  73657510928, 435861, 53314739939, 6362962, 63540136642 },
  { "KQRBKRB", 359282609785, 224886, 54131171732, 9682, 1422848625,  163137372797, 69276, 2977164093, 378, 123660578,  1299188047, 9304, 51154007639, 155610, 196145236988 },
  { "KQRBKRN", 376590535852, 215057, 49149486745, 0, 919668852,  164178322977, 43707, 1986060854, 0, 73839584,  845829268, 0, 47163425891, 171350, 212412212875 },
  { "KQRBKRR", 301166687907, 12086846, 83304545095, 4472, 7124397362,  155492238301, 680358, 10086892433, 138, 658455892,  6465941470, 4334, 73217652662, 11406488, 145674449606 },
  { "KQRNKBB", 410192330257, 11906701464, 19761675965, 174458388, 188027494,  174999547438, 223172216, 427409198, 137546, 15775266,  172252228, 174320842, 19334266767, 11683529248, 235192782819 },
  { "KQRNKBN", 432073207988, 1843076287, 23633488133, 67120, 25934632,  175334743745, 50945634, 276099653, 265, 4252367,  21682265, 66855, 23357388480, 1792130653, 256738464243 },
  { "KQRNKNN", 451624307313, 1963043313, 17189131060, 0, 19204698,  175427355117, 46842482, 188654065, 0, 3190000,  16014698, 0, 17000476995, 1916200831, 276196952196 },
  { "KQRNKQB", 251834839004, 604168, 71614126394, 0, 60642403958,  156971427848, 163821, 13726869133, 0, 4967580862,  55674823096, 0, 57887257261, 440347, 94863411156 },
  { "KQRNKQN", 275330285157, 398349, 65753204469, 0, 55196172769,  162867048432, 92162, 9620413929, 0, 3178487141,  52017685628, 0, 56132790540, 306187, 112463236725 },
  { "KQRNKQQ", 116634119544, 838550, 60236794850, 73188, 158159546196,  93322116036, 438722, 36149231788, 40798, 46194214320,  111965331876, 32390, 24087563062, 399828, 23312003508 },
  { "KQRNKQR", 186454720228, 6544294, 74353657261, 486247, 105369819966,  131372026692, 3264281, 25498400744, 125397, 18792224550,  86577595416, 360850, 48855256517, 3280013, 55082693536 },
  { "KQRNKRB", 362740465743, 0, 59812113418, 14163, 1712045928,  172052501789, 0, 3476087801, 310, 137451764,  1574594164, 13853, 56336025617, 0, 190687963954 },
  { "KQRNKRN", 380556145827, 0, 54304292055, 0, 1227243166,  173216955874, 0, 2359553398, 0, 89532392,  1137710774, 0, 51944738657, 0, 207339189953 },
  { "KQRNKRR", 297355300571, 1794745783, 87065848361, 557568, 14819043941,  162801537140, 115862017, 11741262559, 15440, 1007364508,  13811679433, 542128, 75324585802, 1678883766, 134553763431 },
  { "KQRRKBB", 412356473330, 3059824, 2639676948, 0, 49287008,  148472616896, 4824, 9543486, 0, 9180000,  40107008, 0, 2630133462, 3055000, 263883856434 },
  { "KQRRKBN", 428755760318, 0, 1630533898, 0, 14783486,  148484702587, 0, 3633037, 0, 3009582,  11773904, 0, 1626900861, 0, 280271057731 },
  { "KQRRKNN", 442542089786, 5208, 1064826184, 0, 14068748,  148486216046, 0, 2722412, 0, 2406748,  11662000, 0, 1062103772, 5208, 294055873740 },
  { "KQRRKQB", 280241476261, 1125682, 43410145395, 59658138, 33204871590,  141043635297, 203324, 5454767755, 3708763, 1989030067,  31215841523, 55949375, 37955377640, 922358, 139197840964 },
  { "KQRRKQN", 304560050288, 472022, 45309758456, 3239754956, 15995328564,  144227516212, 53772, 3493318362, 103811613, 666645247,  15328683317, 3135943343, 41816440094, 418250, 160332534076 },
  { "KQRRKQQ", 160930167668, 1202552, 31527921430, 0, 115397384220,  105629208700, 285732, 13203067434, 0, 29658783340,  85738600880, 0, 18324853996, 916820, 55300958968 },
  { "KQRRKQR", 229418640157, 811468, 43752515108, 395902, 65838168903,  128082942908, 222840, 10724474826, 64460, 9683640172,  56154528731, 331442, 33028040282, 588628, 101335697249 },
  { "KQRRKRB", 370049055861, 10276, 26514419437, 7892, 526449328,  147720423286, 688, 671679810, 12, 99241410,  427207918, 7880, 25842739627, 9588, 222328632575 },
  { "KQRRKRN", 391251179573, 631186, 17237623303, 0, 423550528,  148213979294, 8642, 215520594, 0, 61836676,  361713852, 0, 17022102709, 622544, 243037200279 },
  { "KQRRKRR", 334530134868, 0, 34408703190, 0, 4921961708,  146032869186, 0, 1929009882, 0, 529466138,  4392495570, 0, 32479693308, 0, 188497265682 },
  { "KRBBKBB", 353965246814, 120727814, 128870291468, 0, 231139136,  202783777446, 17734390, 13781910236, 0, 46831256,  184307880, 0, 115088381232, 102993424, 151181469368 },
  { "KRBBKBN", 404677520228, 14414916291, 79384285389, 0, 63263916,  209897045219, 677458013, 6043385724, 0, 12364372,  50899544, 0, 73340899665, 13737458278, 194780475009 },
  { "KRBBKNN", 437120367594, 23801023500, 50808410124, 0, 30096830,  212540049742, 942432028, 3142542952, 0, 5228606,  24868224, 0, 47665867172, 22858591472, 224580317852 },
  { "KRBBKQB", 96872746222, 0, 87590154493, 17828519516, 222764764957,  82818273984, 0, 60920961625, 11395833703, 61495184016,  161269580941, 6432685813, 26669192868, 0, 14054472238 },
  { "KRBBKQN", 111969910594, 3577478684, 88638889658, 3338262052, 229719731420,  94345213268, 2077656828, 58375375042, 1928872160, 59903136030,  169816595390, 1409389892, 30263514616, 1499821856, 17624697326 },
  { "KRBBKQQ", 6259256534, 0, 82643067046, 4953806456, 282139453956,  5784610330, 0, 74706353920, 4325525328, 131813763750,  150325690206, 628281128, 7936713126, 0, 474646204 },
  { "KRBBKQR", 54896675596, 1309554, 97720651249, 2183973519, 252346829742,  50036110824, 1194128, 74253208506, 1719447983, 90620291887,  161726537855, 464525536, 23467442743, 115426, 4860564772 },
  { "KRBBKRB", 207981795936, 349889556, 251080823274, 50228916, 5766113234,  150697601827, 220769538, 65407279068, 1710754, 302892141,  5463221093, 48518162, 185673544206, 129120018, 57284194109 },
  { "KRBBKRN", 245673637699, 5312732229, 207935920237, 12282175339, 5847427208,  167097545496, 1196280409, 47657142484, 482748559, 196536380,  5650890828, 11799426780, 160278777753, 4116451820, 78576092203 },
  { "KRBBKRR", 104581961406, 2904642, 284373626524, 8264, 53041207052,  90373923792, 2400236, 121510319164, 824, 4743609312,  48297597740, 7440, 162863307360, 504406, 14208037614 },
  { "KRBNKBB", 363868464613, 9636489222, 121177565785, 257929174, 368960150,  214888206674, 707420214, 13112403152, 662946, 43564054,  325396096, 257266228, 108065162633, 8929069008, 148980257939 },
  { "KRBNKBN", 401807570538, 6608476286, 102185115884, 98665, 60728163,  220543319927, 435729764, 7762508990, 729, 10697630,  50030533, 97936, 94422606894, 6172746522, 181264250611 },
  { "KRBNKNN", 430923331100, 8579619288, 84343436232, 0, 35515140,  223092712878, 298343532, 5355256444, 0, 5944186,  29570954, 0, 78988179788, 8281275756, 207830618222 },
  { "KRBNKQB", 93342076139, 4059145, 88297484536, 1655014032, 253879555048,  82578769322, 3763251, 64370061348, 1048345775, 80751317344,  173128237704, 606668257, 23927423188, 295894, 10763306817 },
  { "KRBNKQN", 104174295390, 35765764, 68162241258, 29312796465, 247681177243,  91421711080, 29700422, 49153557962, 18111210425, 70036077151,  177645100092, 11201586040, 19008683296, 6065342, 12752584310 },
  { "KRBNKQQ", 5397279800, 8808244, 71980986134, 0, 310730513526,  5122037655, 8622826, 66903799072, 0, 156717797487,  154012716039, 0, 5077187062, 185418, 275242145 },
  { "KRBNKQR", 51592324693, 4760692, 89778558842, 0, 277895799145,  48216546143, 4519861, 71847321493, 0, 108683869543,  169211929602, 0, 17931237349, 240831, 3375778550 },
  { "KRBNKRB", 202764356521, 51086539077, 189872655444, 15113270904, 18514032682,  155950754468, 26621078322, 43814149694, 1464452729, 901821827,  17612210855, 13648818175, 146058505750, 24465460755, 46813602053 },
  { "KRBNKRN", 237224993292, 56964246083, 183550192470, 1955054802, 9479409777,  173921980707, 24706022696, 29783069943, 40271251, 300912443,  9178497334, 1914783551, 153767122527, 32258223387, 63303012585 },
  { "KRBNKRR", 96652307213, 1252885, 287824868821, 602834, 69642679847,  86681828280, 1080039, 135231569794, 64708, 6837714219,  62804965628, 538126, 152593299027, 172846, 9970478933 },
  { "KRNNKBB", 365888501450, 20702913812, 116339795286, 1874321012, 949875012,  222089919070, 2670881716, 14336558542, 51216940, 49678400,  900196612, 1823104072, 102003236744, 18032032096, 143798582380 },
  { "KRNNKBN", 399674904553, 3155193227, 118208338782, 328835, 69221767,  228561012957, 363875895, 10263842500, 2238, 9521078,  59700689, 326597, 107944496282, 2791317332, 171113891596 },
  { "KRNNKNN", 430118142522, 3040223514, 101127482858, 0, 42050494,  232294221626, 152493474, 6744932790, 0, 6606778,  35443716, 0, 94382550068, 2887730040, 197823920896 },
  { "KRNNKQB", 83555606592, 0, 100500146296, 18392966, 263550040674,  75889240541, 0, 75624594399, 12333882, 87672085846,  175877954828, 6059084, 24875551897, 0, 7666366051 },
  { "KRNNKQN", 92612222210, 0, 120059174751, 91427818, 247049448969,  83591360407, 0, 85088830358, 52506385, 70465557518,  176583891451, 38921433, 34970344393, 0, 9020861803 },
  { "KRNNKQQ", 2564454796, 0, 76120357658, 17733170, 319861039708,  2395087422, 0, 71717790298, 16374722, 165069002226,  154792037482, 1358448, 4402567360, 0, 169367374 },
  { "KRNNKQR", 40154726924, 0, 96357757091, 233135308, 292971821677,  38250891275, 0, 81225104910, 172238511, 119550019972,  173421801705, 60896797, 15132652181, 0, 1903835649 },
  { "KRNNKRB", 148149759530, 67656780595, 216928468966, 26361463087, 28700380078,  124724802117, 42338408544, 68233419250, 2463565841, 1438058916,  27262321162, 23897897246, 148695049716, 25318372051, 23424957413 },
  { "KRNNKRN", 189753866562, 82083224999, 209205495116, 3035063126, 15542244249,  150918233425, 42862111048, 44827059564, 78357106, 512493525,  15029750724, 2956706020, 164378435552, 39221113951, 38835633137 },
  { "KRNNKRR", 78055509156, 0, 299365618968, 1247052, 87145334052,  72289390054, 0, 157108624634, 182984, 9800056996,  77345277056, 1064068, 142256994334, 0, 5766119102 },
  { "KRRBKBB", 408449844350, 138441132, 56042080000, 0, 133397804,  195231878178, 18066072, 2930166152, 0, 26500980,  106896824, 0, 53111913848, 120375060, 213217966172 },
  { "KRRBKBN", 447725351372, 12551443230, 19802060698, 0, 37488578,  197414497990, 320731910, 463846464, 0, 7535018,  29953560, 0, 19338214234, 12230711320, 250310853382 },
  { "KRRBKNN", 471707732864, 17887113802, 3714524336, 0, 26885100,  197862130254, 288301988, 51494592, 0, 4684548,  22200552, 0, 3663029744, 17598811814, 273845602610 },
  { "KRRBKQB", 158047805288, 8297521, 109411675317, 59998455, 139104766661,  122665219534, 6293860, 53084451219, 14944854, 22435701915,  116669064746, 45053601, 56327224098, 2003661, 35382585754 },
  { "KRRBKQN", 176798613363, 11942040, 124646083344, 3022472349, 114341519366,  133517865894, 7740605, 50972297597, 503778966, 13204928320,  101136591046, 2518693383, 73673785747, 4201435, 43280747469 },
  { "KRRBKQQ", 52619581002, 19291408, 92900986372, 0, 212032083264,  47822682874, 17188266, 72976737846, 0, 77390002396,  134642080868, 0, 19924248526, 2103142, 4796898128 },
  { "KRRBKQR", 104615894607, 10174067, 108845070765, 13737751, 175240920524,  88406526267, 8418418, 65411690624, 6809674, 44373166399,  130867754125, 6928077, 43433380141, 1755649, 16209368340 },
  { "KRRBKRB", 322043850930, 465487, 122713590339, 22234, 2047279980,  186164580082, 185173, 11859895205, 1060, 181949862,  1865330118, 21174, 110853695134, 280314, 135879270848 },
  { "KRRBKRN", 350245413724, 970984, 107011415111, 0, 1370450947,  190630582199, 168626, 7456780907, 0, 119079650,  1251371297, 0, 99554634204, 802358, 159614831525 },
  { "KRRBKRR", 224086160932, 4854274928, 182316537456, 8328, 12319084298,  157532358332, 1936647116, 37603242200, 356, 1134363378,  11184720920, 7972, 144713295256, 2917627812, 66553802600 },
  { "KRRNKBB", 411647588216, 23235244586, 38484872150, 244892532, 279978428,  204331810284, 1231602588, 1747438308, 286692, 24286136,  255692292, 244605840, 36737433842, 22003641998, 207315777932 },
  { "KRRNKBN", 439791584796, 3626159865, 45786802264, 106978, 40502601,  205837944876, 203915100, 1286908050, 1286, 6654696,  33847905, 105692, 44499894214, 3422244765, 233953639920 },
  { "KRRNKNN", 465556712584, 3803416302, 33075369420, 0, 29570422,  206590648734, 136493592, 603293064, 0, 4988618,  24581804, 0, 32472076356, 3666922710, 258966063850 },
  { "KRRNKQB", 147873806044, 8046198, 106992881733, 89544775, 160797077118,  119672731831, 6939820, 57146341934, 35106058, 30474304365,  130322772753, 54438717, 49846539799, 1106378, 28201074213 },
  { "KRRNKQN", 167245846309, 12227908, 126192960834, 3446335628, 131052072409,  131632055776, 9039632, 57405737083, 798565700, 17490025817,  113562046592, 2647769928, 68787223751, 3188276, 35613790533 },
  { "KRRNKQQ", 44154349052, 18642280, 84317642638, 0, 238210120702,  41097293796, 17963756, 69836615638, 0, 96383550818,  141826569884, 0, 14481027000, 678524, 3057055256 },
  { "KRRNKQR", 94693404030, 8387114, 107047772273, 3829303, 196101217620,  82806609049, 7813132, 69286880579, 2018021, 55232103227,  140869114393, 1811282, 37760891694, 573982, 11886794981 },
  { "KRRNKRB", 320398248820, 8732, 133100082121, 30320, 2435651603,  193278591383, 1928, 13854524045, 932, 202305720,  2233345883, 29388, 119245558076, 6804, 127119657437 },
  { "KRRNKRN", 350072532162, 482410, 115904109986, 0, 1779938834,  198519074106, 38978, 8674957092, 0, 141353832,  1638585002, 0, 107229152894, 443432, 151553458056 },
  { "KRRNKRR", 160933852332, 38263694798, 207018085544, 1126184, 26488119710,  128286295230, 21578030118, 55396649454, 62576, 2074386630,  24413733080, 1063608, 151621436090, 16685664680, 32647557102 },
  { "KRRRKBB", 436468843506, 0, 5668623990, 0, 73872792,  175593820578, 0, 46158186, 0, 14209620,  59663172, 0, 5622465804, 0, 260875022928 },
  { "KRRRKBN", 454058937258, 0, 3481905876, 0, 23077746,  175632539994, 0, 16998624, 0, 4649766,  18427980, 0, 3464907252, 0, 278426397264 },
  { "KRRRKNN", 468606885930, 0, 2155644924, 0, 21302250,  175640441526, 0, 10059576, 0, 3687282,  17614968, 0, 2145585348, 0, 292966444404 },
  { "KRRRKQB", 211408614150, 2524755819, 77879044320, 168885327, 92098820628,  140813914728, 1126669488, 23168220507, 24418353, 10520965308,  81577855320, 144466974, 54710823813, 1398086331, 70594699422 },
  { "KRRRKQN", 244987550250, 204833664, 97647273366, 10106211858, 43322338326,  152441262810, 75112650, 19239781830, 900608724, 2997422370,  40324915956, 9205603134, 78407491536, 129721014, 92546287440 },
  { "KRRRKQQ", 85645647498, 1218180, 81878027868, 0, 167494625502,  69807024282, 988836, 54707849622, 0, 51138325644,  116356299858, 0, 27170178246, 229344, 15838623216 },
  { "KRRRKQR", 172874987361, 362070, 57104401164, 947706, 136192676415,  121983442179, 271014, 24239882577, 383547, 29430209067,  106762467348, 564159, 32864518587, 91056, 50891545182 },
  { "KRRRKRB", 353453456802, 32328, 70004568855, 14754, 794713233,  172170176118, 5172, 3341569806, 90, 142437198,  652276035, 14664, 66662999049, 27156, 181283280684 },
  { "KRRRKRN", 389078673105, 1724229, 46321083918, 0, 674346516,  174366791529, 40044, 1189515183, 0, 97841628,  576504888, 0, 45131568735, 1684185, 214711881576 },
  { "KRRRKRR", 304112012106, 0, 88474448976, 0, 8437181862,  166449679524, 0, 8342614902, 0, 861893958,  7575287904, 0, 80131834074, 0, 137662332582 },
  { "KBBBBKP", 1325982895104, 3318552, 283636919784, 596775048, 5912829192,  513000965184, 4224, 80200412520, 125184, 128829288,  5783999904, 596649864, 203436507264, 3314328, 812981929920 },
  { "KBBBNKP", 1584462558420, 9536453166, 62555887458, 657857292, 4874598312,  635593996206, 106981032, 3569157162, 191754, 14627214,  4859971098, 657665538, 58986730296, 9429472134, 948868562214 },
  { "KBBBPKB", 1264658414364, 419670, 328373062542, 0, 123405840,  596221342986, 163062, 75102034584, 0, 25456872,  97948968, 0, 253271027958, 256608, 668437071378 },
  { "KBBBPKN", 1528587440082, 485384310, 108145992618, 0, 10075614,  655352020320, 5615982, 15989743938, 0, 1617264,  8458350, 0, 92156248680, 479768328, 873235419762 },
  { "KBBBPKP", 1182746383230, 131295648, 69551666460, 768618972, 13721974782,  495295929162, 6692640, 7839322446, 951120, 388249644,  13333725138, 767667852, 61712344014, 124603008, 687450454068 },
  { "KBBBPKQ", 466814281554, 1591841424, 490742623644, 20978596098, 412522916880,  367382099778, 981970980, 234100806930, 4597073844, 64287045972,  348235870908, 16381522254, 256641816714, 609870444, 99432181776 },
  { "KBBBPKR", 1151120909328, 254967204, 369948806250, 0, 1729246146,  596627465328, 33748872, 74443572894, 0, 244210410,  1485035736, 0, 295505233356, 221218332, 554493444000 },
  { "KBBNNKP", 1632785147780, 104726484, 63614811320, 430732244, 6070033380,  675404154416, 2121560, 4777526568, 303736, 18943648,  6051089732, 430428508, 58837284752, 102604924, 957380993364 },
  { "KBBNPKB", 1549707735356, 0, 88992811726, 0, 83942302,  710767738152, 0, 6192792576, 0, 17653744,  66288558, 0, 82800019150, 0, 838939997204 },
  { "KBBNPKN", 1663621051374, 174813084, 19049261952, 0, 12953182,  716806891390, 487304, 168762972, 0, 2042806,  10910376, 0, 18880498980, 174325780, 946814159984 },
  { "KBBNPKP", 1241020493430, 102624686, 43956438380, 736350288, 14511257176,  532581544512, 6689026, 4219657442, 5224912, 125253988,  14386003188, 731125376, 39736780938, 95935660, 708438948918 },
  { "KBBNPKQ", 506802820714, 900892110, 462975616944, 14803848966, 452796267834,  408441498434, 637089226, 228738214088, 4930898282, 74230484442,  378565783392, 9872950684, 234237402856, 263802884, 98361322280 },
  { "KBBNPKR", 1319517257372, 61647830, 246792206234, 0, 2312004460,  694658109778, 6241276, 21993330058, 0, 320503360,  1991501100, 0, 224798876176, 55406554, 624859147594 },
  { "KBBPPKB", 1082614438088, 1960, 171202714104, 0, 103750096,  528962471232, 416, 36000694168, 0, 21586328,  82163768, 0, 135202019936, 1544, 553651966856 },
  { "KBBPPKN", 1250402715664, 225652940, 35478709348, 0, 12789440,  562854482408, 3801424, 2124518632, 0, 1949680,  10839760, 0, 33354190716, 221851516, 687548233256 },
  { "KBBPPKP", 926688597580, 4160552, 29019580180, 603518444, 31053605404,  414395486088, 590908, 3509386904, 12140444, 2804684632,  28248920772, 591378000, 25510193276, 3569644, 512293111492 },
  { "KBBPPKQ", 321378413280, 28034224, 234846389048, 10274474488, 538436549792,  274248479584, 16225092, 126963922336, 4748368732, 159007756400,  379428793392, 5526105756, 107882466712, 11809132, 47129933696 },
  { "KBBPPKR", 916430259228, 231133604, 280980817720, 0, 3681936256,  518713934956, 77535944, 45867527168, 0, 325754076,  3356182180, 0, 235113290552, 153597660, 397716324272 },
  { "KBNNNKP", 1666059333402, 10882455474, 54668919264, 3627888, 6976869588,  713025506358, 1133508948, 1595947140, 21666, 33820224,  6943049364, 3606222, 53072972124, 9748946526, 953033827044 },
  { "KBNNPKB", 1642054899444, 923656, 36810953324, 0, 48080086,  756588998808, 3654, 509231186, 0, 10317950,  37762136, 0, 36301722138, 920002, 885465900636 },
  { "KBNNPKN", 1689012437870, 13533214, 33946483932, 0, 15991702,  756701947990, 198802, 403941744, 0, 2463062,  13528640, 0, 33542542188, 13334412, 932310489880 },
  { "KBNNPKP", 1262806231142, 455763342, 49920750662, 325268590, 16173187074,  561499445960, 70896748, 4478042008, 7777940, 236244074,  15936943000, 317490650, 45442708654, 384866594, 701306785182 },
  { "KBNNPKQ", 507604688538, 1463760972, 493925114848, 4355724848, 471060524488,  422257343836, 1049012512, 241109128940, 1826966686, 90866099624,  380194424864, 2528758162, 252815985908, 414748460, 85347344702 },
  { "KBNNPKR", 1206550095662, 4978784, 399236802850, 0, 3021605726,  706543683506, 1230006, 50149170730, 0, 414467356,  2607138370, 0, 349087632120, 3748778, 500006412156 },
  { "KBNPPKB", 1239227423944, 0, 47586632000, 0, 69397708,  596575004440, 0, 1357541926, 0, 14755182,  54642526, 0, 46229090074, 0, 642652419504 },
  { "KBNPPKN", 1295009020904, 6879366, 24049833672, 0, 16682854,  597649734062, 204980, 294902474, 0, 2460032,  14222822, 0, 23754931198, 6674386, 697359286842 },
  { "KBNPPKP", 940976754606, 1768792, 37299474064, 647958388, 32407979638,  435427065782, 285698, 7110049550, 110050402, 2039310872,  30368668766, 537907986, 30189424514, 1483094, 505549688824 },
  { "KBNPPKQ", 317603290124, 10279364, 238757631786, 4168130100, 577387078862,  277590875712, 6532462, 138303193808, 2707591068, 179339108498,  398047970364, 1460539032, 100454437978, 3746902, 40012414412 },
  { "KBNPPKR", 984910616166, 15486346, 244079417090, 0, 5281176610,  566399372646, 7642650, 31090047160, 0, 450239092,  4830937518, 0, 212989369930, 7843696, 418511243520 },
  { "KBPPPKB", 894531402744, 0, 86196551790, 0, 89348952,  458210026236, 0, 10496979360, 0, 19098870,  70250082, 0, 75699572430, 0, 436321376508 },
  { "KBPPPKN", 973010221080, 10998, 31154982426, 0, 22272594,  467137738404, 420, 1585563552, 0, 2802090,  19470504, 0, 29569418874, 10578, 505872482676 },
  { "KBPPPKP", 702264813144, 2716152, 14712516132, 1069914, 47750889294,  336676337532, 457224, 2465008518, 227454, 7374243588,  40376645706, 842460, 12247507614, 2258928, 365588475612 },
  { "KBPPPKQ", 222077896344, 10534284, 96482633868, 6148386, 552119117256,  203688947388, 7488420, 51143001726, 3393438, 213883273494,  338235843762, 2754948, 45339632142, 3045864, 18388948956 },
  { "KBPPPKR", 637911744450, 2347494, 258746596842, 0, 44821559244,  392605637028, 1086006, 73419214164, 0, 2700167268,  42121391976, 0, 185327382678, 1261488, 245306107422 },
  { "KNNNNKP", 1677977085480, 620090592, 84976888464, 5373792, 5065876584,  741665172696, 12339000, 4119124992, 43224, 46233720,  5019642864, 5330568, 80857763472, 607751592, 936311912784 },
  { "KNNNPKB", 1626011790540, 119312982, 87132403242, 0, 20495400,  788154185724, 4180476, 3314705970, 0, 4625082,  15870318, 0, 83817697272, 115132506, 837857604816 },
  { "KNNNPKN", 1680345473676, 19740414, 76972842912, 0, 19535370,  790400843340, 1045446, 1072840050, 0, 2968416,  16566954, 0, 75900002862, 18694968, 889944630336 },
  { "KNNNPKP", 1264613725164, 870073350, 76643269752, 9411666, 12657123120,  582180100404, 89720328, 8889820356, 332526, 244835358,  12412287762, 9079140, 67753449396, 780353022, 682433624760 },
  { "KNNNPKQ", 485619135558, 1630194294, 639338486496, 170698404, 386020444596,  415372693650, 1206391734, 305186041428, 50894718, 69661675722,  316358768874, 119803686, 334152445068, 423802560, 70246441908 },
  { "KNNNPKR", 983357704992, 0, 655998742140, 0, 3826181544,  658662807198, 0, 132291064008, 0, 523826046,  3302355498, 0, 523707678132, 0, 324694897794 },
  { "KNNPPKB", 1242953474592, 1460692, 72454271120, 0, 38008508,  622665148952, 99500, 3837745104, 0, 8069252,  29939256, 0, 68616526016, 1361192, 620288325640 },
  { "KNNPPKN", 1279240576988, 1465656, 68381487672, 0, 22647740,  622795907368, 113128, 3711663028, 0, 3379284,  19268456, 0, 64669824644, 1352528, 656444669620 },
  { "KNNPPKP", 937901895812, 257483724, 64847243048, 8913356, 29063077000,  448968203316, 4780224, 14671609216, 1468960, 1785378040,  27277698960, 7444396, 50175633832, 252703500, 488933692496 },
  { "KNNPPKQ", 302156929960, 4942724, 336381177856, 91490688, 527855630268,  270846677700, 3344416, 198906554944, 47788008, 156706697740,  371148932528, 43702680, 137474622912, 1598308, 31310252260 },
  { "KNNPPKR", 764110575104, 1070232, 491780283232, 0, 6958528904,  504053526888, 742492, 121853648804, 0, 603144624,  6355384280, 0, 369926634428, 327740, 260057048216 },
  { "KNPPPKB", 933377307660, 0, 70856023266, 0, 62669418,  486335742420, 0, 5855553606, 0, 13505298,  49164120, 0, 65000469660, 0, 447041565240 },
  { "KNPPPKN", 981907821972, 2016, 45725298246, 0, 33061722,  489140216826, 144, 3059882988, 0, 4701366,  28360356, 0, 42665415258, 1872, 492767605146 },
  { "KNPPPKP", 700773528354, 2243676, 21775820148, 40128, 59124105378,  346588824588, 468654, 5135354274, 11316, 11735348532,  47388756846, 28812, 16640465874, 1775022, 354184703766 },
  { "KNPPPKQ", 214580674986, 6973680, 103610496300, 170718, 575976711312,  199710398568, 4988040, 59803705656, 101112, 232685607948,  343291103364, 69606, 43806790644, 1985640, 14870276418 },
  { "KNPPPKR", 584846845134, 130710, 330214358760, 0, 49899610284,  378375647490, 75996, 109790916642, 0, 4038161196,  45861449088, 0, 220423442118, 54714, 206471197644 },
  { "KPPPPKB", 663048391920, 0, 99049309272, 0, 86263464,  364601758416, 0, 19074218352, 0, 19175280,  67088184, 0, 79975090920, 0, 298446633504 },
  { "KPPPPKN", 717469540512, 0, 61475449680, 0, 85581696,  373432531728, 0, 10247357352, 0, 15262968,  70318728, 0, 51228092328, 0, 344037008784 },
  { "KPPPPKP", 514548413328, 4792560, 14573259000, 0, 59277513384,  263587913256, 1339320, 3570776232, 0, 14404648344,  44872865040, 0, 11002482768, 3453240, 250960500072 },
  { "KPPPPKQ", 146889174984, 11392248, 58491360840, 888, 475787219664,  142172701704, 9873528, 37810024560, 360, 203702551896,  272084667768, 528, 20681336280, 1518720, 4716473280 },
  { "KPPPPKR", 413251197888, 0, 120005090880, 0, 199604224848,  273999606720, 0, 48501925632, 0, 61193619696,  138410605152, 0, 71503165248, 0, 139251591168 },
  { "KQBBBKP", 1462201960518, 1334892, 25303230708, 126088410, 864770736,  465671810040, 432, 21274050, 1044, 1898418,  862872318, 126087366, 25281956658, 1334460, 996530150478 },
  { "KQBBNKP", 1518598746890, 493730, 5236506218, 110860440, 957770882,  502081276672, 254, 18684438, 7202, 2008314,  955762568, 110853238, 5217821780, 493476, 1016517470218 },
  { "KQBBPKB", 1411024246022, 0, 37248293390, 0, 39969412,  526371154142, 0, 127219290, 0, 7830480,  32138932, 0, 37121074100, 0, 884653091880 },
  { "KQBBPKN", 1480858409224, 152255878, 11370217926, 0, 5216004,  526416351000, 433370, 88573626, 0, 845916,  4370088, 0, 11281644300, 151822508, 954442058224 },
  { "KQBBPKP", 1152331900600, 1297902, 4232504794, 104605598, 2192526866,  395459544578, 410, 12237240, 5976, 2253476,  2190273390, 104599622, 4220267554, 1297492, 756872356022 },
  { "KQBBPKQ", 1068256443610, 67890910, 75385726694, 5858463546, 98238941248,  522120834572, 4199178, 3489918956, 28535988, 862715218,  97376226030, 5829927558, 71895807738, 63691732, 546135609038 },
  { "KQBBPKR", 1313532116870, 49889754, 63623427958, 0, 1005700754,  526150033258, 65212, 221248472, 0, 134856970,  870843784, 0, 63402179486, 49824542, 787382083612 },
  { "KQBNNKP", 1549517256896, 2095238, 6722270064, 451460, 1225941394,  534646490798, 194, 17037816, 26, 2084938,  1223856456, 451434, 6705232248, 2095044, 1014870766098 },
  { "KQBNPKB", 1478824598981, 0, 5661939374, 0, 23160423,  562694015309, 0, 4775181, 0, 4603376,  18557047, 0, 5657164193, 0, 916130583672 },
  { "KQBNPKN", 1524481966315, 4920136, 4089922481, 0, 6480054,  562679361027, 49939, 22955830, 0, 1027070,  5452984, 0, 4066966651, 4870197, 961802605288 },
  { "KQBNPKP", 1179330724978, 424176, 3321734266, 60222082, 2690329621,  422001427537, 77, 10824444, 907, 2388078,  2687941543, 60221175, 3310909822, 424099, 757329297441 },
  { "KQBNPKQ", 1090130273476, 20427655, 73285401622, 949135469, 119619417740,  557993135132, 2165536, 3669712348, 9610239, 1028770611,  118590647129, 939525230, 69615689274, 18262119, 532137138344 },
  { "KQBNPKR", 1361405245830, 2244661, 51659409953, 0, 1341424846,  562414525515, 10189, 113076485, 0, 175781677,  1165643169, 0, 51546333468, 2234472, 798990720315 },
  { "KQBPPKB", 1112300351808, 0, 20027180454, 0, 30273434,  443383586654, 0, 32277076, 0, 5789862,  24483572, 0, 19994903378, 0, 668916765154 },
  { "KQBPPKN", 1157724554024, 4920, 6826398194, 0, 5811702,  443373738008, 24, 46991510, 0, 924050,  4887652, 0, 6779406684, 4896, 714350816016 },
  { "KQBPPKP", 891559127338, 838914, 2313122544, 24202, 3417956030,  330634218418, 196, 7079116, 0, 2598114,  3415357916, 24202, 2306043428, 838718, 560924908920 },
  { "KQBPPKQ", 814110677612, 41084956, 58754767304, 406354, 110493826054,  437899268760, 5099158, 4422829484, 1286, 1094454904,  109399371150, 405068, 54331937820, 35985798, 376211408852 },
  { "KQBPPKR", 1018393582800, 426, 59333670400, 0, 2033794630,  443075488824, 0, 177808092, 0, 168356676,  1865437954, 0, 59155862308, 426, 575318093976 },
  { "KQNNNKP", 1570105964748, 166958628, 14800998486, 852720, 879964938,  563134059756, 714, 16203300, 96, 2074374,  877890564, 852624, 14784795186, 166957914, 1006971904992 },
  { "KQNNPKB", 1494157479714, 943382, 22344609234, 0, 9055112,  594674746278, 144, 29127108, 0, 1909000,  7146112, 0, 22315482126, 943238, 899482733436 },
  { "KQNNPKN", 1535129218570, 750950, 25448168042, 0, 7540088,  594478630084, 5556, 225988326, 0, 1158564,  6381524, 0, 25222179716, 745394, 940650588486 },
  { "KQNNPKP", 1197804268976, 142921378, 8709332616, 1514672, 2187107658,  445443910178, 284, 10023690, 52, 2417016,  2184690642, 1514620, 8699308926, 142921094, 752360358798 },
  { "KQNNPKQ", 1077855848520, 9328976, 137970031624, 39603486, 100132232020,  586815543722, 1003926, 6890360782, 139664, 998734436,  99133497584, 39463822, 131079670842, 8325050, 491040304798 },
  { "KQNNPKR", 1316805725236, 0, 127869788348, 0, 1735200370,  593936493156, 0, 548482794, 0, 220806580,  1514393790, 0, 127321305554, 0, 722869232080 },
  { "KQNPPKB", 1140238507404, 0, 18337160406, 0, 15361680,  469637899650, 0, 14102802, 0, 2874934,  12486746, 0, 18323057604, 0, 670600607754 },
  { "KQNPPKN", 1178741263870, 1052, 12041749280, 0, 6978432,  469567456338, 26, 86372974, 0, 1048048,  5930384, 0, 11955376306, 1026, 709173807532 },
  { "KQNPPKP", 908130473994, 388652, 3349542768, 94, 4908528938,  349732759086, 230, 6287980, 0, 2713966,  4905814972, 94, 3343254788, 388422, 558397714908 },
  { "KQNPPKQ", 816540381336, 14337640, 73261408810, 2920, 119817855368,  462054024148, 1262228, 6363507390, 4, 1236083616,  118581771752, 2916, 66897901420, 13075412, 354486357188 },
  { "KQNPPKR", 1029623395216, 0, 73116151690, 0, 3254725144,  469193375542, 0, 251367182, 0, 210134662,  3044590482, 0, 72864784508, 0, 560430019674 },
  { "KQPPPKB", 859530481860, 0, 20679630366, 0, 21052746,  368110263534, 0, 25965666, 0, 3736752,  17315994, 0, 20653664700, 0, 491420218326 },
  { "KQPPPKN", 891237536400, 0, 12354633258, 0, 9178926,  368027257878, 0, 111757566, 0, 950508,  8228418, 0, 12242875692, 0, 523210278522 },
  { "KQPPPKP", 682247086626, 1206582, 3174016116, 0, 5277991824,  272477376810, 750, 4288188, 0, 2905080,  5275086744, 0, 3169727928, 1205832, 409769709816 },
  { "KQPPPKQ", 595376542674, 30699378, 74822185200, 0, 99880764372,  357949851636, 3774030, 8985698400, 0, 1200641886,  98680122486, 0, 65836486800, 26925348, 237426691038 },
  { "KQPPPKR", 783178180836, 0, 21136014270, 0, 36581914410,  367732836954, 0, 130547268, 0, 276581730,  36305332680, 0, 21005467002, 0, 415445343882 },
  { "KQQBBKP", 1376494950704, 745328, 3844441960, 1861484, 43442748,  357550990160, 108, 31214300, 0, 836376,  42606372, 1861484, 3813227660, 745220, 1018943960544 },
  { "KQQBNKP", 1405092177808, 12258, 3484709782, 0, 56447404,  385800683610, 0, 29422152, 0, 840210,  55607194, 0, 3455287630, 12258, 1019291494198 },
  { "KQQBPKB", 1325481094316, 0, 234640304, 0, 10453770,  403917714288, 0, 260230, 0, 1908960,  8544810, 0, 234380074, 0, 921563380028 },
  { "KQQBPKN", 1369340330746, 118, 456985452, 0, 2462282,  403918815706, 0, 670410, 0, 397362,  2064920, 0, 456315042, 118, 965421515040 },
  { "KQQBPKP", 1064818394222, 301792, 2422861588, 0, 59590014,  303891287542, 46, 20072882, 0, 993066,  58596948, 0, 2402788706, 301746, 760927106680 },
  { "KQQBPKQ", 1093419805208, 28981198, 28470595028, 0, 3301764140,  401229498574, 168856, 2379007578, 0, 311208470,  2990555670, 0, 26091587450, 28812342, 692190306634 },
  { "KQQBPKR", 1254460616246, 0, 600988202, 0, 563210454,  403800813730, 0, 47997104, 0, 71072644,  492137810, 0, 552991098, 0, 850659802516 },
  { "KQQNNKP", 1430455740540, 139108, 3507709584, 6780, 64252028,  411196921932, 96, 27709248, 0, 815484,  63436544, 6780, 3480000336, 139012, 1019258818608 },
  { "KQQNPKB", 1353650592320, 0, 201388656, 0, 3955976,  432048719434, 0, 131024, 0, 781582,  3174394, 0, 201257632, 0, 921601872886 },
  { "KQQNPKN", 1397524081548, 0, 402508412, 0, 2937200,  432048652554, 0, 520182, 0, 459304,  2477896, 0, 401988230, 0, 965475428994 },
  { "KQQNPKP", 1085599793470, 46200, 2290088122, 0, 71782380,  324553312998, 10, 18608638, 0, 994446,  70787934, 0, 2271479484, 46190, 761046480472 },
  { "KQQNPKQ", 1114960925348, 3457348, 34773645620, 0, 3612865820,  429437857686, 23278, 2257469442, 0, 354281634,  3258584186, 0, 32516176178, 3434070, 685523067662 },
  { "KQQNPKR", 1282471919948, 0, 538158526, 0, 744484990,  431914460788, 0, 41499858, 0, 93671394,  650813596, 0, 496658668, 0, 850557459160 },
  { "KQQPPKB", 1029298592240, 0, 126777244, 0, 6600524,  340494488948, 0, 117532, 0, 1211424,  5389100, 0, 126659712, 0, 688804103292 },
  { "KQQPPKN", 1061396297316, 0, 231938604, 0, 2697232,  340494920668, 0, 458400, 0, 438836,  2258396, 0, 231480204, 0, 720901376648 },
  { "KQQPPKP", 819254401712, 176772, 1605307356, 0, 74761796,  254273998584, 112, 12318212, 0, 1157544,  73604252, 0, 1592989144, 176660, 564980403128 },
  { "KQQPPKQ", 846635681896, 16039364, 30384859396, 0, 3438345936,  338473581940, 88020, 1673540604, 0, 348607340,  3089738596, 0, 28711318792, 15951344, 508162099956 },
  { "KQQPPKR", 975772946148, 0, 317091672, 0, 745174748,  340373464956, 0, 30544348, 0, 91808600,  653366148, 0, 286547324, 0, 635399481192 },
  { "KQQQBKP", 1285033733670, 0, 5012300196, 0, 16787058,  267223051044, 0, 37066194, 0, 302406,  16484652, 0, 4975234002, 0, 1017810682626 },
  { "KQQQNKP", 1306737055176, 0, 4668187692, 0, 21385260,  288587831394, 0, 36119334, 0, 276120,  21109140, 0, 4632068358, 0, 1018149223782 },
  { "KQQQPKB", 1222788327288, 0, 612013206, 0, 1586142,  301595232024, 0, 116598, 0, 273102,  1313040, 0, 611896608, 0, 921193095264 },
  { "KQQQPKN", 1266663076998, 0, 811548354, 0, 891492,  301594753632, 0, 726828, 0, 141264,  750228, 0, 810821526, 0, 965068323366 },
  { "KQQQPKP", 987497835330, 5100, 3256166262, 0, 23926398,  227363519532, 6, 25242864, 0, 376608,  23549790, 0, 3230923398, 5094, 760134315798 },
  { "KQQQPKQ", 1003735238106, 290976, 17520566904, 0, 1640787834,  298713415728, 6216, 2732891352, 0, 149308428,  1491479406, 0, 14787675552, 284760, 705021822378 },
  { "KQQQPKR", 1152393982356, 0, 610146732, 0, 296424060,  301497818358, 0, 63628554, 0, 34174812,  262249248, 0, 546518178, 0, 850896163998 },
  { "KQQQQKP", 1209766923432, 0, 6093519288, 0, 6724992,  193025922960, 0, 38772432, 0, 71040,  6653952, 0, 6054746856, 0, 1016741000472 },
  { "KQQQRKP", 1254283157682, 522, 5438878104, 0, 12988812,  236895240510, 0, 37199508, 0, 183822,  12804990, 0, 5401678596, 522, 1017387917172 },
  { "KQQRBKP", 1345085853116, 468, 4143972232, 0, 31470940,  326425058162, 0, 33225752, 0, 611562,  30859378, 0, 4110746480, 468, 1018660794954 },
  { "KQQRNKP", 1366748072102, 1222, 3838705810, 0, 38778770,  347790817198, 0, 31758966, 0, 580460,  38198310, 0, 3806946844, 1222, 1018957254904 },
  { "KQQRPKB", 1286249584376, 0, 470555296, 0, 2345270,  364915558124, 0, 204950, 0, 416956,  1928314, 0, 470350346, 0, 921334026252 },
  { "KQQRPKN", 1330259252064, 0, 534316856, 0, 2506230,  364915039298, 0, 706676, 0, 434056,  2072174, 0, 533610180, 0, 965344212766 },
  { "KQQRPKP", 1035190904218, 18438, 2594818214, 5644, 45779088,  274420811434, 24, 21201172, 0, 718892,  45060196, 5644, 2573617042, 18414, 760770092784 },
  { "KQQRPKQ", 1064853120540, 1321448, 18709016436, 233008, 2653750694,  362073029964, 25350, 2597632540, 0, 245492176,  2408258518, 233008, 16111383896, 1296098, 702780090576 },
  { "KQQRPKR", 1215597901110, 0, 520264776, 0, 502945568,  364794058634, 0, 62630262, 0, 59491134,  443454434, 0, 457634514, 0, 850803842476 },
  { "KQQRRKP", 1304459050320, 972, 4618928104, 0, 23623396,  286265230348, 0, 33587032, 0, 384132,  23239264, 0, 4585341072, 972, 1018193819972 },
  { "KQRBBKP", 1453353842712, 434152, 4600271904, 9735102, 124699946,  435260369940, 214, 24681156, 0, 1531226,  123168720, 9735102, 4575590748, 433938, 1018093472772 },
  { "KQRBNKP", 1481812758914, 1611295, 4279973509, 0, 174088776,  463441781171, 701, 22704827, 0, 1544515,  172544261, 0, 4257268682, 1610594, 1018370977743 },
  { "KQRBPKB", 1408432311552, 0, 475849202, 0, 15316530,  487113874816, 0, 417158, 0, 2880398,  12436132, 0, 475432044, 0, 921318436736 },
  { "KQRBPKN", 1452656004074, 136, 335416704, 0, 5646578,  487115507861, 0, 694202, 0, 970309,  4676269, 0, 334722502, 136, 965540496213 },
  { "KQRBPKP", 1125702642562, 624624, 3018153089, 100545, 258316954,  365574771035, 219, 14533796, 0, 1738644,  256578310, 100545, 3003619293, 624405, 760127871527 },
  { "KQRBPKQ", 1075538141282, 41065481, 115020791426, 6400705, 17812035574,  483445653553, 1081975, 3154729246, 5241, 515702357,  17296333217, 6395464, 111866062180, 39983506, 592092487729 },
  { "KQRBPKR", 1333892625811, 394857, 3996660926, 0, 932422202,  486947103040, 2938, 49674460, 0, 120391934,  812030268, 0, 3946986466, 391919, 846945522771 },
  { "KQRNNKP", 1505730623108, 1302832, 5178799778, 45148, 207225898,  488292846436, 96, 21230014, 0, 1518938,  205706960, 45148, 5157569764, 1302736, 1017437776672 },
  { "KQRNPKB", 1436283588260, 5, 441011852, 0, 5690979,  514922549549, 0, 282341, 0, 1154294,  4536685, 0, 440729511, 5, 921361038711 },
  { "KQRNPKN", 1480448647966, 12, 349020418, 0, 6212908,  514922433167, 0, 506623, 0, 1046394,  5166514, 0, 348513795, 12, 965526214799 },
  { "KQRNPKP", 1145923272864, 652910, 3038597115, 242841, 420187218,  385979191971, 209, 13211371, 40, 1755277,  418431941, 242801, 3025385744, 652701, 759944080893 },
  { "KQRNPKQ", 1090031319697, 28305739, 115424722653, 15208040, 30725692151,  510900562237, 412003, 3439859831, 29353, 583122760,  30142569391, 15178687, 111984862822, 27893736, 579130757460 },
  { "KQRNPKR", 1357798380293, 40331, 7638494499, 0, 1192002485,  514716659702, 62, 54451468, 0, 152874952,  1039127533, 0, 7584043031, 40269, 843081720591 },
  { "KQRPPKB", 1095035266560, 244, 304315882, 0, 9555618,  406410929900, 0, 256018, 0, 1800282,  7755336, 0, 304059864, 244, 688624336660 },
  { "KQRPPKN", 1127341577862, 0, 200990566, 0, 5533020,  406411488576, 0, 542400, 0, 955224,  4577796, 0, 200448166, 0, 720930089286 },
  { "KQRPPKP", 866645276614, 169546, 1875632530, 785750, 964388276,  302829080624, 134, 7996320, 60, 2002394,  962385882, 785690, 1867636210, 169412, 563816195990 },
  { "KQRPPKQ", 818377146500, 11171642, 65845464554, 38653626, 62119658566,  402325372364, 510556, 3386582812, 233468, 700287000,  61419371566, 38420158, 62458881742, 10661086, 416051774136 },
  { "KQRPPKR", 1031636343628, 0, 9839830624, 0, 1276206612,  406215750284, 0, 48525922, 0, 148709994,  1127496618, 0, 9791304702, 0, 625420593344 },
  { "KQRRBKP", 1412405065644, 141962, 3324069606, 0, 59187600,  392958133274, 16, 26819268, 0, 1110974,  58076626, 0, 3297250338, 141946, 1019446932370 },
  { "KQRRNKP", 1433607390098, 326154, 3130757176, 0, 81008528,  413990769868, 134, 25241690, 0, 1068984,  79939544, 0, 3105515486, 326020, 1019616620230 },
  { "KQRRPKB", 1357390708612, 16, 354351270, 0, 3387994,  435941204354, 0, 319368, 0, 619258,  2768736, 0, 354031902, 16, 921449504258 },
  { "KQRRPKN", 1401469095930, 0, 347999184, 0, 4942986,  435940681526, 0, 576188, 0, 885266,  4057720, 0, 347422996, 0, 965528414404 },
  { "KQRRPKP", 1087905761184, 12867346, 2514819842, 57412, 130291392,  327158129626, 2914, 15628162, 0, 1242394,  129048998, 57412, 2499191680, 12864432, 760747631558 },
  { "KQRRPKQ", 1079052907800, 1035568328, 68861674494, 3858034, 8289396420,  432654441466, 8030204, 2882390654, 310, 397280346,  7892116074, 3857724, 65979283840, 1027538124, 646398466334 },
  { "KQRRPKR", 1286072513374, 0, 754228770, 0, 820332260,  435762249016, 0, 80686380, 0, 99207584,  721124676, 0, 673542390, 0, 850310264358 },
  { "KQRRRKP", 1360701639810, 6414, 3733500714, 0, 41330454,  341645985054, 0, 27388818, 0, 702240,  40628214, 0, 3706111896, 6414, 1019055654756 },
  { "KRBBBKP", 1541772982164, 127488, 44988291138, 174534066, 1284014412,  564692650362, 342, 721075836, 41316, 3780132,  1280234280, 174492750, 44267215302, 127146, 977080331802 },
  { "KRBBNKP", 1605823109024, 19241982, 16983057210, 160595870, 1481671874,  601641454006, 59384, 19659212, 67966, 4034112,  1477637762, 160527904, 16963397998, 19182598, 1004181655018 },
  { "KRBBPKB", 1514231585212, 0, 40901217362, 0, 57492946,  632011037734, 0, 1361451700, 0, 11501174,  45991772, 0, 39539765662, 0, 882220547478 },
  { "KRBBPKN", 1587498646860, 156252756, 11598116552, 0, 10869560,  633229335530, 646532, 152145216, 0, 1863330,  9006230, 0, 11445971336, 155606224, 954269311330 },
  { "KRBBPKP", 1215815665454, 992502144, 17622982124, 162317014, 3345230292,  474469875050, 14837640, 59941328, 53814, 5195116,  3340035176, 162263200, 17563040796, 977664504, 741345790404 },
  { "KRBBPKQ", 727872993812, 13053407274, 447419064034, 8361656942, 157978130642,  508133101982, 7488421552, 108348112450, 553064916, 8861289708,  149116840934, 7808592026, 339070951584, 5564985722, 219739891830 },
  { "KRBBPKR", 1410307570902, 51023398, 73110878776, 0, 1619448956,  632163606418, 405330, 996847114, 0, 223131746,  1396317210, 0, 72114031662, 50618068, 778143964484 },
  { "KRBNNKP", 1631589239540, 23957790, 22697660422, 651322, 1933617730,  633397933040, 85718, 40305438, 512, 4400816,  1929216914, 650810, 22657354984, 23872072, 998191306500 },
  { "KRBNPKB", 1584166319362, 5, 6688159982, 0, 32579037,  668992206862, 0, 81911660, 0, 6634952,  25944085, 0, 6606248322, 5, 915174112500 },
  { "KRBNPKN", 1630726438726, 5066329, 4216733844, 0, 12409695,  669048315357, 62140, 30286553, 0, 2089424,  10320271, 0, 4186447291, 5004189, 961678123369 },
  { "KRBNPKP", 1240862235478, 27668860, 18815017826, 73163347, 4306031818,  500614667657, 320623, 74397461, 23067, 5914441,  4300117377, 73140280, 18740620365, 27348237, 740247567821 },
  { "KRBNPKQ", 731974842085, 392462577, 457107312656, 1515408023, 199391990229,  529667873007, 199855935, 126714168862, 154814273, 12344041397,  187047948832, 1360593750, 330393143794, 192606642, 202306969078 },
  { "KRBNPKR", 1454133227608, 2849597, 64578752353, 0, 2070855340,  668014340941, 63941, 785977213, 0, 280371379,  1790483961, 0, 63792775140, 2785656, 786118886667 },
  { "KRBPPKB", 1195142954512, 240, 21751676772, 0, 42733776,  527464062614, 0, 528728238, 0, 8422344,  34311432, 0, 21222948534, 240, 667678891898 },
  { "KRBPPKN", 1242174901524, 5132, 6950536030, 0, 10885758,  527934537398, 70, 64828520, 0, 1847208,  9038550, 0, 6885707510, 5062, 714240364126 },
  { "KRBPPKP", 927112573480, 11628004, 25888471876, 1743500, 6449994760,  392264534624, 300402, 543496364, 2122, 8904924,  6441089836, 1741378, 25344975512, 11327602, 534848038856 },
  { "KRBPPKQ", 409168980126, 151958150, 420629636270, 57168196, 237972579142,  320656980554, 83039088, 184298853730, 10157928, 22952181896,  215020397246, 47010268, 236330782540, 68919062, 88511999572 },
  { "KRBPPKR", 1087107960184, 565034, 74426600674, 0, 2805481968,  526443212488, 26902, 1288026926, 0, 269946880,  2535535088, 0, 73138573748, 538132, 560664747696 },
  { "KRNNNKP", 1644306817896, 172538214, 37318841250, 1275768, 1562890464,  660396529050, 222930, 158680278, 1014, 4529040,  1558361424, 1274754, 37160160972, 172315284, 983910288846 },
  { "KRNNPKB", 1597380607338, 965536, 24349357714, 0, 12512544,  699493548232, 9350, 440859248, 0, 2721390,  9791154, 0, 23908498466, 956186, 897887059106 },
  { "KRNNPKN", 1639744616534, 756210, 26058168724, 0, 13491872,  699576338012, 6834, 358577850, 0, 2215524,  11276348, 0, 25699590874, 749376, 940168278522 },
  { "KRNNPKP", 1252835108054, 162711218, 29630295628, 2885500, 4028820794,  522929383122, 940788, 333456208, 2546, 7244450,  4021576344, 2882954, 29296839420, 161770430, 729905724932 },
  { "KRNNPKQ", 682525369466, 217245938, 549055626900, 89821756, 189350336256,  516890163060, 121353734, 169243538316, 9794622, 13672288488,  175678047768, 80027134, 379812088584, 95892204, 165635206406 },
  { "KRNNPKR", 1401358347132, 82172, 147707525950, 0, 2576114390,  696970859842, 5482, 2624778974, 0, 341493922,  2234620468, 0, 145082746976, 76690, 704387487290 },
  { "KRNPPKB", 1222811931916, 254, 19668900158, 0, 21054546,  553224019320, 0, 337576638, 0, 4138812,  16915734, 0, 19331323520, 254, 669587912596 },
  { "KRNPPKN", 1262429794644, 1278, 12258887780, 0, 12166316,  553447286924, 84, 116460718, 0, 1987044,  10179272, 0, 12142427062, 1194, 708982507720 },
  { "KRNPPKP", 942462936722, 22820144, 26290563730, 3474878, 9273686204,  410430754828, 1647334, 950741990, 33950, 23130392,  9250555812, 3440928, 25339821740, 21172810, 532032181894 },
  { "KRNPPKQ", 425027322082, 198880934, 387039679178, 84262950, 281194698314,  341291350472, 129529926, 174512622530, 25469522, 37606762320,  243587935994, 58793428, 212527056648, 69351008, 83735971610 },
  { "KRNPPKR", 1091905690276, 68992, 93865866020, 0, 4133504146,  551131700188, 3984, 2103909010, 0, 330121588,  3803382558, 0, 91761957010, 65008, 540773990088 },
  { "KRPPPKB", 924387691032, 576, 22187203980, 0, 29071446,  433987718538, 0, 519572556, 0, 5476920,  23594526, 0, 21667631424, 576, 490399972494 },
  { "KRPPPKN", 957384496356, 0, 12575749086, 0, 13905204,  434362000548, 0, 148953534, 0, 1813932,  12091272, 0, 12426795552, 0, 523022495808 },
  { "KRPPPKP", 703362372702, 1415094, 20502560844, 8307174, 15287588562,  319163460612, 46782, 1489525332, 345570, 293135760,  14994452802, 7961604, 19013035512, 1368312, 384198912090 },
  { "KRPPPKQ", 268583213370, 10139934, 232680066318, 122009184, 335087564880,  231458323302, 4650750, 118691424726, 57532926, 84300836310,  250786728570, 64476258, 113988641592, 5489184, 37124890068 },
  { "KRPPPKR", 822259038498, 0, 46959394476, 0, 38050478604,  430204696890, 0, 3551717448, 0, 756353676,  37294124928, 0, 43407677028, 0, 392054341608 },
  { "KRRBBKP", 1537916294916, 424296, 7160505764, 18694900, 242009740,  522515264460, 156, 17614580, 6456, 2642684,  239367056, 18688444, 7142891184, 424140, 1015401030456 },
  { "KRRBNKP", 1565477146902, 5562378, 7169231618, 0, 349498386,  550180461150, 4164, 15876592, 0, 2696098,  346802288, 0, 7153355026, 5558214, 1015296685752 },
  { "KRRBPKB", 1501426483558, 16, 714563368, 0, 21978476,  580351895674, 0, 580484, 0, 4244348,  17734128, 0, 713982884, 16, 921074587884 },
  { "KRRBPKN", 1545938662096, 154, 287588674, 0, 10364702,  580354273046, 0, 611302, 0, 1836158,  8528544, 0, 286977372, 154, 965584389050 },
  { "KRRBPKP", 1190690184106, 32837014, 6721957530, 274096, 588035676,  434631513294, 9478, 9976722, 74, 2994774,  585040902, 274022, 6711980808, 32827536, 756058670812 },
  { "KRRBPKQ", 960470124464, 1249857086, 300738003220, 17116324, 39182881508,  551192079132, 174856220, 27676075018, 301726, 1313408410,  37869473098, 16814598, 273061928202, 1075000866, 409278045332 },
  { "KRRBPKR", 1422523297570, 782040, 8051212912, 0, 1486359408,  580040219952, 7414, 120656780, 0, 195836360,  1290523048, 0, 7930556132, 774626, 842483077618 },
  { "KRRNNKP", 1587028592496, 3939676, 9361651304, 101004, 438029624,  574008885688, 1032, 18322944, 32, 2703128,  435326496, 100972, 9343328360, 3938644, 1013019706808 },
  { "KRRNPKB", 1528463770776, 26, 682487772, 0, 8019978,  607345894952, 0, 415182, 0, 1663506,  6356472, 0, 682072590, 26, 921117875824 },
  { "KRRNPKN", 1572866272224, 24, 350798122, 0, 10798390,  607345725868, 0, 361878, 0, 1885894,  8912496, 0, 350436244, 24, 965520546356 },
  { "KRRNPKP", 1209254534502, 45846798, 7478052534, 678158, 1038235396,  454414923382, 23204, 10366822, 200, 3239700,  1034995696, 677958, 7467685712, 45823594, 754839611120 },
  { "KRRNPKQ", 953495147022, 1424370726, 304953548554, 37679910, 68738489524,  570373016906, 283540090, 33998954754, 1715610, 2690746280,  66047743244, 35964300, 270954593800, 1140830636, 383122130116 },
  { "KRRNPKR", 1441837191640, 78442, 15372545716, 0, 1843089266,  606917167556, 726, 189982172, 0, 240823186,  1602266080, 0, 15182563544, 77716, 834920024084 },
  { "KRRPPKB", 1168509565784, 516, 490697292, 0, 13551096,  480074390260, 0, 656340, 0, 2615984,  10935112, 0, 490040952, 516, 688435175524 },
  { "KRRPPKN", 1200992599364, 0, 210640004, 0, 9538464,  480075392412, 0, 570064, 0, 1700108,  7838356, 0, 210069940, 0, 720917206952 },
  { "KRRPPKP", 911627101236, 210586676, 9170897172, 2212200, 2679189408,  357011664568, 812608, 26268932, 340, 4067060,  2675122348, 2211860, 9144628240, 209774068, 554615436668 },
  { "KRRPPKQ", 597812357960, 3945601468, 276014754092, 93088292, 142190969460,  401452581880, 1706985492, 68215018400, 7268656, 8695808156,  133495161304, 85819636, 207799735692, 2238615976, 196359776080 },
  { "KRRPPKR", 1094958494984, 0, 19463786024, 0, 1994776240,  479615146112, 0, 229613032, 0, 232903440,  1761872800, 0, 19234172992, 0, 615343348872 },
  { "KRRRBKP", 1487478627360, 493890, 2730946734, 0, 106320984,  467494148052, 330, 17978226, 0, 1861080,  104459904, 0, 2712968508, 493560, 1019984479308 },
  { "KRRRNKP", 1507656278112, 1311324, 2804814552, 0, 157280004,  487798128864, 1638, 17334720, 0, 1817490,  155462514, 0, 2787479832, 1309686, 1019858149248 },
  { "KRRRPKB", 1436901243906, 48, 267026178, 0, 4811322,  515365486092, 0, 388710, 0, 901740,  3909582, 0, 266637468, 48, 921535757814 },
  { "KRRRPKN", 1481002607844, 0, 235678116, 0, 8385702,  515364955446, 0, 288366, 0, 1532730,  6852972, 0, 235389750, 0, 965637652398 },
  { "KRRRPKP", 1145024101512, 63343248, 4095057444, 163674, 297865728,  386079797940, 17004, 9861402, 12, 2061168,  295804560, 163662, 4085196042, 63326244, 758944303572 },
  { "KRRRPKQ", 1019023043028, 3333221082, 195138311034, 11116200, 19162347294,  500754198018, 244087548, 13645117284, 56082, 723317610,  18439029684, 11060118, 181493193750, 3089133534, 518268845010 },
  { "KRRRPKR", 1364193400422, 0, 1581551400, 0, 1296756144,  515034516900, 0, 172368642, 0, 159891000,  1136865144, 0, 1409182758, 0, 849158883522 },
  { "KRRRRKP", 1423393540656, 20520, 2934508968, 0, 69856512,  403576033536, 0, 18319824, 0, 1172016,  68684496, 0, 2916189144, 20520, 1019817507120 },
  { "KBBBBKB", 295945537836, 0, 207104223960, 0, 36371796,  137554061880, 0, 58794738444, 0, 7515636,  28856160, 0, 148309485516, 0, 158391475956 },
  { "KBBBBKN", 429838244712, 127034592, 88106411376, 0, 1978080,  169655772192, 480792, 26699740032, 0, 322944,  1655136, 0, 61406671344, 126553800, 260182472520 },
  { "KBBBBKQ", 159613219248, 19604520, 208105380456, 6636108036, 61561791228,  112746346824, 7371636, 76883882916, 309510828, 6409203756,  55152587472, 6326597208, 131221497540, 12232884, 46866872424 },
  { "KBBBBKR", 378210212736, 118475460, 101138317032, 0, 338176548,  168026256264, 1843656, 28281588048, 0, 46627992,  291548556, 0, 72856728984, 116631804, 210183956472 },
  { "KBBBNKB", 416646743565, 0, 102001222503, 0, 25275318,  188285614929, 0, 23652501624, 0, 5307201,  19968117, 0, 78348720879, 0, 228361128636 },
  { "KBBBNKN", 522447896085, 65068716, 11145050637, 0, 2761116,  211611960381, 163308, 330856527, 0, 443538,  2317578, 0, 10814194110, 64905408, 310835935704 },
  { "KBBBNKQ", 174958202301, 33134250633, 174181105455, 6520136808, 62729516085,  132299586867, 18432234372, 56511511848, 553047156, 4147043511,  58582472574, 5967089652, 117669593607, 14702016261, 42658615434 },
  { "KBBBNKR", 377337630009, 36293223444, 81300698652, 0, 460737465,  190058794059, 15806345112, 6014711295, 0, 63573288,  397164177, 0, 75285987357, 20486878332, 187278835950 },
  { "KBBNNKB", 507240188232, 0, 25308093162, 0, 15423002,  224295211724, 0, 1535361342, 0, 3313698,  12109304, 0, 23772731820, 0, 282944976508 },
  { "KBBNNKN", 545456808144, 22680888, 2068228332, 0, 3522200,  225819008388, 71816, 14248864, 0, 557696,  2964504, 0, 2053979468, 22609072, 319637799756 },
  { "KBBNNKQ", 176518051986, 339409288, 212386947236, 3608609464, 72560656318,  137196658294, 199351320, 82004878368, 537499628, 5895499154,  66665157164, 3071109836, 130382068868, 140057968, 39321393692 },
  { "KBBNNKR", 386224054952, 6827768, 122446712502, 0, 605157358,  215782799064, 665226, 9967063854, 0, 83358620,  521798738, 0, 112479648648, 6162542, 170441255888 },
  { "KBNNNKB", 532737781503, 37282443, 11872983042, 0, 7427376,  237791416143, 56430, 132508737, 0, 1675422,  5751954, 0, 11740474305, 37226013, 294946365360 },
  { "KBNNNKN", 554822569305, 9131235, 4807120626, 0, 4188366,  237861230160, 140622, 63633300, 0, 652650,  3535716, 0, 4743487326, 8990613, 316961339145 },
  { "KBNNNKQ", 183959275377, 24363589344, 193478855049, 28177434, 75675547056,  147709524780, 15377880780, 66806362935, 4467402, 8027420835,  67648126221, 23710032, 126672492114, 8985708564, 36249750597 },
  { "KBNNNKR", 345339899055, 0, 175261935765, 0, 772687728,  216133051386, 0, 21686093226, 0, 106512120,  666175608, 0, 153575842539, 0, 129206847669 },
  { "KNNNNKB", 527300878416, 129965304, 27446932800, 0, 1694880,  247360116756, 2039004, 786998208, 0, 499800,  1195080, 0, 26659934592, 127926300, 279940761660 },
  { "KNNNNKN", 560276285196, 16410300, 9569624424, 0, 4686648,  248098088880, 141240, 50707656, 0, 715992,  3970656, 0, 9518916768, 16269060, 312178196316 },
  { "KNNNNKQ", 190545090324, 132328956, 241107685932, 44951520, 55899384564,  155158212192, 84711768, 86904946284, 6275376, 5995508148,  49903876416, 38676144, 154202739648, 47617188, 35386878132 },
  { "KNNNNKR", 326956968264, 0, 203682209616, 0, 959341704,  215355470808, 0, 32661252240, 0, 132930720,  826410984, 0, 171020957376, 0, 111601497456 },
  { "KQBBBKB", 437455808364, 0, 23136971010, 0, 12361362,  153746714931, 0, 126166413, 0, 2441760,  9919602, 0, 23010804597, 0, 283709093433 },
  { "KQBBBKN", 464833835925, 40780110, 10716995139, 0, 1064730,  153700108398, 72132, 174969834, 0, 172740,  891990, 0, 10542025305, 40707978, 311133727527 },
  { "KQBBBKQ", 336272477604, 18114654, 42436229022, 1969119540, 12759169812,  152193182097, 733086, 1529616861, 2721900, 149069160,  12610100652, 1966397640, 40906612161, 17381568, 184079295507 },
  { "KQBBBKR", 420546864192, 35593170, 16546778850, 0, 194952708,  153698283342, 42792, 151702266, 0, 25294704,  169658004, 0, 16395076584, 35550378, 266848580850 },
  { "KQBBNKB", 462319274010, 0, 10607402198, 0, 7846698,  166166715856, 0, 36400659, 0, 1588759,  6257939, 0, 10571001539, 0, 296152558154 },
  { "KQBBNKN", 487400469253, 13438930, 506687945, 0, 1461946,  166202544446, 35350, 1894332, 0, 231146,  1230800, 0, 504793613, 13403580, 321197924807 },
  { "KQBBNKQ", 353014828198, 6589650, 34586441480, 1782146704, 16394486770,  165045665326, 298062, 980975882, 4777204, 172988800,  16221497970, 1777369500, 33605465598, 6291588, 187969162872 },
  { "KQBBNKR", 429543979989, 4750384, 19837451405, 0, 267389312,  166105678910, 15052, 64321560, 0, 34689752,  232699560, 0, 19773129845, 4735332, 263438301079 },
  { "KQBNNKB", 482353914247, 0, 1614257649, 0, 3881688,  177239744174, 0, 1667248, 0, 824530,  3057158, 0, 1612590401, 0, 305114170073 },
  { "KQBNNKN", 498073039807, 2111574, 882619835, 0, 1817536,  177236915704, 27580, 5011938, 0, 280730,  1536806, 0, 877607897, 2083994, 320836124103 },
  { "KQBNNKQ", 355730020432, 1692800, 41452975296, 5968278, 19631366674,  175893853766, 106116, 1143674584, 14544, 204586942,  19426779732, 5953734, 40309300712, 1586684, 179836166666 },
  { "KQBNNKR", 421632568003, 0, 38706098463, 0, 352435302,  177070330990, 0, 126351676, 0, 45553286,  306882016, 0, 38579746787, 0, 244562237013 },
  { "KQNNNKB", 485308543023, 38087421, 8290114956, 0, 785580,  186898040208, 2478, 9448320, 0, 222342,  563238, 0, 8280666636, 38084943, 298410502815 },
  { "KQNNNKN", 505229475870, 5092320, 3388414830, 0, 2083128,  186860199216, 96876, 47103858, 0, 313398,  1769730, 0, 3341310972, 4995444, 318369276654 },
  { "KQNNNKQ", 348090723432, 4979130, 64548507567, 11930988, 13831359759,  184536742869, 494520, 2159216913, 21516, 211237530,  13620122229, 11909472, 62389290654, 4484610, 163553980563 },
  { "KQNNNKR", 421931197683, 0, 47977134657, 0, 448246824,  186614346150, 0, 235735362, 0, 57631836,  390614988, 0, 47741399295, 0, 235316851533 },
  { "KQQBBKB", 424541874136, 0, 120437184, 0, 3528956,  117935242834, 0, 137442, 0, 642368,  2886588, 0, 120299742, 0, 306606631302 },
  { "KQQBBKN", 439403501548, 1945680, 247439828, 0, 488388,  117935689568, 4, 254432, 0, 78640,  409748, 0, 247185396, 1945676, 321467811980 },
  { "KQQBBKQ", 347925776420, 12100780, 8801326878, 16522484, 760083610,  116948654704, 110128, 919385722, 1168, 67870922,  692212688, 16521316, 7881941156, 11990652, 230977121716 },
  { "KQQBBKR", 400950183432, 0, 327990556, 0, 106714472,  117903875332, 0, 19304768, 0, 12842544,  93871928, 0, 308685788, 0, 283046308100 },
  { "KQQBNKB", 434126364404, 0, 86334588, 0, 1817772,  127484279558, 0, 67480, 0, 352094,  1465678, 0, 86267108, 0, 306642084846 },
  { "KQQBNKN", 449017485852, 377256, 183534326, 0, 654498,  127484400080, 26, 198104, 0, 100922,  553576, 0, 183336222, 377230, 321533085772 },
  { "KQQBNKQ", 357686825865, 40856, 8510062807, 0, 867557132,  126555523150, 376, 850450798, 0, 78724808,  788832324, 0, 7659612009, 40480, 231131302715 },
  { "KQQBNKR", 410581185000, 0, 204650456, 0, 147729492,  127450218698, 0, 16699396, 0, 17781038,  129948454, 0, 187951060, 0, 283130966302 },
  { "KQQNNKB", 442688944476, 0, 117395456, 0, 305436,  136076715022, 0, 30866, 0, 81848,  223588, 0, 117364590, 0, 306612229454 },
  { "KQQNNKN", 457537572524, 0, 255833392, 0, 774620,  136076567328, 0, 145780, 0, 114628,  659992, 0, 255687612, 0, 321461005196 },
  { "KQQNNKQ", 360375797080, 3235788, 14368380394, 60144, 909141858,  135155172434, 16300, 832173764, 28, 89465210,  819676648, 60116, 13536206630, 3219488, 225220624646 },
  { "KQQNNKR", 418942472056, 0, 387215880, 0, 196005616,  136037565592, 0, 15656504, 0, 23605640,  172399976, 0, 371559376, 0, 282904906464 },
  { "KQQQBKB", 394440574140, 0, 237675906, 0, 768834,  87949000674, 0, 66114, 0, 134460,  634374, 0, 237609792, 0, 306491573466 },
  { "KQQQBKN", 409293073986, 0, 373321002, 0, 159060,  87948905910, 0, 269958, 0, 25380,  133680, 0, 373051044, 0, 321344168076 },
  { "KQQQBKQ", 320597963583, 0, 6554219937, 0, 376805256,  86883731352, 0, 1033259490, 0, 32210406,  344594850, 0, 5520960447, 0, 233714232231 },
  { "KQQQBKR", 371061091032, 0, 282915618, 0, 54060414,  87919554414, 0, 23735388, 0, 5911446,  48148968, 0, 259180230, 0, 283141536618 },
  { "KQQQNKB", 401663445300, 0, 220857408, 0, 82728,  95154524754, 0, 22512, 0, 20538,  62190, 0, 220834896, 0, 306508920546 },
  { "KQQQNKN", 416576625825, 0, 295093641, 0, 201138,  95154324282, 0, 214404, 0, 29118,  172020, 0, 294879237, 0, 321422301543 },
  { "KQQQNKQ", 327987720390, 0, 6326806818, 0, 419828124,  94130539245, 0, 986881035, 0, 37147524,  382680600, 0, 5339925783, 0, 233857181145 },
  { "KQQQNKR", 378314572242, 0, 213049890, 0, 75811488,  95124935526, 0, 21370068, 0, 8262210,  67549278, 0, 191679822, 0, 283189636716 },
  { "KQQQQKB", 369605062980, 0, 475373772, 0, 0,  63350613960, 0, 5160, 0, 0,  0, 0, 475368612, 0, 306254449020 },
  { "KQQQQKN", 384421674936, 0, 646296984, 0, 0,  63350343600, 0, 275520, 0, 0,  0, 0, 646021464, 0, 321071331336 },
  { "KQQQQKQ", 295951716972, 0, 6791402412, 0, 187287264,  62273054844, 0, 1063177020, 0, 14387256,  172900008, 0, 5728225392, 0, 233678662128 },
  { "KQQQQKR", 346447872648, 0, 327018096, 0, 24594192,  63322528344, 0, 25706808, 0, 2383968,  22210224, 0, 301311288, 0, 283125344304 },
  { "KQQQRKB", 384285125382, 0, 418895346, 0, 0,  77974179624, 0, 23472, 0, 0,  0, 0, 418871874, 0, 306310945758 },
  { "KQQQRKN", 399183630204, 0, 507742404, 0, 183288,  77973888576, 0, 281040, 0, 33480,  149808, 0, 507461364, 0, 321209741628 },
  { "KQQQRKQ", 310514585565, 4188, 6741121407, 0, 298279464,  76858595973, 66, 1091316879, 0, 24290178,  273989286, 0, 5649804528, 4122, 233655989592 },
  { "KQQQRKR", 361113999870, 0, 263462142, 0, 45606900,  77942282400, 0, 27331692, 0, 4589004,  41017896, 0, 236130450, 0, 283171717470 },
  { "KQQRBKB", 414277892866, 0, 191647004, 0, 1146258,  107740569630, 0, 92612, 0, 206254,  940004, 0, 191554392, 0, 306537323236 },
  { "KQQRBKN", 429202799937, 0, 254834151, 0, 587208,  107740498970, 0, 267456, 0, 102070,  485138, 0, 254566695, 0, 321462300967 },
  { "KQQRBKQ", 339867601543, 6318, 6859950809, 0, 593097354,  106664976250, 240, 1021830090, 0, 54061916,  539035438, 0, 5838120719, 6078, 233202625293 },
  { "KQQRBKR", 390848659032, 6624, 244525186, 0, 96543470,  107705894280, 28, 24072882, 0, 10901306,  85642164, 0, 220452304, 6596, 283142764752 },
  { "KQQRNKB", 421507979528, 0, 173904442, 0, 120742,  114952107648, 0, 47780, 0, 31652,  89090, 0, 173856662, 0, 306555871880 },
  { "KQQRNKN", 436469658476, 0, 199227986, 0, 653418,  114951875074, 0, 202100, 0, 109906,  543512, 0, 199025886, 0, 321517783402 },
  { "KQQRNKQ", 347120115406, 20000, 6762361545, 0, 649477657,  113923801292, 504, 967471798, 0, 60913486,  588564171, 0, 5794889747, 19496, 233196314114 },
  { "KQQRNKR", 398063604908, 0, 207956862, 0, 129491126,  114915584128, 0, 22055168, 0, 14547784,  114943342, 0, 185901694, 0, 283148020780 },
  { "KQQRRKB", 400819930814, 0, 353647414, 0, 0,  94443699348, 0, 61248, 0, 0,  0, 0, 353586166, 0, 306376231466 },
  { "KQQRRKN", 415772981820, 0, 387641524, 0, 490052,  94443405524, 0, 264044, 0, 91028,  399024, 0, 387377480, 0, 321329576296 },
  { "KQQRRKQ", 327098831832, 14616, 6461482134, 0, 463219542,  93323306132, 484, 1080471510, 0, 39982470,  423237072, 0, 5381010624, 14132, 233775525700 },
  { "KQQRRKR", 377574967702, 0, 238104990, 0, 79553720,  94404334892, 0, 31177712, 0, 8247992,  71305728, 0, 206927278, 0, 283170632810 },
  { "KQRBBKB", 450440472556, 0, 260147276, 0, 5251970,  143974846430, 0, 220551, 0, 987189,  4264781, 0, 259926725, 0, 306465626126 },
  { "KQRBBKN", 465491361487, 2911212, 197799153, 0, 1335118,  143975568596, 2, 256174, 0, 229398,  1105720, 0, 197542979, 2911210, 321515792891 },
  { "KQRBBKQ", 345122778352, 5958836, 36069488817, 168220956, 2189394737,  142789826911, 150558, 1071824968, 10796, 114240937,  2075153800, 168210160, 34997663849, 5808278, 202332951441 },
  { "KQRBBKR", 426138383400, 80126, 1101104630, 0, 185351830,  143933549898, 144, 19534724, 0, 22969404,  162382426, 0, 1081569906, 79982, 282204833502 },
  { "KQRBNKB", 460076734615, 0, 160742144, 0, 2630555,  153509645900, 0, 120449, 0, 523333,  2107222, 0, 160621695, 0, 306567088715 },
  { "KQRBNKN", 475084110886, 401081, 141558104, 0, 1572411,  153509838631, 67, 187586, 0, 263398,  1309013, 0, 141370518, 401014, 321574272255 },
  { "KQRBNKQ", 352377987983, 20461439, 37723502881, 0, 2968124907,  152365450420, 398012, 1015161667, 0, 129279583,  2838845324, 0, 36708341214, 20063427, 200012537563 },
  { "KQRBNKR", 435572550338, 57425, 1140676203, 0, 245871532,  153462279794, 230, 17598243, 0, 30411415,  215460117, 0, 1123077960, 57195, 282110270544 },
  { "KQRNNKB", 468317476916, 0, 338283510, 0, 429930,  161926154340, 0, 98406, 0, 119978,  309952, 0, 338185104, 0, 306391322576 },
  { "KQRNNKN", 483355667677, 0, 286341897, 0, 1715950,  161925969994, 0, 122894, 0, 279836,  1436114, 0, 286219003, 0, 321429697683 },
  { "KQRNNKQ", 350797477350, 1646538, 47156492537, 950922, 3549592905,  160623477512, 67674, 1159260910, 174, 143566454,  3406026451, 950748, 45997231627, 1578864, 190173999838 },
  { "KQRNNKR", 442598785401, 0, 2461938885, 0, 314514254,  161867193214, 0, 20323472, 0, 38856038,  275658216, 0, 2441615413, 0, 280731592187 },
  { "KQRRBKB", 436583140634, 0, 148092956, 0, 1665490,  130002642550, 0, 131616, 0, 307282,  1358208, 0, 147961340, 0, 306580498084 },
  { "KQRRBKN", 451550156104, 0, 169002880, 0, 1275264,  130002621658, 0, 231854, 0, 227936,  1047328, 0, 168771026, 0, 321547534446 },
  { "KQRRBKQ", 359305885392, 3265056, 9290416162, 0, 983302366,  128921605684, 28840, 992454144, 0, 88992780,  894309586, 0, 8297962018, 3236216, 230384279708 },
  { "KQRRBKR", 412945276488, 17698, 342892724, 0, 163760354,  129954489614, 54, 29591920, 0, 18999860,  144760494, 0, 313300804, 17644, 282990786874 },
  { "KQRRNKB", 443706256654, 0, 131330628, 0, 169412,  137107806320, 0, 86408, 0, 46334,  123078, 0, 131244220, 0, 306598450334 },
  { "KQRRNKN", 458691335101, 0, 132621757, 0, 1335004,  137107551534, 0, 152978, 0, 234550,  1100454, 0, 132468779, 0, 321583783567 },
  { "KQRRNKQ", 364968325455, 7908956, 10411665901, 0, 1299826278,  136055214703, 44762, 954397713, 0, 98281884,  1201544394, 0, 9457268188, 7864194, 228913110752 },
  { "KQRRNKR", 419983478377, 0, 361221881, 0, 212104620,  137054129492, 0, 29342614, 0, 24466956,  187637664, 0, 331879267, 0, 282929348885 },
  { "KQRRRKB", 419376689160, 0, 285152826, 0, 0,  112931908140, 0, 116214, 0, 0,  0, 0, 285036612, 0, 306444781020 },
  { "KQRRRKN", 434359458630, 0, 288966234, 0, 952290,  112931634156, 0, 210972, 0, 179226,  773064, 0, 288755262, 0, 321427824474 },
  { "KQRRRKQ", 344376779181, 120474, 7421130372, 0, 713781855,  111817588602, 6552, 1049777760, 0, 64651440,  649130415, 0, 6371352612, 113922, 232559190579 },
  { "KQRRRKR", 395968576671, 0, 279471717, 0, 132841782,  112875325086, 0, 42570606, 0, 14128662,  118713120, 0, 236901111, 0, 283093251585 },
  { "KRBBBKB", 468833731587, 0, 25239252777, 0, 18093618,  186396867723, 0, 960751500, 0, 3641127,  14452491, 0, 24278501277, 0, 282436863864 },
  { "KRBBBKN", 498031470867, 43480488, 11001090333, 0, 2571462,  187012629702, 98166, 348091416, 0, 441066,  2130396, 0, 10652998917, 43382322, 311018841165 },
  { "KRBBBKQ", 239969306703, 211818, 165325076358, 2703668688, 18942784311,  152281276779, 76530, 34255404582, 60883500, 763618959,  18179165352, 2642785188, 131069671776, 135288, 87688029924 },
  { "KRBBBKR", 451338323736, 34958040, 19106608944, 0, 330235446,  186737323386, 105618, 579725286, 0, 44106060,  286129386, 0, 18526883658, 34852422, 264601000350 },
  { "KRBBNKB", 494755791418, 0, 11611854322, 0, 11224692,  199283376616, 0, 363364844, 0, 2311340,  8913352, 0, 11248489478, 0, 295472414802 },
  { "KRBBNKN", 520807689756, 14455414, 541140596, 0, 3119834,  199645792814, 44690, 2690494, 0, 524802,  2595032, 0, 538450102, 14410724, 321161896942 },
  { "KRBBNKQ", 274292994419, 99663112, 138469313381, 2380920624, 23985948792,  178820473145, 36761113, 19819290363, 104294532, 868233647,  23117715145, 2276626092, 118650023018, 62901999, 95472521274 },
  { "KRBBNKR", 460042639470, 4813258, 22616608775, 0, 433857113,  199323034117, 32980, 268134299, 0, 57851404,  376005709, 0, 22348474476, 4780278, 260719605353 },
  { "KRBNNKB", 515112833570, 0, 2043067632, 0, 5436610,  210409429913, 0, 20916273, 0, 1173994,  4262616, 0, 2022151359, 0, 304703403657 },
  { "KRBNNKN", 531176510114, 2185658, 966622188, 0, 3555020,  210424923044, 33410, 5977996, 0, 585730,  2969290, 0, 960644192, 2152248, 320751587070 },
  { "KRBNNKQ", 261991949540, 86458848, 158409731635, 8550502, 29514617183,  180735051024, 37805212, 28412049882, 447350, 1246166712,  28268450471, 8103152, 129997681753, 48653636, 81256898516 },
  { "KRBNNKR", 450558713569, 93910, 42771363262, 0, 550215255,  209808480864, 2076, 549926594, 0, 73110646,  477104609, 0, 42221436668, 91834, 240750232705 },
  { "KRNNNKB", 517282967391, 38311944, 9050708277, 0, 1074720,  219513602055, 156177, 129172296, 0, 314172,  760548, 0, 8921535981, 38155767, 297769365336 },
  { "KRNNNKN", 537733319535, 5180184, 3618282477, 0, 3815304,  219581499645, 126984, 61004955, 0, 613116,  3202188, 0, 3557277522, 5053200, 318151819890 },
  { "KRNNNKQ", 241981877100, 12566700, 193561441494, 17185341, 23649961593,  176388803157, 6099414, 42140033157, 837471, 1107471501,  22542490092, 16347870, 151421408337, 6467286, 65593073943 },
  { "KRNNNKR", 448950571590, 0, 53465562141, 0, 675976785,  218697133944, 0, 856647408, 0, 89463348,  586513437, 0, 52608914733, 0, 230253437646 },
  { "KRRBBKB", 479582110532, 0, 385118034, 0, 7636214,  173243212174, 0, 358464, 0, 1476510,  6159704, 0, 384759570, 0, 306338898358 },
  { "KRRBBKN", 494774973052, 3876744, 180889000, 0, 2661152,  173244352888, 0, 224052, 0, 470208,  2190944, 0, 180664948, 3876744, 321530620164 },
  { "KRRBBKQ", 331730124110, 5869452, 76745995384, 328823556, 4014022174,  168135572528, 384664, 4904616676, 1377680, 203095600,  3810926574, 327445876, 71841378708, 5484788, 163594551582 },
  { "KRRBBKR", 454248829562, 169048, 2137660222, 0, 307254132,  173167557528, 688, 38440328, 0, 39048604,  268205528, 0, 2099219894, 168360, 281081272034 },
  { "KRRBNKB", 489103982348, 0, 235600366, 0, 3728840,  182612569134, 0, 165922, 0, 758866,  2969974, 0, 235434444, 0, 306491413214 },
  { "KRRBNKN", 504202743235, 424766, 124751151, 0, 2927570,  182612852778, 74, 132802, 0, 508268,  2419302, 0, 124618349, 424692, 321589890457 },
  { "KRRBNKQ", 334411661737, 58336799, 81928183430, 0, 5795099484,  176964227719, 5680912, 5402925073, 0, 240660218,  5554439266, 0, 76525258357, 52655887, 157447434018 },
  { "KRRBNKR", 463311813885, 118864, 2356871185, 0, 393555804,  182525338210, 640, 38303736, 0, 49851336,  343704468, 0, 2318567449, 118224, 280786475675 },
  { "KRRNNKB", 496892395612, 0, 529943104, 0, 586128,  190692778554, 0, 159566, 0, 169092,  417036, 0, 529783538, 0, 306199617058 },
  { "KRRNNKN", 512071639874, 0, 335794386, 0, 3025752,  190692539404, 0, 53232, 0, 514576,  2511176, 0, 335741154, 0, 321379100470 },
  { "KRRNNKQ", 326891403080, 15869234, 96046376124, 1987692, 7317258610,  183713356370, 2486896, 6675787394, 32800, 301443752,  7015814858, 1954892, 89370588730, 13382338, 143178046710 },
  { "KRRNNKR", 468845777132, 0, 4808278384, 0, 487917512,  190577886312, 0, 53566484, 0, 61654416,  426263096, 0, 4754711900, 0, 278267890820 },
  { "KRRRBKB", 461582997348, 0, 109090932, 0, 2375034,  154964031492, 0, 165780, 0, 448410,  1926624, 0, 108925152, 0, 306618965856 },
  { "KRRRBKN", 476569107378, 0, 110594466, 0, 2296638,  154964080872, 0, 147480, 0, 417330,  1879308, 0, 110446986, 0, 321605026506 },
  { "KRRRBKQ", 376555520850, 10671234, 16371765177, 0, 1606475949,  153717072768, 276348, 1101707796, 0, 145588770,  1460887179, 0, 15270057381, 10394886, 222838448082 },
  { "KRRRBKR", 437508060186, 33522, 638156682, 0, 267261108,  154879865028, 156, 53028570, 0, 31751928,  235509180, 0, 585128112, 33366, 282628195158 },
  { "KRRRNKB", 468464762328, 0, 94694082, 0, 230370,  161829682806, 0, 121122, 0, 65220,  165150, 0, 94572960, 0, 306635079522 },
  { "KRRRNKN", 483454909458, 0, 90023808, 0, 2288682,  161829405900, 0, 51330, 0, 411918,  1876764, 0, 89972478, 0, 321625503558 },
  { "KRRRNKQ", 378096744930, 25199094, 20835328911, 0, 2452383741,  160350780297, 1016667, 1315755513, 0, 162316671,  2290067070, 0, 19519573398, 24182427, 217745964633 },
  { "KRRRNKR", 444190483410, 0, 752029734, 0, 336221820,  161730745608, 0, 59423394, 0, 39700146,  296521674, 0, 692606340, 0, 282459737802 },
  { "KRRRRKB", 440132766444, 0, 218618244, 0, 0,  133621394016, 0, 173040, 0, 0,  0, 0, 218445204, 0, 306511372428 },
  { "KRRRRKN", 455123877600, 0, 213438432, 0, 1603824,  133621155288, 0, 106728, 0, 305040,  1298784, 0, 213331704, 0, 321502722312 },
  { "KRRRRKQ", 360216604332, 417144, 11893554744, 0, 1090778364,  132428226660, 18672, 1090144272, 0, 103177452,  987600912, 0, 10803410472, 398472, 227788377672 },
  { "KRRRRKR", 416388306564, 0, 467404572, 0, 214721736,  133521274848, 0, 76935576, 0, 23356632,  191365104, 0, 390468996, 0, 282867031716 },
  { "KBBBBPK", 1602695827104, 0, 23363052288, 0, 0,  571647787056, 0, 2201118096, 0, 0,  0, 0, 21161934192, 0, 1031048040048 },
  { "KBBBNPK", 1665771915390, 0, 5983720074, 0, 0,  619541447856, 0, 4213368, 0, 0,  0, 0, 5979506706, 0, 1046230467534 },
  { "KBBBPPK", 1268203394880, 0, 4745422896, 0, 0,  487442344320, 0, 210035376, 0, 0,  0, 0, 4535387520, 0, 780761050560 },
  { "KBBNNPK", 1708173742088, 0, 4398520380, 0, 0,  660360030768, 0, 2257460, 0, 0,  0, 0, 4396262920, 0, 1047813711320 },
  { "KBBNPPK", 1303721283548, 0, 2508490648, 0, 0,  520929842444, 0, 3493672, 0, 0,  0, 0, 2504996976, 0, 782791441104 },
  { "KBBPPPK", 989146160964, 0, 1640121000, 0, 0,  407884176816, 0, 24988236, 0, 0,  0, 0, 1615132764, 0, 581261984148 },
  { "KBNNNPK", 1744555722156, 0, 3649025136, 0, 0,  695992171464, 0, 2601588, 0, 0,  0, 0, 3646423548, 0, 1048563550692 },
  { "KBNNPPK", 1333652141980, 0, 1925047472, 0, 0,  550278342824, 0, 2408548, 0, 0,  0, 0, 1922638924, 0, 783373799156 },
  { "KBNPPPK", 1013636753424, 0, 1073798562, 0, 0,  431830692858, 0, 2742216, 0, 0,  0, 0, 1071056346, 0, 581806060566 },
  { "KBPPPPK", 765773990640, 0, 675730656, 0, 0,  336275309760, 0, 4233936, 0, 0,  0, 0, 671496720, 0, 429498680880 },
  { "KNNNNPK", 1775272744776, 0, 3170963496, 0, 0,  726232104792, 0, 1629240, 0, 0,  0, 0, 3169334256, 0, 1049040639984 },
  { "KNNNPPK", 1359225740052, 0, 1569600276, 0, 0,  575497763640, 0, 1138608, 0, 0,  0, 0, 1568461668, 0, 783727976412 },
  { "KNNPPPK", 1034713165404, 0, 792354600, 0, 0,  452627561376, 0, 841716, 0, 0,  0, 0, 791512884, 0, 582085604028 },
  { "KNPPPPK", 782982682392, 0, 423719712, 0, 0,  353235732360, 0, 492144, 0, 0,  0, 0, 423227568, 0, 429746950032 },
  { "KPPPPPK", 588801087000, 0, 258211080, 0, 0,  273467186760, 0, 275640, 0, 0,  0, 0, 257935440, 0, 315333900240 },
  { "KQBBBPK", 1483012978458, 0, 19707390000, 0, 0,  450510214728, 0, 179490, 0, 0,  0, 0, 19707210510, 0, 1032502763730 },
  { "KQBBNPK", 1522054994804, 0, 16862660750, 0, 0,  486707512496, 0, 168818, 0, 0,  0, 0, 16862491932, 0, 1035347482308 },
  { "KQBBPPK", 1157558186296, 0, 10824260364, 0, 0,  383085721312, 0, 287268, 0, 0,  0, 0, 10823973096, 0, 774472464984 },
  { "KQBNNPK", 1556635341968, 0, 14754990678, 0, 0,  519180115704, 0, 242702, 0, 0,  0, 0, 14754747976, 0, 1037455226264 },
  { "KQBNPPK", 1185588606454, 0, 9227091540, 0, 0,  409519018802, 0, 241112, 0, 0,  0, 0, 9226850428, 0, 776069587652 },
  { "KQBPPPK", 897771795942, 0, 5741357286, 0, 0,  320635770162, 0, 266154, 0, 0,  0, 0, 5741091132, 0, 577136025780 },
  { "KQNNNPK", 1586976105486, 0, 12922859532, 0, 0,  547688841246, 0, 149532, 0, 0,  0, 0, 12922710000, 0, 1039287264240 },
  { "KQNNPPK", 1210366421272, 0, 7874846828, 0, 0,  432944680036, 0, 149984, 0, 0,  0, 0, 7874696844, 0, 777421741236 },
  { "KQNPPPK", 917837956404, 0, 4733433120, 0, 0,  339694151040, 0, 121572, 0, 0,  0, 0, 4733311548, 0, 578143805364 },
  { "KQPPPPK", 691859496528, 0, 2788937904, 0, 0,  264478211088, 0, 45744, 0, 0,  0, 0, 2788892160, 0, 427381285440 },
  { "KQQBBPK", 1365403046984, 0, 32810747140, 0, 0,  346003785032, 0, 34852, 0, 0,  0, 0, 32810712288, 0, 1019399261952 },
  { "KQQBNPK", 1395874585142, 0, 30419041066, 0, 0,  374083609710, 0, 42258, 0, 0,  0, 0, 30418998808, 0, 1021790975432 },
  { "KQQBPPK", 1059412643656, 0, 20337705140, 0, 0,  294453868700, 0, 42016, 0, 0,  0, 0, 20337663124, 0, 764958774956 },
  { "KQQNNPK", 1423603365124, 0, 28004572276, 0, 0,  399397924280, 0, 38880, 0, 0,  0, 0, 28004533396, 0, 1024205440844 },
  { "KQQNPPK", 1081828418700, 0, 18492417488, 0, 0,  315024351464, 0, 46644, 0, 0,  0, 0, 18492370844, 0, 766804067236 },
  { "KQQPPPK", 817497382296, 0, 12014706120, 0, 0,  246634941984, 0, 29520, 0, 0,  0, 0, 12014676600, 0, 570862440312 },
  { "KQQQBPK", 1267005123000, 0, 43869004524, 0, 0,  258664148244, 0, 5040, 0, 0,  0, 0, 43868999484, 0, 1008340974756 },
  { "KQQQNPK", 1289908536564, 0, 42197009994, 0, 0,  279895559940, 0, 12378, 0, 0,  0, 0, 42196997616, 0, 1010012976624 },
  { "KQQQPPK", 976525308684, 0, 29126321628, 0, 0,  220355179848, 0, 12384, 0, 0,  0, 0, 29126309244, 0, 756170128836 },
  { "KQQQQPK", 1189431973800, 0, 49672739976, 0, 0,  186894739536, 0, 0, 0, 0,  0, 0, 49672739976, 0, 1002537234264 },
  { "KQQQRPK", 1236082625382, 0, 45953556834, 0, 0,  229826207976, 0, 0, 0, 0,  0, 0, 45953556834, 0, 1006256417406 },
  { "KQQRBPK", 1332109084664, 0, 36681106454, 0, 0,  316580179794, 0, 37084, 0, 0,  0, 0, 36681069370, 0, 1015528904870 },
  { "KQQRNPK", 1355650587194, 0, 34420939436, 0, 0,  337861511122, 0, 41268, 0, 0,  0, 0, 34420898168, 0, 1017789076072 },
  { "KQQRPPK", 1028833276280, 0, 22860261220, 0, 0,  266397077944, 0, 21476, 0, 0,  0, 0, 22860239744, 0, 762436198336 },
  { "KQQRRPK", 1291105597032, 0, 39266548336, 0, 0,  278162164920, 0, 6208, 0, 0,  0, 0, 39266542128, 0, 1012943432112 },
  { "KQRBBPK", 1450683335182, 0, 23526268636, 0, 0,  421999520172, 0, 109406, 0, 0,  0, 0, 23526159230, 0, 1028683815010 },
  { "KQRBNPK", 1481229197807, 0, 21060140953, 0, 0,  450079226941, 0, 137579, 0, 0,  0, 0, 21060003374, 0, 1031149970866 },
  { "KQRBPPK", 1126827367822, 0, 13258625986, 0, 0,  354789403736, 0, 151992, 0, 0,  0, 0, 13258473994, 0, 772037964086 },
  { "KQRNNPK", 1508320416740, 0, 18812944784, 0, 0,  474923236316, 0, 150968, 0, 0,  0, 0, 18812793816, 0, 1033397180424 },
  { "KQRNPPK", 1148869189554, 0, 11587264106, 0, 0,  375159881732, 0, 133848, 0, 0,  0, 0, 11587130258, 0, 773709307822 },
  { "KQRPPPK", 870149600910, 0, 6866417142, 0, 0,  294138833868, 0, 67272, 0, 0,  0, 0, 6866349870, 0, 576010767042 },
  { "KQRRBPK", 1406675440190, 0, 27232293722, 0, 0,  381697629868, 0, 129804, 0, 0,  0, 0, 27232163918, 0, 1024977810322 },
  { "KQRRNPK", 1430023382974, 0, 24892765320, 0, 0,  402706049616, 0, 124438, 0, 0,  0, 0, 24892640882, 0, 1027317333358 },
  { "KQRRPPK", 1087959959572, 0, 15357008400, 0, 0,  318020466608, 0, 63284, 0, 0,  0, 0, 15356945116, 0, 769939492964 },
  { "KQRRRPK", 1354720309506, 0, 29896219170, 0, 0,  332406520044, 0, 34392, 0, 0,  0, 0, 29896184778, 0, 1022313789462 },
  { "KRBBBPK", 1589343365916, 0, 10877094894, 0, 0,  548008873236, 0, 1613334, 0, 0,  0, 0, 10875481560, 0, 1041334492680 },
  { "KRBBNPK", 1627774679366, 0, 8575499436, 0, 0,  584139142808, 0, 1061754, 0, 0,  0, 0, 8574437682, 0, 1043635536558 },
  { "KRBBPPK", 1240737205288, 0, 4977613820, 0, 0,  460416562188, 0, 1818840, 0, 0,  0, 0, 4975794980, 0, 780320643100 },
  { "KRBNNPK", 1660955668836, 0, 7180927180, 0, 0,  615925495452, 0, 1126324, 0, 0,  0, 0, 7179800856, 0, 1045030173384 },
  { "KRBNPPK", 1267887209590, 0, 3942453836, 0, 0,  486532002504, 0, 1222842, 0, 0,  0, 0, 3941230994, 0, 781355207086 },
  { "KRBPPPK", 962234376540, 0, 2095716102, 0, 0,  381451756560, 0, 1219170, 0, 0,  0, 0, 2094496932, 0, 580782619980 },
  { "KRNNNPK", 1689230598720, 0, 6151290498, 0, 0,  643171048590, 0, 866388, 0, 0,  0, 0, 6150424110, 0, 1046059550130 },
  { "KRNNPPK", 1291236269260, 0, 3235310276, 0, 0,  509174347712, 0, 793744, 0, 0,  0, 0, 3234516532, 0, 782061921548 },
  { "KRNPPPK", 981347308926, 0, 1595446848, 0, 0,  400065141828, 0, 497034, 0, 0,  0, 0, 1594949814, 0, 581282167098 },
  { "KRPPPPK", 741359145960, 0, 704896824, 0, 0,  311893751208, 0, 113976, 0, 0,  0, 0, 704782848, 0, 429465394752 },
  { "KRRBBPK", 1545829937228, 0, 13753170628, 0, 0,  507372028872, 0, 1104744, 0, 0,  0, 0, 13752065884, 0, 1038457908356 },
  { "KRRBNPK", 1575402487136, 0, 11827400536, 0, 0,  535018891214, 0, 1022218, 0, 0,  0, 0, 11826378318, 0, 1040383595922 },
  { "KRRBPPK", 1201162973764, 0, 6505852212, 0, 0,  422371415964, 0, 971932, 0, 0,  0, 0, 6504880280, 0, 778791557800 },
  { "KRRNNPK", 1600839868604, 0, 10304009364, 0, 0,  558932846764, 0, 1056964, 0, 0,  0, 0, 10302952400, 0, 1041907021840 },
  { "KRRNPPK", 1222020085188, 0, 5468877896, 0, 0,  442191627704, 0, 897300, 0, 0,  0, 0, 5467980596, 0, 779828457484 },
  { "KRRPPPK", 927521278680, 0, 2554531656, 0, 0,  347198398956, 0, 294468, 0, 0,  0, 0, 2554237188, 0, 580322879724 },
  { "KRRRBPK", 1490247046188, 0, 16644274614, 0, 0,  454680258330, 0, 1088232, 0, 0,  0, 0, 16643186382, 0, 1035566787858 },
  { "KRRRNPK", 1512398945142, 0, 14845263360, 0, 0,  475033187196, 0, 1047066, 0, 0,  0, 0, 14844216294, 0, 1037365757946 },
  { "KRRRPPK", 1153274530392, 0, 7747712268, 0, 0,  375725358432, 0, 446148, 0, 0,  0, 0, 7747266120, 0, 777549171960 },
  { "KRRRRPK", 1426635437520, 0, 18666521136, 0, 0,  393091597056, 0, 387360, 0, 0,  0, 0, 18666133776, 0, 1033543840464 },
  { "KBBBBBK", 472463137920, 0, 44016132540, 0, 0,  154251886200, 0, 11628488340, 0, 0,  0, 0, 32387644200, 0, 318211251720 },
  { "KBBBBNK", 526481917020, 0, 5494112844, 0, 0,  181367711448, 0, 9422496, 0, 0,  0, 0, 5484690348, 0, 345114205572 },
  { "KBBBNNK", 543580073502, 0, 2399622972, 0, 0,  195379874478, 0, 926076, 0, 0,  0, 0, 2398696896, 0, 348200199024 },
  { "KBBNNNK", 556361942310, 0, 2014134246, 0, 0,  207776013264, 0, 1167372, 0, 0,  0, 0, 2012966874, 0, 348585929046 },
  { "KBNNNNK", 567331574604, 0, 1750014204, 0, 0,  218481538896, 0, 1153992, 0, 0,  0, 0, 1748860212, 0, 348850035708 },
  { "KNNNNNK", 576454437360, 0, 1588681680, 0, 0,  227443598640, 0, 624480, 0, 0,  0, 0, 1588057200, 0, 349010838720 },
  { "KQBBBBK", 470006761500, 0, 10732617216, 0, 0,  130140482772, 0, 24, 0, 0,  0, 0, 10732617192, 0, 339866278728 },
  { "KQBBBNK", 485640064068, 0, 7345061808, 0, 0,  142386207534, 0, 22422, 0, 0,  0, 0, 7345039386, 0, 343253856534 },
  { "KQBBNNK", 497531781062, 0, 6561277840, 0, 0,  153494079322, 0, 83660, 0, 0,  0, 0, 6561194180, 0, 344037701740 },
  { "KQBNNNK", 508118496042, 0, 5855767638, 0, 0,  163375259802, 0, 107958, 0, 0,  0, 0, 5855659680, 0, 344743236240 },
  { "KQNNNNK", 517318441068, 0, 5244020028, 0, 0,  171963532464, 0, 32712, 0, 0,  0, 0, 5243987316, 0, 345354908604 },
  { "KQQBBBK", 437778885480, 0, 12688490082, 0, 0,  99868479642, 0, 0, 0, 0,  0, 0, 12688490082, 0, 337910405838 },
  { "KQQBBNK", 447947587292, 0, 11991800036, 0, 0,  109340483332, 0, 8076, 0, 0,  0, 0, 11991791960, 0, 338607103960 },
  { "KQQBNNK", 457333446388, 0, 11234971934, 0, 0,  117969511442, 0, 10960, 0, 0,  0, 0, 11234960974, 0, 339363934946 },
  { "KQQNNNK", 465825284454, 0, 10461379146, 0, 0,  125687763936, 0, 3744, 0, 0,  0, 0, 10461375402, 0, 340137520518 },
  { "KQQQBBK", 409272871320, 0, 15904292310, 0, 0,  74578267710, 0, 0, 0, 0,  0, 0, 15904292310, 0, 334694603610 },
  { "KQQQBNK", 416755162278, 0, 15557982660, 0, 0,  81714247860, 0, 1158, 0, 0,  0, 0, 15557981502, 0, 335040914418 },
  { "KQQQNNK", 423778467120, 0, 15068163078, 0, 0,  88247734278, 0, 0, 0, 0,  0, 0, 15068163078, 0, 335530732842 },
  { "KQQQQBK", 387250301436, 0, 17152441524, 0, 0,  53803847040, 0, 0, 0, 0,  0, 0, 17152441524, 0, 333446454396 },
  { "KQQQQNK", 392386716396, 0, 17215693764, 0, 0,  59003514240, 0, 0, 0, 0,  0, 0, 17215693764, 0, 333383202156 },
  { "KQQQQQK", 371379605580, 0, 16318605420, 0, 0,  37099315080, 0, 0, 0, 0,  0, 0, 16318605420, 0, 334280290500 },
  { "KQQQQRK", 380399086992, 0, 16845896088, 0, 0,  46646087160, 0, 0, 0, 0,  0, 0, 16845896088, 0, 333752999832 },
  { "KQQQRBK", 401217072618, 0, 16556603880, 0, 0,  67174780578, 0, 0, 0, 0,  0, 0, 16556603880, 0, 334042292040 },
  { "KQQQRNK", 406775766846, 0, 16266962100, 0, 0,  72443833026, 0, 0, 0, 0,  0, 0, 16266962100, 0, 334331933820 },
  { "KQQQRRK", 391573633212, 0, 16470772386, 0, 0,  57445509678, 0, 0, 0, 0,  0, 0, 16470772386, 0, 334128123534 },
  { "KQQRBBK", 429194575654, 0, 14050130978, 0, 0,  92645801228, 0, 9484, 0, 0,  0, 0, 14050121494, 0, 336548774426 },
  { "KQQRBNK", 437059160600, 0, 13398192062, 0, 0,  99858445910, 0, 10832, 0, 0,  0, 0, 13398181230, 0, 337200714690 },
  { "KQQRNNK", 444264633694, 0, 12689301838, 0, 0,  106355029852, 0, 9760, 0, 0,  0, 0, 12689292078, 0, 337909603842 },
  { "KQQRRBK", 417963552888, 0, 14905181394, 0, 0,  82269834834, 0, 3528, 0, 0,  0, 0, 14905177866, 0, 335693718054 },
  { "KQQRRNK", 423774571432, 0, 14361887852, 0, 0,  87537563360, 0, 4, 0, 0,  0, 0, 14361887848, 0, 336237008072 },
  { "KQQRRRK", 405123463284, 0, 15094864548, 0, 0,  69619431912, 0, 0, 0, 0,  0, 0, 15094864548, 0, 335504031372 },
  { "KQRBBBK", 464362339206, 0, 9839876664, 0, 0,  123603305502, 0, 14448, 0, 0,  0, 0, 9839862216, 0, 340759033704 },
  { "KQRBBNK", 474768858842, 0, 8989003804, 0, 0,  133158935322, 0, 31404, 0, 0,  0, 0, 8988972400, 0, 341609923520 },
  { "KQRBNNK", 484136843784, 0, 8179647508, 0, 0,  141717547450, 0, 47922, 0, 0,  0, 0, 8179599586, 0, 342419296334 },
  { "KQRNNNK", 492385848672, 0, 7433160516, 0, 0,  149220086022, 0, 27246, 0, 0,  0, 0, 7433133270, 0, 343165762650 },
  { "KQRRBBK", 452365943750, 0, 11251497102, 0, 0,  113018506848, 0, 38084, 0, 0,  0, 0, 11251459018, 0, 339347436902 },
  { "KQRRBNK", 460348255392, 0, 10460420226, 0, 0,  120209749542, 0, 30156, 0, 0,  0, 0, 10460390070, 0, 340138505850 },
  { "KQRRNNK", 467467997430, 0, 9694715454, 0, 0,  126563794888, 0, 22076, 0, 0,  0, 0, 9694693378, 0, 340904202542 },
  { "KQRRRBK", 437607418512, 0, 12245852286, 0, 0,  99254360064, 0, 14814, 0, 0,  0, 0, 12245837472, 0, 338353058448 },
  { "KQRRRNK", 443453234049, 0, 11581384599, 0, 0,  104435722458, 0, 270, 0, 0,  0, 0, 11581384329, 0, 339017511591 },
  { "KQRRRRK", 421237860528, 0, 12658116780, 0, 0,  83297081388, 0, 0, 0, 0,  0, 0, 12658116780, 0, 337940779140 },
  { "KRBBBBK", 503605510416, 0, 7609809720, 0, 0,  160615146396, 0, 1277820, 0, 0,  0, 0, 7608531900, 0, 342990364020 },
  { "KRBBBNK", 519259012146, 0, 4292403540, 0, 0,  172952169120, 0, 350646, 0, 0,  0, 0, 4292052894, 0, 346306843026 },
  { "KRBBNNK", 530878575074, 0, 3667570038, 0, 0,  183946863664, 0, 385528, 0, 0,  0, 0, 3667184510, 0, 346931711410 },
  { "KRBNNNK", 540946218990, 0, 3176520786, 0, 0,  193523542368, 0, 301488, 0, 0,  0, 0, 3176219298, 0, 347422676622 },
  { "KRNNNNK", 549422273664, 0, 2807148312, 0, 0,  201630407352, 0, 118704, 0, 0,  0, 0, 2807029608, 0, 347791866312 },
  { "KRRBBBK", 494429005422, 0, 6518046582, 0, 0,  150347838036, 0, 318048, 0, 0,  0, 0, 6517728534, 0, 344081167386 },
  { "KRRBBNK", 504722418990, 0, 5731976690, 0, 0,  159855211118, 0, 288642, 0, 0,  0, 0, 5731688048, 0, 344867207872 },
  { "KRRBNNK", 513733114562, 0, 5067647718, 0, 0,  168201703620, 0, 162740, 0, 0,  0, 0, 5067484978, 0, 345531410942 },
  { "KRRNNNK", 521418082590, 0, 4520327442, 0, 0,  175339448286, 0, 65826, 0, 0,  0, 0, 4520261616, 0, 346078634304 },
  { "KRRRBBK", 478709053884, 0, 7805278032, 0, 0,  135915095160, 0, 340836, 0, 0,  0, 0, 7804937196, 0, 342793958724 },
  { "KRRRBNK", 486496962927, 0, 7069706853, 0, 0,  142967634222, 0, 139638, 0, 0,  0, 0, 7069567215, 0, 343529328705 },
  { "KRRRNNK", 493223138892, 0, 6430814844, 0, 0,  149055017688, 0, 40128, 0, 0,  0, 0, 6430774716, 0, 344168121204 },
  { "KRRRRBK", 460155518808, 0, 8746961676, 0, 0,  118303480764, 0, 103800, 0, 0,  0, 0, 8746857876, 0, 341852038044 },
  { "KRRRRNK", 465763586088, 0, 8133127848, 0, 0,  123297816960, 0, 1056, 0, 0,  0, 0, 8133126792, 0, 342465769128 },
  { "KRRRRRK", 440021543220, 0, 9192416580, 0, 0,  98615063880, 0, 0, 0, 0,  0, 0, 9192416580, 0, 341406479340 }
};
#if defined(__GNUC__)
#pragma GCC diagnostic pop
#endif