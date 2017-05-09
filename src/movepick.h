/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#ifndef MOVEPICK_H_INCLUDED
#define MOVEPICK_H_INCLUDED

#include <cstring>   // For std::memset

#include "movegen.h"
#include "position.h"
#include "types.h"

/// Map the FromTo moves to the legal moves (and MOVE_NONE and MOVE_NULL)
const int FROMTO_NB = 1794;
extern int FromToMap[4096];


/// HistoryStats records how often quiet moves have been successful or unsuccessful
/// during the current search, and is used for reduction and move ordering decisions.
struct HistoryStats {

  static const int Max = 1 << 28;

  int get(Color c, Move m) const { return table[c][FromToMap[from_to_bits(m)]]; }
  void clear() { std::memset(table, 0, sizeof(table)); }
  void update(Color c, Move m, int16_t v) {

    int fromto = FromToMap[from_to_bits(m)];

    const int16_t D = 324;

    assert(abs(int16_t(v)) <= D); // Consistency check for below formula

    table[c][fromto] -= int(table[c][fromto]) * abs(v) / D;
    table[c][fromto] += v * int16_t(32);
  }

private:
  int16_t table[COLOR_NB][FROMTO_NB];
};


/// A template struct, used to generate MoveStats and CounterMoveHistoryStats:
/// MoveStats store the move that refute a previous one.
/// CounterMoveHistoryStats is like HistoryStats, but with two consecutive moves.
/// Entries are stored using only the moving piece and destination square, hence
/// two moves with different origin but same destination and piece will be
/// considered identical.
template<typename T>
struct Stats {
  void clear() { std::memset(table, 0, sizeof(table)); }
  T&   operator[](Move m) {return table[FromToMap[from_to_bits(m)]]; }
  const T& operator[](Move m) const {return table[FromToMap[from_to_bits(m)]]; }
  void update(Move m1, Move m) { table[FromToMap[from_to_bits(m1)]] = m; }
  void update(Move m, int16_t v) {

    const int16_t D = 936;

    int fromto = FromToMap[from_to_bits(m)];

    assert(abs(int16_t(v)) <= D); // Consistency check for below formula

    table[fromto] -= int(table[fromto]) * abs(v) / D;
    table[fromto] += v * int16_t(32);
  }

private:
  T table[FROMTO_NB];
};

typedef Stats<Move> MoveStats;
typedef Stats<int16_t> CounterMoveStats;
typedef Stats<CounterMoveStats> CounterMoveHistoryStats;


/// MovePicker class is used to pick one pseudo legal move at a time from the
/// current position. The most important method is next_move(), which returns a
/// new pseudo legal move each time it is called, until there are no moves left,
/// when MOVE_NONE is returned. In order to improve the efficiency of the alpha
/// beta algorithm, MovePicker attempts to return the moves which are most likely
/// to get a cut-off first.
namespace Search { struct Stack; }

class MovePicker {
public:
  MovePicker(const MovePicker&) = delete;
  MovePicker& operator=(const MovePicker&) = delete;

  MovePicker(const Position&, Move, Value);
  MovePicker(const Position&, Move, Depth, Square);
  MovePicker(const Position&, Move, Depth, Search::Stack*);

  Move next_move(bool skipQuiets = false);

private:
  template<GenType> void score();
  ExtMove* begin() { return cur; }
  ExtMove* end() { return endMoves; }

  const Position& pos;
  const Search::Stack* ss;
  Move killers[2];
  Move countermove;
  Depth depth;
  Move ttMove;
  Square recaptureSquare;
  Value threshold;
  int stage;
  ExtMove *cur, *endMoves, *endBadCaptures;
  ExtMove moves[MAX_MOVES];
};

#endif // #ifndef MOVEPICK_H_INCLUDED
