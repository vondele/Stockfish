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

#ifndef ENDGAME_H_INCLUDED
#define ENDGAME_H_INCLUDED

#include <unordered_map>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

#include "position.h"
#include "types.h"


/// EndgameCode lists all supported endgame functions by corresponding codes

enum EndgameCode {

  EVALUATION_FUNCTIONS,
  KNNK,  // KNN vs K
  KNNKP, // KNN vs KP
  KXK,   // Generic "mate lone king" eval
  KBNK,  // KBN vs K
  KPK,   // KP vs K
  KRKP,  // KR vs KP
  KRKB,  // KR vs KB
  KRKN,  // KR vs KN
  KQKP,  // KQ vs KP
  KQKR,  // KQ vs KR

  SCALING_FUNCTIONS,
  KBPsK,   // KB and pawns vs K
  KQKRPs,  // KQ vs KR and pawns
  KRPKR,   // KRP vs KR
  KRPKB,   // KRP vs KB
  KRPPKRP, // KRPP vs KRP
  KPsK,    // K and pawns vs K
  KBPKB,   // KBP vs KB
  KBPPKB,  // KBPP vs KB
  KBPKN,   // KBP vs KN
  KNPK,    // KNP vs K
  KNPKB,   // KNP vs KB
  KPKP,    // KP vs KP

  KTKT     // King with any known Tablebase stats
};


struct TBStat {

    std::string code;

    // Total from whites perspective
    uint64_t tWins;
    uint64_t tCursedWins;
    uint64_t tDraws;
    uint64_t tBlessedLosses;
    uint64_t tLosses;

    // White to move from whites perspective
    uint64_t wWins;
    uint64_t wCursedWins;
    uint64_t wDraws;
    uint64_t wBlessedLosses;
    uint64_t wLosses;

    // Black to move from blacks perspective
    uint64_t bWins;
    uint64_t bCursedWins;
    uint64_t bDraws;
    uint64_t bBlessedLosses;
    uint64_t bLosses;

    float scoreRatio[COLOR_NB];
    float drawRatio[COLOR_NB];
    float winRatio[COLOR_NB];
    float lossRatio[COLOR_NB];

    void init_ratios()
    {
        //NOTE: Blessed or cursed results are discounted by 50%
        uint64_t wSum = wWins + wCursedWins + wDraws + wBlessedLosses + wLosses;
        uint64_t bSum = bWins + bCursedWins + bDraws + bBlessedLosses + bLosses;

        drawRatio[WHITE] = float(double(wDraws * 2 + wCursedWins + wBlessedLosses) / double(wSum * 2));
        drawRatio[BLACK] = float(double(bDraws * 2 + bCursedWins + bBlessedLosses) / double(bSum * 2));

        winRatio[WHITE] = float(double(wWins * 2 + wCursedWins) / double(wSum * 2));
        winRatio[BLACK] = float(double(bWins * 2 + bCursedWins) / double(bSum * 2));

        lossRatio[WHITE] = float(double(wLosses * 2 + wBlessedLosses) / double(wSum * 2));
        lossRatio[BLACK] = float(double(bLosses * 2 + bBlessedLosses) / double(bSum * 2));

        scoreRatio[WHITE] = winRatio[WHITE] + drawRatio[WHITE] * 0.5f;
        scoreRatio[BLACK] = winRatio[BLACK] + drawRatio[BLACK] * 0.5f;
    }

    float score_ratio(Color stm) const { return scoreRatio[stm]; }
    float draw_ratio(Color stm) const { return drawRatio[stm]; }
    float win_ratio(Color stm) const { return winRatio[stm]; }
    float loss_ratio(Color stm) const { return lossRatio[stm]; }
};

const int TB_STAT_COUNT = 1001;
extern TBStat TBstats[TB_STAT_COUNT];


/// Endgame functions can be of two types depending on whether they return a
/// Value or a ScaleFactor.

template<EndgameCode E> using
eg_type = typename std::conditional<(E < SCALING_FUNCTIONS), Value, ScaleFactor>::type;


/// Base and derived functors for endgame evaluation and scaling functions

template<typename T>
struct EndgameBase {

  explicit EndgameBase(Color c, const TBStat* _tbs = nullptr) : strongSide(c), weakSide(~c), tbs(_tbs) {}
  virtual ~EndgameBase() = default;
  virtual T operator()(const Position&) const = 0;
  virtual bool is_tbstat() const = 0;

  const Color strongSide, weakSide;
  const TBStat* tbs;
};


template<EndgameCode E, typename T = eg_type<E>>
struct Endgame : public EndgameBase<T> {

  explicit Endgame(Color c, const TBStat* _tbs = nullptr) : EndgameBase<T>(c, _tbs) {}
  T operator()(const Position&) const override;
  bool is_tbstat() const override { return EndgameBase<T>::tbs != nullptr; }
};


/// The Endgames namespace handles the pointers to endgame evaluation and scaling
/// base objects in two std::map. We use polymorphism to invoke the actual
/// endgame function by calling its virtual operator().

namespace Endgames {

  template<typename T> using Ptr = std::unique_ptr<EndgameBase<T>>;
  template<typename T> using Map = std::unordered_map<Key, Ptr<T>>;

  extern std::pair<Map<Value>, Map<ScaleFactor>> maps;

  void init();

  template<typename T>
  Map<T>& map() {
    return std::get<std::is_same<T, ScaleFactor>::value>(maps);
  }

  template<EndgameCode E, typename T = eg_type<E>>
  void add(const std::string& code, const TBStat* tbs = nullptr) {

    StateInfo st;
    map<T>()[Position().set(code, WHITE, &st).material_key()] = Ptr<T>(new Endgame<E>(WHITE, tbs));
    map<T>()[Position().set(code, BLACK, &st).material_key()] = Ptr<T>(new Endgame<E>(BLACK, tbs));
  }

  template<typename T>
  const EndgameBase<T>* probe(Key key) {
    auto it = map<T>().find(key);
    return it != map<T>().end() ? it->second.get() : nullptr;
  }
}

#endif // #ifndef ENDGAME_H_INCLUDED
