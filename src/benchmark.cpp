/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2020 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include <fstream>
#include <iostream>
#include <istream>
#include <vector>

#include "position.h"

using namespace std;

namespace {

const vector<string> Defaults = {
  "setoption name UCI_Chess960 value false",
  "3R3K/4ppp1/2p5/1nN1kpQN/8/r7/b4rP1/3R2n1 w - -",
  "2R5/8/3k4/3N2p1/4K1Q1/8/8/8 w - -",
  "5N2/2n1n3/2p5/4p3/1N1k4/1KR2B2/1Pp5/2r1B1b1 w - -",
  "1BK5/8/2p5/3k4/4bR2/4R3/8/2Q5 w - -",
  "4k3/8/5K2/8/5Q2/8/8/8 w - -",
  "4rrk1/1p3pp1/bnp5/4N3/p7/1B3P2/PPP3P1/2KR3R w - -",
  "3B1n2/KPpq1nN1/3p4/1B1p4/2pkp3/1R1NpR2/2PP3Q/2b2b2 w - -",
  "2B4r/1Pk1P3/2N5/2P1N1P1/5B2/4p3/1r5p/6bK w - -",
  "5R2/8/2p3B1/3k1N2/4R3/1K2b3/8/7Q w - -",
  "1R6/1p1k1p2/1R3P2/3b4/3K4/6B1/8/3Q4 w - -",
  "3r3r/pbp2pbp/1p5q/8/Q6P/B2B1PPN/1R2P1N1/2k1nK1R w - -",
  "3q4/3R4/1bn1R3/n7/K1N2N2/B1k1r3/Q7/5b2 w - -",
  "8/4K3/3N3B/3pk2n/6R1/8/8/4n3 w - -",

  // Mate and stalemate positions
  "6k1/3b3r/1p1p4/p1n2p2/1PPNpP1q/P3Q1p1/1R1RB1P1/5K2 b - - 0 1",
  "8/8/8/8/8/6k1/6p1/6K1 w - -"
};

} // namespace

/// setup_bench() builds a list of UCI commands to be run by bench. There
/// are five parameters: TT size in MB, number of search threads that
/// should be used, the limit value spent for each position, a file name
/// where to look for positions in FEN format and the type of the limit:
/// depth, perft, nodes and movetime (in millisecs).
///
/// bench -> search default positions up to depth 13
/// bench 64 1 15 -> search default positions up to depth 15 (TT = 64MB)
/// bench 64 4 5000 current movetime -> search current position with 4 threads for 5 sec
/// bench 64 1 100000 default nodes -> search default positions for 100K nodes each
/// bench 16 1 5 default perft -> run a perft 5 on default positions

vector<string> setup_bench(const Position& current, istream& is) {

  vector<string> fens, list;
  string go, token;

  // Assign default values to missing arguments
  string ttSize    = (is >> token) ? token : "16";
  string threads   = (is >> token) ? token : "1";
  string limit     = (is >> token) ? token : "2";
  string fenFile   = (is >> token) ? token : "default";
  string limitType = (is >> token) ? token : "mate";

  go = limitType == "eval" ? "eval" : "go " + limitType + " " + limit;

  if (fenFile == "default")
      fens = Defaults;

  else if (fenFile == "current")
      fens.push_back(current.fen());

  else
  {
      string fen;
      ifstream file(fenFile);

      if (!file.is_open())
      {
          cerr << "Unable to open file " << fenFile << endl;
          exit(EXIT_FAILURE);
      }

      while (getline(file, fen))
          if (!fen.empty())
              fens.push_back(fen);

      file.close();
  }

  list.emplace_back("setoption name Threads value " + threads);
  list.emplace_back("setoption name Hash value " + ttSize);
  list.emplace_back("ucinewgame");

  for (const string& fen : fens)
      if (fen.find("setoption") != string::npos)
          list.emplace_back(fen);
      else
      {
          list.emplace_back("position fen " + fen);
          list.emplace_back(go);
      }

  return list;
}
