/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2026 The Stockfish developers (see AUTHORS file)

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

#include "evaluate.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <memory>
#include <sstream>

#include "nnue/network.h"
#include "nnue/nnue_misc.h"
#include "position.h"
#include "types.h"
#include "uci.h"
#include "nnue/nnue_accumulator.h"


#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <cstdlib>  // For std::abort()
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <algorithm>

// RAII Class to manage the background Lc0 process
class Lc0Process {
   private:
    int   write_pipe[2] = {-1, -1};
    int   read_pipe[2]  = {-1, -1};
    pid_t pid           = -1;

    void send_command(const std::string& cmd) { write(write_pipe[1], cmd.c_str(), cmd.length()); }

    bool read_line(std::string& out_line) {
        out_line.clear();
        char ch;
        while (read(read_pipe[0], &ch, 1) > 0)
        {
            if (ch == '\n')
                return true;
            if (ch != '\r')
                out_line += ch;
        }
        return !out_line.empty();
    }

   public:
    Lc0Process() {
        if (pipe(write_pipe) == -1 || pipe(read_pipe) == -1)
        {
            std::cerr << "Error: Failed to create pipes for Lc0\n";
            std::abort();
        }

        pid = fork();
        if (pid == -1)
        {
            std::cerr << "Error: Failed to fork Lc0 process\n";
            std::abort();
        }

        if (pid == 0)
        {  // Child Process
            dup2(write_pipe[0], STDIN_FILENO);
            dup2(read_pipe[1], STDOUT_FILENO);

            // Close unused pipe ends in child
            close(write_pipe[1]);
            close(read_pipe[0]);
            close(STDERR_FILENO);  // Silencing stderr

            execl("/home/vondele/chess/lc0/build/release/lc0", "lc0", "-w",
                  "/home/vondele/chess/lc0/BT4-1024x15x32h-swa-6147500-policytune-332.pb.gz",
                  nullptr);

            // If execl fails, exit the child process immediately
            std::abort();
        }
        else
        {  // Parent Process
            close(write_pipe[0]);
            close(read_pipe[1]);

            // Initialize UCI protocol
            send_command("uci\nisready\n");
            std::string line;
            while (read_line(line) && line != "readyok")
            {
                // Wait until Lc0 is fully ready
            }
        }
    }

    ~Lc0Process() {
        if (pid > 0)
        {
            send_command("quit\n");
            close(write_pipe[1]);
            close(read_pipe[0]);
            waitpid(pid, nullptr, 0);
        }
    }

    // Evaluates a FEN string and returns the score in Stockfish internal units
    int eval(const std::string& fen) {
        send_command("position fen " + fen + "\ngo nodes 1\n");

        std::string line;
        int         last_score_cp   = 0;
        int         last_score_mate = 0;
        bool        has_mate        = false;

        // Parse UCI output
        while (read_line(line))
        {
            if (line.rfind("bestmove", 0) == 0)
            {
                break;  // Stop at the end of the search
            }

            if (line.rfind("info ", 0) == 0)
            {
                std::istringstream iss(line);
                std::string        token;
                while (iss >> token)
                {
                    if (token == "score")
                    {
                        iss >> token;  // read "cp" or "mate"
                        if (token == "cp")
                        {
                            iss >> last_score_cp;
                            has_mate = false;
                        }
                        else if (token == "mate")
                        {
                            iss >> last_score_mate;
                            has_mate = true;
                        }
                    }
                }
            }
        }

        // Convert score to Stockfish Internal Units (1 pawn = 280 units -> x 2.8)
        if (has_mate)
        {
            if (last_score_mate > 0)
            {
                return 5000 - (50 * last_score_mate);
            }
            else
            {
                return -5000 - (50 * last_score_mate);
            }
        }
        else
        {
            return static_cast<int>(last_score_cp * 2.8);
        }
    }
};

namespace Stockfish {

// Evaluate is the evaluator for the outer world. It returns a static evaluation
// of the position from the point of view of the side to move.
Value Eval::evaluate(const Eval::NNUE::Network&     network,
                     const Position&                pos,
                     Eval::NNUE::AccumulatorStack&  accumulators,
                     Eval::NNUE::AccumulatorCaches& caches,
                     int                            optimism) {

    assert(!pos.checkers());

    auto [psqt, positional] = network.evaluate(pos, accumulators, caches);

    Value nnue = (125 * psqt + 131 * positional) / 128;

    // Blend optimism and eval with nnue complexity
    int nnueComplexity = std::abs(psqt - positional);
    optimism += optimism * nnueComplexity / 476;
    nnue -= nnue * nnueComplexity / 18236;

    int material = 534 * pos.count<PAWN>() + pos.non_pawn_material();
    int v        = (nnue * (77871 + material) + optimism * (7191 + material)) / 77871;

    // Damp down the evaluation linearly when shuffling
    v -= v * pos.rule50_count() / 199;

    static Lc0Process lc0;
    int               lc0_eval = lc0.eval(pos.fen());

    // std::cout << "Lc0 evaluation: " << lc0_eval << " NNUE " << v << std::endl;
    v = lc0_eval;

    // Guarantee evaluation does not hit the tablebase range
    v = std::clamp(v, VALUE_TB_LOSS_IN_MAX_PLY + 1, VALUE_TB_WIN_IN_MAX_PLY - 1);

    return v;
}

// Like evaluate(), but instead of returning a value, it returns
// a string (suitable for outputting to stdout) that contains the detailed
// descriptions and values of each evaluation term. Useful for debugging.
// Trace scores are from white's point of view
std::string Eval::trace(Position& pos, const Eval::NNUE::Network& network) {

    if (pos.checkers())
        return "Final evaluation: none (in check)";

    auto accumulators = std::make_unique<Eval::NNUE::AccumulatorStack>();
    auto caches       = std::make_unique<Eval::NNUE::AccumulatorCaches>(network);

    std::stringstream ss;
    ss << std::showpoint << std::noshowpos << std::fixed << std::setprecision(2);
    ss << '\n' << NNUE::trace(pos, network, *caches) << '\n';

    ss << std::showpoint << std::showpos << std::fixed << std::setprecision(2) << std::setw(15);

    auto [psqt, positional] = network.evaluate(pos, *accumulators, *caches);
    Value v                 = psqt + positional;
    ss << "NNUE evaluation          " << v << " (side to move, internal units)\n";
    v = pos.side_to_move() == WHITE ? v : -v;
    ss << "NNUE evaluation        " << 0.01 * UCIEngine::to_cp(v, pos) << " (white side)\n";

    v = evaluate(network, pos, *accumulators, *caches, VALUE_ZERO);
    v = pos.side_to_move() == WHITE ? v : -v;

    ss << "Final evaluation      ";
    ss << 0.01 * UCIEngine::to_cp(v, pos) << " (white side)";
    ss << " [with scaled NNUE, ...]\n";

    return ss.str();
}

}  // namespace Stockfish
