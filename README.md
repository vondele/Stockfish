# Matefish
Matefish is a mate-solving program for standard checkmate problems.
It is a UCI chess engine based on Stockfish 11, yet the search has completely been rewritten
for the purpose to solve checkmate problems as fast as possible.

It is mainly inspired by the publication REALIZATION OF THE CHESS MATE SOLVER APPLICATION
http://www.doiserbia.nb.rs/img/doi/0354-0243/2004/0354-02430402273V.pdf
and discussions on Talkchess and the german CSS forum.

It's still way beyond the functionality and speed offered by ChestUCI or Gustav.
Matefish is best used in a terminal or Windows command line.


# Examples
```
position fen 8/8/8/8/2Np4/3N4/k1K5/8 w - -
go mate 4
info depth 1 seldepth 1 multipv 1 score cp 0 nodes 2 nps 1000 tbhits 0 time 2 pv d3c1
info depth 3 seldepth 3 multipv 1 score cp 0 nodes 68 nps 34000 tbhits 0 time 2 pv d3c1
info depth 5 seldepth 5 multipv 1 score cp 0 nodes 1086 nps 543000 tbhits 0 time 2 pv d3c1
info string Success! Mate in 4 found!
info depth 7 seldepth 7 multipv 1 score mate 4 nodes 1570 nps 785000 tbhits 0 time 2 pv d3b4 a2a1 c4a3 d4d3 c2b3 d3d2 b4c2
bestmove d3b4 ponder a2a1

position fen 6r1/p1pq1p1p/1p1p1Qnk/3PrR2/2n1P1PP/P1P5/4R3/6K1 w - -
go depth 11
info depth 1 seldepth 1 multipv 1 score cp 0 nodes 5 nps 2500 tbhits 0 time 2 pv f6g6
info string Success! Mate in 11 found!
info depth 3 seldepth 21 multipv 1 score mate 11 nodes 3807 nps 1269000 tbhits 0 time 3 pv f5h5 e5h5 g4g5 h5g5 h4g5 h6h5 e2h2 h5g4 h2g2 g4h5 f6f3 h5h4 f3g3 h4h5 g2h2 d7h3 g3h3 g6h4 h3h4 h5g6 h4h6
bestmove f5h5 ponder e5h5
```

The second example also demonstrates that now 'go depth x' and 'go mate x' commands are handled the same way!
It also shows that checks get always extended so that forced checkmates will be found noticeably faster!

As a special feature, Matefish is able to construct a mating sequence with the help of the Syzygy endgame bases.
This works for very basic endings only. (Specifically KQK, KRK, KBBK, KBNK and KNNNK.)

```
position fen 4k3/8/8/8/8/8/8/2B1K1N1 w - - 0 1
go
info depth 57 seldepth 57 multipv 1 score mate 29 nodes 56 nps 1806 tbhits 651 time 31 pv e1d2 e8d7 d2c3 d7c6 c3c4 c6d7 c1f4 d7c6 g1e2 c6b7 c4b5 b7a7 e2c3 a7a8 c3a4 a8a7 a4b6 a7b7 f4h2 b7a7 b5c6 a7a6 h2b8 a6a5 b6d5 a5a4 b8e5 a4b3 d5e3 b3b4 c6b6 b4a4 e5c3 a4b3 c3e1 b3a4 e1d2 a4b3 b6a5 b3a3 d2e1 a3b2 a5a4 b2a2 e3d1 a2a1 a4b3 a1b1 e1d2 b1a1 d2c1 a1b1 c1a3 b1a1 a3b2 a1b1 d1c3
bestmove e1d2 ponder e8d7

```


## Terms of use

Matefish is free, and distributed under the **GNU General Public License version 3**
(GPL v3). Essentially, this means you are free to do almost exactly
what you want with the program, including distributing it among your
friends, making it available for download from your website, selling
it (either by itself or as part of some bigger software package), or
using it as the starting point for a software project of your own.

The only real limitation is that whenever you distribute Matefish in
some way, you MUST always include the license and the full source code
(or a pointer to where the source code can be found) to generate the 
exact binary you are distributing. If you make any changes to the
source code, these changes must also be made available under the GPL v3.

For full details, read the copy of the GPL v3 found in the file named
*Copying.txt*.
