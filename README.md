## The Huntsman

**This time he's after the King!**


The Huntsman is a specialized version of [Stockfish](https://stockfishchess.org) with some tweaks
for faster mate-finding.

For evaluation of positions, it only uses the tweaked KingSafety part of the
handcrafted eval. Therefore, embedding and use of the NNUE file is switched off
by default. In addition, the search has been modified as well as the scoring
of moves in the movepicker class.

For more information about usage, the UCI options, etc, please refer to the
official Stockfish page.

Thanks to the Stockfish team, all contributors and testers.


## Terms of use

Stockfish is free, and distributed under the **GNU General Public License version 3**
(GPL v3). Essentially, this means you are free to do almost exactly
what you want with the program, including distributing it among your
friends, making it available for download from your website, selling
it (either by itself or as part of some bigger software package), or
using it as the starting point for a software project of your own.

The only real limitation is that whenever you distribute Stockfish in
some way, you MUST always include the full source code, or a pointer
to where the source code can be found, to generate the exact binary
you are distributing. If you make any changes to the source code,
these changes must also be made available under the GPL.

For full details, read the copy of the GPL v3 found in the file named
[*Copying.txt*](https://github.com/official-stockfish/Stockfish/blob/master/Copying.txt).
