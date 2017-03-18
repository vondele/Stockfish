#!/bin/bash
# report performance on the arasan testsuite

error()
{
  echo "arasan benchmarking failed on line $1"
  exit 1
}
trap 'error ${LINENO}' ERR

# define suitable post and prefixes for testing options
case $1 in
  --nodes)
    gocommand="go nodes $2"
  ;;
  --depth)
    gocommand="go depth $2"
  ;;
  --movetime)
    gocommand="go movetime $2"
  ;;
  *)
    echo "unknown option"
    false
  ;;
esac

echo "arasan benchmarking started with command : $gocommand"

# arasan 19a from http://www.arasanchess.org/testsuite.shtml
cat << EOF > arasan_data.txt
r1bq1r1k/p1pnbpp1/1p2p3/6p1/3PB3/5N2/PPPQ1PPP/2KR3R w - - 0 1;g2g4
r1b2rk1/1p1nbppp/pq1p4/3B4/P2NP3/2N1p3/1PP3PP/R2Q1R1K w - - 0 1;f1f7
r3r1kb/1npb1p1p/2q2np1/4p1B1/ppP1P2Q/1P2RNNP/P1B2PP1/3R2K1 w - - 0 1;g3f5
2rr3k/2qnbppp/p1n1p3/1p1pP3/3P1N2/1Q1BBP2/PP3P1P/1KR3R1 w - - 0 1;d3h7
3q1r1k/1b3ppp/p1n5/1p1pPB2/2rP4/P6N/1P2Q1PP/R4RK1 w - - 0 1;e2h5
r1b1k2r/1p1pppb1/p5pp/3P4/q2p1B2/3P1Q2/PPP2PPP/R3R1K1 w kq - 0 1;e1e7
R4bk1/2Bbp2p/2p2pp1/1rPp4/3P4/4P2P/4BPPK/1q1Q4 w - - 0 1;d1a4
r2q1rk1/5ppp/3b4/1nnP2P1/1Q3BbP/Pp4N1/1P4B1/1KN2R1R b - - 0 1;b5a3
r7/5pk1/5p2/2p5/pp6/1P1B1KPN/P6P/8 b - - 0 1;c5c4
r2q3r/1p1bbQ2/4p1Bk/3pP3/1n1P1P1p/pP6/Pn4PP/R1B1R1K1 w - - 0 1;g2g4
1r2brk1/4n1p1/4p2p/p2pP1qP/2pP1NP1/P1Q1BK2/2P4R/6R1 b - - 0 1;e8g6
1rb2k1r/2q2pp1/p2b3p/2n3B1/2QN4/3B4/PpP3PP/1K2R2R w - - 0 1;g5d8
5rk1/1pp3p1/3ppr1p/pP2p2n/4P2q/P2PQ2P/2P1NPP1/R4RK1 b - - 0 1;f6f3
r4rk1/1b1n1pb1/3p2p1/1p1Pq1Bp/2p1P3/2P2RNP/1PBQ2P1/5R1K w - - 0 1;g3f5
2kr2r1/ppq1bp1p/4pn2/2p1n1pb/4P1P1/2P2N1P/PPBNQP2/R1B1R1K1 b - - 0 1;f6g4
rn1q1rk1/2pbb3/pn2p3/1p1pPpp1/3P4/1PNBBN2/P1P1Q1PP/R4R1K w - - 0 1;f3g5
3r1rk1/q4pp1/n1bNp2p/p7/pn2P1N1/6P1/1P1Q1PBP/2RR2K1 w - - 0 1;g4h6
r1q2rk1/ppnbbpp1/n4P1p/4P3/3p4/2N1B1PP/PP4BK/R2Q1R2 w - - 0 1;e3h6
1R6/5p1k/4bPpp/3pN3/2pP1P1P/2r5/6PK/8 w - - 0 1;h4h5
3q1rk1/pr1b1p1p/1bp2p2/2ppP3/8/2P1BN2/PPQ3PP/R4RK1 w - - 0 1;e3h6
8/5pk1/p4npp/1pPN4/1P2p3/1P4PP/5P2/5K2 w - - 0 1;d5f6
8/6p1/P1b1pp2/2p1p3/1k4P1/3PP3/1PK5/5B2 w - - 0 1;f1g2
r5n1/p1p1q2k/4b2p/3pB3/3PP1pP/8/PPPQ2P1/5RK1 w - - 0 1;d2f4
2b2rk1/r3q1pp/1nn1p3/3pP1NP/p1pP2Q1/2P1N3/1P1KBP2/R5R1 w - - 0 1;g5h7
rnb3k1/p3qpr1/2p1p3/2NP3p/1pP3p1/3BQ3/P4PP1/4RRK1 w - - 0 1;e3d4
r3r1k1/p3bppp/q1b2n2/5Q2/1p1B4/1BNR4/PPP3PP/2K2R2 w - - 0 1;d3g3
rn1rb1k1/pq2bppp/4p3/2p1N3/4PQ2/1PB3P1/P4PBP/2R2RK1 w - - 0 1;e5g4
3q1r1k/2r2pp1/p6p/1pbppP1N/3pP1PP/3P1Q2/PPP4R/5RK1 w - - 0 1;g4g5
1q6/6k1/5Np1/1r4Pp/2p4P/2Nrb3/PP6/KR5Q b - - 0 1;e3d4
b2rk3/r4p2/p3p3/P3Q1Np/2Pp3P/8/6P1/6K1 w - - 0 1;e5h8
2kr1b1r/1pp1ppp1/p7/q2P3n/2BB1pb1/2NQ4/P1P1N3/1R3RK1 w - - 0 1;b1b7
r1br2k1/ppp2q1p/nb3p2/6p1/2PN1B2/2P2B2/P1Q2PPP/3RR1K1 w - - 0 1;d4c6
br4k1/1qrnbppp/pp1ppn2/8/NPPBP3/PN3P2/5QPP/2RR1B1K w - - 0 1;a4b6
r2q1rk1/ppp2p2/3p1np1/4pNQ1/4P1pP/1PPP4/1P3P2/R3K1R1 w Q - 0 1;g5h6
1qb2rk1/3p1pp1/1p6/1pbBp3/r5p1/3QB3/PPP2P1P/2KR2R1 w - - 0 1;b2b3
r1b2q1k/2Qn1p1p/1p1Rpp2/p6B/4P2P/6N1/P4PP1/6K1 w - - 0 1;e4e5
r2q1rk1/p2pn3/bpp2p1p/3Nb1pQ/7B/8/PPB2PPP/R3R1K1 w - - 0 1;h4g5
r4rk1/p4ppp/qp2p3/b5B1/n1R5/5N2/PP2QPPP/1R4K1 w - - 0 1;g5f6
r2q1rk1/4bppp/3pb3/2n1pP2/1p2P1PP/1P3Q2/1BP1N1B1/2KR3R b - - 0 1;a8a2
2r2rkb/1Q1b3p/p2p3q/2PPnp2/1P2p1p1/2N5/P3BPPB/4RRK1 b - - 0 1;e4e3
2b1rk2/5p2/p1P5/2p2P2/2p5/7B/P7/2KR4 w - - 0 1;f5f6
rn1qr1k1/1p2bppp/p3p3/3pP3/P2P1B2/2RB1Q1P/1P3PP1/R5K1 w - - 0 1;d3h7
4n3/2p5/1p2r2P/p1p2R2/P1N1k3/1PP4K/8/8 w - - 0 1;f5e5
1n3rk1/3rbppp/p2p4/4pP2/Ppq1P3/1N2B3/1PP3PP/R2Q1R1K w - - 0 1;f5f6
8/2p1k3/3p3p/2PP1pp1/1P1K1P2/6P1/8/8 w - - 0 1;g3g4
r1b2rk1/pp2bppp/3p4/q7/3BN1n1/1B3Q2/PPP3PP/R4RK1 w - - 0 1;f3f7
r1b1rk2/p1pq2p1/1p1b1p1p/n2P4/2P1NP2/P2B1R2/1BQ3PP/R6K w - - 0 1;e4f6
r2qr3/2p1b1pk/p5pp/1p2p3/nP2P1P1/1BP2RP1/P3QPK1/R1B5 w - - 0 1;c1h6
1rbq1rk1/p5bp/3p2p1/2pP4/1p1n1BP1/3P3P/PP2N1B1/1R1Q1RK1 b - - 0 1;c8g4
k1b4r/1p3p2/pq2pNp1/5n1p/P3QP2/1P1R1BPP/2P5/1K6 b - - 0 1;f5g3
7b/8/kq6/8/8/1N2R3/K2P4/8 w - - 0 1;b3d4
q3nrk1/4bppp/3p4/r3nPP1/4P2P/NpQ1B3/1P4B1/1K1R3R b - - 0 1;e8c7
2r5/8/6k1/P1p3p1/2R5/1P1q4/1K4Q1/8 w - - 0 1;a5a6
8/3R1P2/1ppP1p2/3r4/8/K7/p4k2/8 w - - 0 1;a3b2
2qrrbk1/1b3ppp/pn1Pp3/6P1/1Pp2B2/1nN2NQB/1P3P1P/3RR1K1 w - - 0 1;g5g6
r3r1k1/pp3p1p/3bb1BB/4q1Q1/8/7P/P4PP1/R2R2K1 w - - 0 1;d1d6
5rk1/pp3ppp/3q4/8/2Pp2b1/P5Pn/PBQPr1BP/4RR1K b - - 0 1;e2g2
3b3r/1q3pk1/4b2p/3pPppQ/R1pP1P1P/1rP1N1P1/6N1/2R3K1 w - - 0 1;g3g4
r1b2rk1/pp1p2pR/8/1pb2p2/5N2/7Q/qPPB1PPP/6K1 w - - 0 1;g2g3
7q/3k2p1/n1p1p1Pr/1pPpPpQ1/3P1N1p/1P2KP2/6P1/7R w - - 0 1;f4d5
5rk1/8/pqPp1r1p/1p1Pp1bR/4B3/5PP1/PP2Q1K1/R7 w - - 0 1;h5g5
3r2k1/pb3Np1/4pq1p/2pp1n2/3P4/1PQ5/P4PPP/R2R2K1 b - - 0 1;f5d4
2kr1r2/ppq1b1p1/2n5/2PpPb1N/QP1B1pp1/2P5/P2N1P1P/R4RK1 b - - 0 1;f8h8
r1r2k2/pp2bpp1/2bppn1p/6B1/2qNPPPP/2N5/PPPQ4/1K1RR3 w - - 0 1;f4f5
3r2k1/6p1/B1R2p1p/1pPr1P2/3P4/8/1P3nP1/2KR4 w - - 0 1;c6c8
3qb1k1/5rb1/r3p1Np/1n1pP2P/p1pB1PQ1/2P5/R1B4K/6R1 w - - 0 1;d4c5
3q1k2/p4pb1/3Pp3/p3P3/r6p/2QB3P/3B1P2/6K1 w - - 0 1;d3b5
r4r1k/ppqbn1pp/3b1p2/2pP3B/2P4N/7P/P2B1PP1/1R1QR1K1 w - - 0 1;e1e7
1r4k1/1q3pp1/r3b2p/p2N4/3R4/QP3P2/2P3PP/1K1R4 w - - 0 1;d5f6
r2q1r2/1bp1npk1/3p1p1p/p3p2Q/P3P2N/1BpPP3/1P1N2PP/5RK1 w - - 0 1;f1f3
2k1r2r/1pqb1p2/p2pp2b/4n1p1/PQ1NP2p/1P3P1P/2P1NBP1/R4RK1 w - - 0 1;d4b5
2r3r1/1p1qb2k/p5pp/2n1Bp2/2RP3P/1P2PNQ1/5P1K/3R4 w - - 0 1;f3g5
rn3rk1/pp1q3p/4p1B1/2p5/3N1b2/4B3/PPQ2PPP/3R2K1 w - - 0 1;d4f5
rr5k/1q2pPbp/3p2p1/PbpP4/1nB1nP1Q/1NB5/1P4PP/R4R1K w - - 0 1;f4f5
r4rk1/pp1qbppp/1n6/6R1/P1pP4/5Q1P/2B2PP1/2B2RK1 w - - 0 1;g5g7
6k1/1p1q4/p1rP2p1/5p1p/5Q2/1P5P/5PP1/3R2K1 w - - 0 1;g2g4
1qrrbbk1/1p1nnppp/p3p3/4P3/2P5/1PN1N3/PB2Q1PP/1B2RR1K w - - 0 1;b1h7
r1b2rk1/qp5p/p1n1ppp1/7N/4P1P1/2N1pP2/PPP5/2KR1QR1 w - - 0 1;e4e5
3r4/2q5/5pk1/p3n1p1/N3Pp1p/1PPr1P1P/2Q1R1P1/5R1K b - - 0 1;g5g4
4rnk1/3bbpp1/3p2n1/1pqP2PB/2P2B2/1P6/4N1KQ/3R1R2 w - - 0 1;f1f2
r1b1k2r/2q2pp1/p1p1pn2/2b4p/Pp2P3/3B3P/1PP1QPP1/RNB2RK1 b kq - 0 1;f6g4
2r1rb1k/ppq2pp1/4b2p/3pP2Q/5B2/2PB2R1/P4PPP/1R4K1 w - - 0 1;g3g7
6k1/p4qp1/1p3r1p/2pPp1p1/1PP1PnP1/2P1KR1P/1B6/7Q b - - 0 1;h6h5
rnb1kb1r/pp1p1ppp/1q2p3/8/3NP1n1/2N1B3/PPP2PPP/R2QKB1R w KQkq - 0 1;d1g4
r3kb1r/1b1n2p1/p3Nn1p/3Pp3/1p4PP/3QBP2/qPP5/2KR1B1R w kq - 0 1;d3g6
1r1qrbk1/pb3p2/2p1pPpp/1p4B1/2pP2PQ/2P5/P4PBP/R3R1K1 w - - 0 1;g5h6
2r1r2k/1b1n1p1p/p3pPp1/1p1pP2q/3N4/P3Q1P1/1PP4P/2KRRB2 w - - 0 1;g3g4
2r1b1k1/5p2/1R2nB2/1p2P2p/2p5/2Q1P2K/3R1PB1/r3q3 w - - 0 1;b6e6
rn2r1k1/ppq1pp1p/2b2bp1/8/2BNPP1B/2P4P/P1Q3P1/1R3RK1 w - - 0 1;c4f7
1kr5/1p3p2/q3p3/pRbpPp2/P1rNnP2/2P1B1Pp/1P2Q2P/R5K1 b - - 0 1;c5d4
r3r2k/1pq2pp1/4b2p/3pP3/p1nB3P/P2B1RQ1/1PP3P1/3R3K w - - 0 1;f3f6
r3brk1/2q1bp1p/pnn3p1/1p1pP1N1/3P4/3B2P1/PP1QNR1P/R1B3K1 w - - 0 1;g5h7
1r3r2/q5k1/4p1n1/1bPpPp1p/pPpR1Pp1/P1B1Q3/2B3PP/3R2K1 w - - 0 1;d4d5
rq3rk1/1b1n1ppp/ppn1p3/3pP3/5B2/2NBP2P/PP2QPP1/2RR2K1 w - - 0 1;c3d5
7r/k4pp1/pn2p1pr/2ppP3/1q3P2/1PN2R1P/P1P2QP1/3R3K w - - 0 1;a2a3
1r3rk1/3bbppp/1qn2P2/p2pP1P1/3P4/2PB1N2/6K1/qNBQ1R2 w - - 0 1;d3h7
1r1qrbk1/5ppp/2b1p2B/2npP3/1p4QP/pP1B1N2/P1P2PP1/1K1R3R w - - 0 1;d3h7
r5k1/pbpq1pp1/3b2rp/N3n3/1N6/2P3B1/PP1Q1PPP/R4RK1 b - - 0 1;g6g3
1r2r1k1/2R2p2/1N1Rp2p/p2b3P/4pPP1/8/P4K2/8 w - - 0 1;g4g5
r4r2/pp1b1ppk/2n1p3/3pPnB1/q1pP2QP/P1P4R/2PKNPP1/R7 w - - 0 1;g4h5
8/2k2Bp1/2n5/p1P4p/4pPn1/P3PqPb/1r1BQ2P/2R1K1R1 b - - 0 1;c6e5
8/5kpp/8/8/8/5P2/1RPK2PP/6r1 w - - 0 1;c2c4
r3rnk1/pp2ppb1/1np3p1/3qP2p/3P1B2/4Q1N1/PP2BPP1/1K1R3R w - - 0 1;f4h6
1r1q2k1/2r3bp/B2p1np1/3P1p2/R1P1pP2/4B2P/P5PK/3Q1R2 b - - 0 1;f6g4
2r1rnk1/1p2pp1p/p1np2p1/q4PP1/3NP2Q/4B2R/PPP4P/3R3K w - - 0 1;b2b4
2b2qk1/1r4pp/2p1p3/p2n1PPB/2p4P/2p5/P4Q2/4RRK1 w - - 0 1;f2g3
1r1rkb2/2q2p2/p2p1P1B/P1pPp2Q/2P3b1/1P6/2B3PP/5R1K w - - 0 1;h5g4
r4rk1/3b3p/p1pb4/1p1n2p1/2P2p2/1B1P2Pq/PP1NRP1P/R1BQ2K1 w - - 0 1;d1f1
1r3rk1/4bpp1/p3p2p/q1PpPn2/bn3Q1P/1PN1BN2/2P1BPP1/1KR2R2 b - - 0 1;a4b3
2nb2k1/1rqb1pp1/p2p1n1p/2pPp3/P1P1P3/2B1NN1P/2B2PP1/Q3R2K w - - 0 1;f3e5
3r2k1/p1qn1p1p/4p1p1/2p1N3/8/2P3P1/PP2QP1P/4R1K1 w - - 0 1;e5f7
r2q1rk1/pb1nbp1p/1pp1pp2/8/2BPN2P/5N2/PPP1QPP1/2KR3R w - - 0 1;f3g5
4rr2/3bp1bk/p2q1np1/2pPp2p/2P4P/1R4N1/1P1BB1P1/1Q3RK1 w - - 0 1;e2h5
8/8/4b1p1/2Bp3p/5P1P/1pK1Pk2/8/8 b - - 0 1;g6g5
8/5p2/3p2p1/1bk4p/p2pBNnP/P5P1/1P3P2/4K3 b - - 0 1;d4d3
8/4nk2/1p3p2/1r1p2pp/1P1R1N1P/6P1/3KPP2/8 w - - 0 1;f4d3
6k1/1bq1bpp1/p6p/2p1pP2/1rP1P1P1/2NQ4/2P4P/K2RR3 b - - 0 1;b7d5
r3r1k1/1bqnbp1p/pp1pp1p1/6P1/Pn2PP1Q/1NN1BR2/1PPR2BP/6K1 w - - 0 1;f3h3
4rrk1/1pp1n1pp/1bp1P2q/p4p2/P4P2/3R2N1/1PP2P1P/2BQRK2 w - - 0 1;g3h5
3q4/4k3/1p1b1p1r/p2Q4/3B1p1p/7P/1P4P1/3R3K w - - 0 1;b2b4
8/5p1k/6p1/1p1Q3p/3P4/1R2P1KP/6P1/r4q2 b - - 0 1;h5h4
7k/3q1pp1/1p3r2/p1bP4/P1P2p2/1P2rNpP/2Q3P1/4RR1K b - - 0 1;e3f3
3r3r/k1p2pb1/B1b2q2/2RN3p/3P2p1/1Q2B1Pn/PP3PKP/5R2 w - - 0 1;f1c1
r1b3kr/pp1n2Bp/2pb2q1/3p3N/3P4/2P2Q2/P1P3PP/4RRK1 w - - 0 1;e1e5
2r3k1/1q3pp1/2n1b2p/4P3/3p1BP1/Q6P/1p3PB1/1R4K1 b - - 0 1;c8b8
rn2kb1r/1b1n1p1p/p3p1p1/1p2q1B1/3N3Q/2N5/PPP3PP/2KR1B1R w kq - 0 1;d4e6
r7/ppp3kp/2bn4/4qp2/2B1pR2/2P1Q2P/P5P1/5RK1 w - - 0 1;f4f5
8/2p4k/1p1p4/1PPPp3/1r6/R3K3/P4P2/8 w - - 0 1;c5c6
1nr3k1/q4rpp/1p1p1n2/3Pp3/1PQ1P1b1/4B1P1/2R2NBP/2R3K1 w - - 0 1;c4c8
r2q2k1/1b1nrpb1/p2p2p1/1pnP3p/P1p1P2R/2P1B1NP/1PB2QPN/5RK1 w - - 0 1;h4h5
5rk1/2p1R2p/r5q1/2pPR2p/5p2/1p5P/P4PbK/2BQ4 w - - 0 1;d5d6
r2q1r2/1b2bpkp/p3p1p1/2ppP1P1/7R/1PN1BQR1/1PP2P1P/4K3 w - - 0 1;f3f6
r1r3k1/1ppn2bp/p1q1p1p1/3pP3/3PB1P1/PQ3NP1/3N4/2BK3R w - - 0 1;f3g5
1rr1b1k1/1pq1bp2/p2p1np1/4p3/P2BP3/2NB2Q1/1PP3PP/4RR1K w - - 0 1;f1f6
r1r3k1/1q3p1p/4p1pP/1bnpP1P1/pp1Q1P2/1P6/P1PR1N2/1K3B1R b - - 0 1;a4b3
r1b2rk1/pppnq3/4ppp1/6N1/3P3Q/2PB4/P1PK2PP/3R3R w - - 0 1;g5e6
3r1r1k/pp5p/4b1pb/6q1/3P4/4p1BP/PP2Q1PK/3RRB2 b - - 0 1;g5g3
r2r2k1/3bb1Pp/3pp1p1/p1q5/1p2PP2/P1N5/1PPQ4/1K1R1B1R w - - 0 1;c3d5
8/2R5/3p4/3P4/3k3P/2p3K1/1r4P1/8 w - - 0 1;g3f3
1r3rk1/2q4p/p1Np1bp1/1p1P4/n4pBP/2P2P2/PP1Q4/1K1R2R1 b - - 0 1;a4b2
2kr3r/pp4pp/4pp2/2pq4/P1Nn4/4Q3/KP2B1PP/2RR4 b - - 0 1;d5g2
5r2/1p4k1/pP1pP1pp/2rP2q1/4Qp2/3Bb3/P5PP/4RR1K w - - 0 1;f1f3
r2qr1k1/1b1pppbp/1p4p1/pP2P1B1/3N4/R7/1PP2PPP/3QR1K1 w - a6 0 1;d4f5
4k3/1R6/Pb3p2/1P1n4/5p2/8/4K3/8 w - - 0 1;e2d3
r4nk1/2pq1ppp/3p4/p3pNPQ/4P3/2PP1RP1/Pr3PK1/7R w - - 0 1;f5e3
r1q2rk1/1b2bppp/p1p1p3/4B3/PP6/3B3P/2P1QPP1/R2R2K1 w - - 0 1;d3h7
r2qrb1k/1p1b2p1/p2ppn1p/8/3NP3/1BN5/PPP3QP/1K3RR1 w - - 0 1;e4e5
r2q1k1r/pp2n1pp/2nb1p2/1B1p3Q/N2P4/2P1B3/PP4PP/R4RK1 w - - 0 1;f1f6
4r1k1/6p1/bp2r2p/3QNp2/P2BnP2/4P2P/5qPK/3RR3 b - - 0 1;g8h7
3rr1k1/pb3ppp/1p1p1bq1/3P4/1P2P3/P2Q1RBP/3N2P1/5RK1 w - - 0 1;f3f6
r2r1b1k/1pq3p1/1np1pp1p/p7/Pn1PP1N1/1P5R/1B1NQPPP/5RK1 w - - 0 1;d4d5
5k2/8/3pPp2/p1p3p1/Pp2PP2/1P3n2/7R/7K w - - 0 1;h2h8
3R4/pp2r1pk/q1p3bp/2P2r2/PP6/2Q3P1/6BP/5RK1 w - - 0 1;f1f5
r3k3/1p4p1/1Bb1Bp1p/P1p1bP1P/2Pp2P1/3P4/5K2/4R3 w - - 0 1;g4g5
1r1rb1k1/5ppp/4p3/1p1p3P/1q2P2Q/pN3P2/PPP4P/1K1R2R1 w - - 0 1;g1g7
1r1q1rk1/4bp1p/n3p3/pbNpP1PB/5P2/1P2B1K1/1P1Q4/2RR4 w - - 0 1;c5e4
r1bq1rk1/pp2bppp/1n2p3/3pP3/8/2RBBN2/PP2QPPP/2R3K1 w - - 0 1;d3h7
r6k/N1Rb2bp/p2p1nr1/3Pp2q/1P2Pp1P/5N2/P3QBP1/4R1K1 b - - 0 1;d7h3
r1b2rk1/1pq1nppp/pbn1p3/8/3N4/3BBN2/PPP1QPPP/3R1RK1 w - - 0 1;d3h7
3r1rk1/1b2qp1p/1p3np1/1N1p4/6n1/2NBP1K1/PBQ2PP1/3RR3 b - - 0 1;d5d4
8/1B6/3r2p1/5p1p/5P1P/4k1P1/8/6K1 b - - 0 1;g6g5
r3r2k/ppq3np/2p3p1/NPPp1bb1/P2Pnp2/3B1P2/2Q3PP/1RN1BRK1 b - - 0 1;e4g3
7k/5rp1/3q1p1p/2bNpQ1P/4P1P1/8/1R3PK1/8 w - - 0 1;g4g5
4r3/4r3/1ppqpnk1/p3Rp1p/P2P1R1Q/2PB2P1/1P3P2/6K1 w - - 0 1;d3f5
r3nrk1/1pqbbppp/p2pp3/2n1P3/5P2/2NBBNQ1/PPP3PP/R4RK1 w - - 0 1;d3h7
rnbq3r/ppp2kpp/4pp2/3n4/2BP4/BQ3N2/P4PPP/4RRK1 w - - 0 1;f3g5
8/2N5/1P2p3/5bPk/1q3b2/3Bp2P/2P5/6QK b - - 0 1;h5h4
1k1r1b1r/1p6/p4pp1/P1p1p3/2NpP1p1/1PPP2Pq/1B3P1P/2RQR1K1 b - - 0 1;f6f5
5r2/3rkp2/2R2p2/p2Bb2Q/1p2P2P/4q1P1/Pp6/1K1R4 b - - 0 1;b4b3
1kr4r/1p1bppb1/q1np1n2/p1p2PBp/2P5/P1NP2PP/1P1QN1B1/1R3R1K w - - 0 1;b2b4
r1b1k1r1/1p2np1p/p1n1pQp1/3p4/3NPP2/P2RB3/2PK2PP/q4B1R w q - 0 1;f1e2
4r1k1/1p4p1/p1qBp1Qp/b1pnP3/8/5NP1/1P3PKP/3R4 w - - 0 1;d1d5
2r1k2r/pp1bb1pp/6n1/3Q1p2/1B1N4/P7/1q4PP/4RRK1 w k - 0 1;b4e7
3b2k1/4qp2/2P4Q/3B3p/1P6/1K6/8/8 w - - 0 1;d5c4
1r2brk1/6p1/1q2p1Pp/pN1pPPb1/np1N4/5Q2/1PP1B3/1K1R3R w - - 0 1;f5f6
2rq1Nk1/pb3pp1/4p3/1p6/3b1Pn1/P1N5/1PQ3PP/R1B2R1K b - - 0 1;f7f5
r1b2rk1/1p4p1/p1n1p3/3p1pB1/NqP3n1/b2BP3/1PQN1P1P/1K4RR w - - 0 1;g1g4
q2rn1k1/1b3p1p/1p4p1/2n1B1P1/r1PN3P/P4P2/4Q1B1/3RR1K1 w - - 0 1;e5f6
r1b3r1/5p1k/p1n2P1p/P1qpp1P1/1p1p4/3P2Q1/BPPB2P1/R4RK1 w - - 0 1;g1f2
r2q1rk1/2p2ppp/pb1p1n2/n3p3/P2PP3/2P2NN1/R4PPP/2BQ1RK1 w - - 0 1;c1g5
1r2rbk1/1p1n1p2/p3b1p1/q2NpNPp/4P2Q/1P5R/6BP/5R1K w - h6 0 1;f5g3
2r4k/1prnqp1p/p2p2p1/P2Pn1P1/1PPQ4/4B1R1/4B2P/2R4K w - - 0 1;e3f4
5b2/1b2qp1k/2pp1npp/1p6/1P2PP2/r1PQ2NP/2B3P1/3RB1K1 w - - 0 1;e4e5
r1qr1bk1/2p2pp1/ppn1p2p/8/1PPPN1nP/P4NP1/2Q2PK1/2BRR3 w - - 0 1;e4g5
r1b2r1k/4qp1p/p2ppb1Q/4nP2/1p1NP3/2N5/PPP4P/2KR1BR1 w - - 0 1;d4c6
6k1/1p1bqnp1/1n3r2/p2Ppp2/Pb6/1P2Q1P1/NBr1N1BP/1R3R1K b - - 0 1;f5f4
8/2k5/2PrR1p1/7p/5p1P/5P1K/6P1/8 w - - 0 1;e6d6
8/4bBpp/3p4/P6P/2PN2p1/3k1b2/P7/6K1 w - - 0 1;h5h6
4K1k1/8/1p5p/1Pp3b1/8/1P3P2/P1B2P2/8 w - - 0 1;f3f4
r1b1r1k1/pp3ppp/4p1n1/q1ppP1B1/6QP/P1PB3R/2P1KPP1/R7 w - - 0 1;h4h5
8/k3qrpR/1p1p4/p2QpPp1/P1P1P1K1/1P6/8/8 w - - 0 1;b3b4
r2qk2r/1b1nbp1p/p1n1p1p1/1pp1P3/6Q1/2NPB1PN/PPP3BP/R4RK1 w kq - 0 1;f1f7
r2qk2r/2p1bpp1/p5B1/1p1pP3/3P2p1/5PnP/PP3R2/RNBQ2K1 b kq - 0 1;h8h3
2b1rbk1/6r1/2p1q3/p2p1p2/Pn1P2np/2NBP3/1P1QNPPB/R2R2K1 b - - 0 1;g4h2
k6r/ppqb4/2n5/4p2r/P2p1P1P/B1pQ2P1/2P3B1/RR4K1 w - - 0 1;a4a5
1r1q2k1/p4p1p/2Pp2p1/2p1P3/1r1n4/1P4P1/3R1PBP/3QR1K1 w - - 0 1;e5e6
2r2rk1/1q3p1p/2b1p1p1/1p1pP3/nP1N1PP1/p1P2Q2/P1R4P/4RBK1 w - - 0 1;f4f5
r1b3k1/1p2bppp/p3p3/n7/P3B3/1q2PN2/1B2QPPP/3R2K1 w - - 0 1;b2g7
1q4rk/R1nbp3/1n1p3p/QP1P4/3pPp2/2N2P1P/1P1N3K/5B2 w - - 0 1;d2b3
4rrk1/1bq1pp2/p2p1n1Q/1pn2p1p/4P3/P1N2P2/BPP3PP/2KRR3 w - - 0 1;g2g4
EOF

# expect script to get a bestmove at a given depth (max 24h before timeout)
cat << EOF > bestmove.exp
 log_user 0
 set timeout 86400
 spawn ./stockfish
 lassign \$argv pos gocommand
 send "uci\\n"
 expect "uciok"
 send "ucinewgame\\n"
 send "position fen \$pos \\n"
 send "\$gocommand\\n"
 expect "bestmove "
 expect -re "(\\[a-h\\]\\[1-8\\]\\[a-h\\]\\[1-8\\])"
 puts \$expect_out(buffer)
 send "quit\\n"
 expect eof
EOF

npos=`wc arasan_data.txt | awk '{print $1}'`
correct=0

for i in `seq 1 $npos`
do
   line=`sed "${i}!d;q" arasan_data.txt`
   fen=`echo "$line" | cut -d';' -f1`
   bestmove=`echo "$line" | cut -d';' -f2`
   move=`expect bestmove.exp "$fen" "$gocommand"`
   if [ "$bestmove" == "$move" ]; then
      correct=$((correct+1))
   fi
done

echo "found $correct moves from $npos positions"

rm arasan_data.txt bestmove.exp
