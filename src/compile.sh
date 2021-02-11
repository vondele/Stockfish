for ARCH in x86-64-avx2 x86-64-bmi2 x86-64-modern x86-64-ssse3
do
    make clean
    make -j profile-build ARCH=$ARCH COMP=mingw
    mv ./stockfish.exe ./fat-fritz2-$ARCH.exe
done