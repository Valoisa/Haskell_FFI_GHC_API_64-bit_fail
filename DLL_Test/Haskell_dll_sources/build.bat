cd %1
echo Building Compilation.hs...
ghc -package ghc -c -no-hs-main -O Compilation.hs -fforce-recomp
echo Building HSCodeGen.cpp...
ghc -O -c HSCodeGen.cpp
echo Linking library...
ghc -package ghc -package ghc-paths -package split -package cgi -package safe-exceptions -no-hs-main -shared -g2 -o %2/HSCodeGen.dll Compilation.o HSCodeGen.o