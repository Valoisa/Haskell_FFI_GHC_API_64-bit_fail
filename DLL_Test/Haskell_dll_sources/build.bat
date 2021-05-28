cd %1
echo Building Compilation.hs...
ghc -c -no-hs-main -O Compilation.hs -fforce-recomp
echo Building HSCodeGen.cpp...
ghc -optcxx-fpermissive -O -c HSCodeGen.cpp
echo Linking library...
ghc -package ghc -package ghc-paths -package split -package cgi -package safe-exceptions -no-hs-main -shared -g2 -o %2/HSCodeGen.lib Compilation.o HSCodeGen.o