#!/usr/local/bin/fish

echo COMPILING
ghc vandelay.hs

echo CLEANING
find . | egrep -e '\.hi$' -e '\.o$' | xargs rm

echo MOVING BINARY
mv vandelay bin/OS\ X\ Mavericks
