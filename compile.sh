rm -f blokus
ghc -O2 Types.hs Offset.hs Placement.hs Board.hs Territory.hs GameState.hs Test.hs -o blokus
#ghc -O2 -prof -auto-all Types.hs Offset.hs Placement.hs Board.hs Territory.hs GameState.hs main.hs -o blokus
rm -f *.o *.hi
