#ghc -O2 -prof -auto-all -package containers-0.3.0.0 Trie.hs EditStep.hs main.hs -o editstep
rm -f blokus
ghc -O2 Types.hs Offset.hs Placement.hs Board.hs Territory.hs GameState.hs Test.hs -o blokus
rm -f *.o *.hi
