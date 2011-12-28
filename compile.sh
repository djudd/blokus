rm -f blokus *.o *.hi
#ghc -O2 -prof -auto-all -package containers-0.3.0.0 Trie.hs EditStep.hs main.hs -o editstep
ghc -O2 Piece.hs Placement.hs Board.hs Corner.hs GameState.hs Test.hs -o blokus 
