#rm *.o *.hi
#ghc -O2 -prof -auto-all -package containers-0.3.0.0 Trie.hs EditStep.hs main.hs -o editstep
ghc -O2 Piece.hs Placement.hs -o blokus 
