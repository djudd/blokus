module Piece (
    getPiecesAfterMove,
    iteratePieces,
    initialPieces,
) where

import Data.Bits

import Types
import Player
import Utils

setPieceBit bitmap piece = setBit bitmap (fromEnum piece)
clearPieceBit bitmap piece = clearBit bitmap (fromEnum piece)

initialBitmap = foldl setPieceBit 0 allPieces :: PieceBitmap
initialPieces = replicate numPlayers initialBitmap

head' bitmap = toEnum (firstBitSet bitmap - 1)
tail' bitmap = bitmap .&. (bitmap - 1)

iteratePieces :: PieceBitmap -> [Piece]
iteratePieces 0 = []
iteratePieces bitmap = head' bitmap : iteratePieces (tail' bitmap)

getPiecesAfterMove player (Placement piece _ _ _ _) pieces =
    let index = getIndex player
        bitmap = pieces !! index
        bitmap' = clearPieceBit bitmap piece
     in replaceAt index pieces bitmap'
