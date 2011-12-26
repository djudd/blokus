module Board (
    Board,
    Player,
    assign,
    getOwner,
    hasOwner,
    boardSize
) where

import Data.Array.Unboxed
import Data.Word
import Data.Int

import Piece
import Placement

type Player = Int8
type Board = Array Int8 Word64

boardSize = 20

empty :: Board
empty = array (0,boardSize-1) [(i,0) | i <- [0..(boardSize-1)]]

getOwner board x y = ((board ! x) / (5 ^ y)) `mod` 5

hasOwner player board x y = (getOwner board x y) == player

-- TODO this is inefficient as implemented
setOwner player board (x,y) = 
    let updated = (board ! x) + ((5 ^ y) * (fromIntegral player))
    in board // [(x, updated)]

assign player x y offsets board = 
    let offsets' = (x,y):offsets
    in foldl (setOwner player) board offsets'

main = print $ assign 2 0 0 [] empty



