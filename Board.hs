module Board (
    assign,
    getOwner,
    hasOwner,
    boardSize,
    emptyBoard,
    showBoard
) where

import Data.Array.Unboxed
import Data.Word
import Data.Int

import Types
import Piece

boardBound = boardSize - 1

showRow board row = (concat $ [show $ getOwner board row y | y <- [0..boardBound]]) ++ "\n"
showBoard board = (concat $ [showRow board x | x <- [0..boardBound]]) ++ "\n"

emptyBoard :: Board
emptyBoard = array (0,boardBound) [(i,0) | i <- [0..boardBound]]

getOwner :: Board -> Coord -> Coord -> Player
getOwner board x y = fromIntegral $ ((board ! x) `div` (5 ^ y)) `mod` 5

hasOwner player board x y = (getOwner board x y) == player

-- TODO this is inefficient as implemented
setOwner player board (x,y) =
    let updated = (board ! x) + ((5 ^ y) * (fromIntegral player))
    in board // [(x, updated)]

assign player x y offsets board =
    let offsets' = (x,y):offsets
    in foldl (setOwner player) board offsets'



