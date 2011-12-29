module Board (
    getBoardAfterMove,
    emptyBoard,
    getOwner,
    showBoard
) where

import Data.Array.Unboxed
import Data.Word
import Data.Int

import Types
import Utils

boardBound = boardSize - 1

showBoard board =
    let showCell x y = show $ getOwner board x y
        showRow x = (concat $ [showCell x y | y <- [0..boardBound]]) ++ "\n"
    in (concat $ [showRow x | x <- [0..boardBound]]) ++ "\n"

emptyBoard :: Board
emptyBoard = array (0,boardBound) [(i,0) | i <- [0..boardBound]]

getOwner :: Board -> Coord -> Coord -> Player
getOwner board x y = fromIntegral $ ((board ! x) `div` (5 ^ y)) `mod` 5

-- TODO this is inefficient as implemented
setOwner player board (x,y) =
    let updated = (board ! x) + ((5 ^ y) * (fromIntegral player))
    in board // [(x, updated)]

assign player x y offsets board =
    let coords = (x,y):(translate (x,y) offsets)
    in foldl (setOwner player) board coords

getBoardAfterMove :: Board -> Move -> Board
getBoardAfterMove board (Move player x y (Placement _ offsets _ _)) =
    assign player x y offsets board
