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
import Offset
import Utils

boardBound = boardSize - 1

showBoard board =
    let showCell x y = show $ getOwner board (Coords x y)
        showRow x = (concat $ [showCell x y | y <- [0..boardBound]]) ++ "\n"
    in (concat $ [showRow x | x <- [0..boardBound]]) ++ "\n"

emptyBoard :: Board
emptyBoard = array (0,boardBound) [(i,0) | i <- [0..boardBound]]

getOwner :: Board -> Coords -> Player
getOwner board (Coords x y) = fromIntegral $ ((board ! x) `div` (5 ^ y)) `mod` 5

-- TODO this is inefficient as implemented
setOwner player board (Coords x y) =
    let updated = (board ! x) + ((5 ^ y) * (fromIntegral player))
    in board // [(x, updated)]

toCoords offsets = map (\(x, y) -> (Coords x y)) offsets

assign player (Coords x y) offsets board =
    let coords = (Coords x y):(toCoords $ translate (x,y) $ fromOffsets offsets)
    in foldl (setOwner player) board coords

getBoardAfterMove :: Board -> Move -> Board
getBoardAfterMove board (Move player coords (Placement _ offsets _ _)) =
    assign player coords offsets board
