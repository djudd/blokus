module Board (
    getBoardAfterMove,
    emptyBoard,
    unowned,
    onBoard,
    legal,
    showBoard
) where

import Data.Array.Unboxed
import Data.Word
import Data.Int

import Types

boardBound = boardSize - 1

showBoard board =
    let showCell x y = show $ getOwner board x y
        showRow x = (concat $ [showCell x y | y <- [0..boardBound]]) ++ "\n"
    in (concat $ [showRow x | x <- [0..boardBound]]) ++ "\n"

emptyBoard :: Board
emptyBoard = array (0,boardBound) [(i,0) | i <- [0..boardBound]]

--getOwner :: Board -> Coord -> Coord -> Player
getOwner board x y = fromIntegral $ ((board ! x) `div` (5 ^ y)) `mod` 5

hasOwner player board x y = (getOwner board x y) == player
unowned board (x, y) = (getOwner board x y) == 0

touchesSide player board (x, y)
    | (x < boardBound) && (playerOwns (x+1) y) = True
    | (x > 0) && (playerOwns (x-1) y) = True
    | (y < boardBound) && (playerOwns x (y+1)) = True
    | (y > 0) && (playerOwns x (y-1)) = True
    | otherwise = False
    where playerOwns i j = hasOwner player board i j

legal player board coords = (unowned board coords) && (not $ touchesSide player board coords)

onBoard (x, y) = (x >= 0) && (x < boardSize) && (y >= 0) && (y < boardSize)

-- TODO this is inefficient as implemented
setOwner player board (x,y) =
    let updated = (board ! x) + ((5 ^ y) * (fromIntegral player))
    in board // [(x, updated)]

assign player x y offsets board =
    let offsets' = (x,y):offsets
    in foldl (setOwner player) board offsets'

getBoardAfterMove :: Board -> Move -> Board
getBoardAfterMove board (Move player x y (Placement _ offsets _ _)) =
    assign player x y offsets board
