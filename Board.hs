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
import Player
import Utils

boardBound = boardSize - 1

showBoard board =
    let abbreviate owner = if owner == none then '.' else head $ show owner
        cellChar x y = abbreviate $ getOwner board (Coords x y)
        showRow x = [cellChar x y | y <- [0..boardBound]] ++ "\n"
    in concat [showRow x | x <- [0..boardBound]]

emptyBoard :: Board
emptyBoard = array (0,boardBound) [(i,0) | i <- [0..boardBound]]

getOwner :: Board -> Coords -> Player
getOwner board (Coords x y) = fromOwnershipFlag $ ((board ! x) `div` (5 ^ y)) `mod` 5

-- TODO this is inefficient as implemented
setOwner player board (Coords x y) =
    let updated = (board ! x) + ((5 ^ y) * player)
    in board // [(x, updated)]

toCoords = map (uncurry Coords)

getBoardAfterMove :: Board -> Player -> Coords -> Placement -> Board
getBoardAfterMove board player (Coords x y) (Placement _ offsets _ _) =
   let setOwner' = setOwner (getOwnershipFlag player)
       coords = Coords x y:toCoords (translate (x,y) $ fromOffsets offsets)
   in foldl setOwner' board coords

