module Board (
    getBoardAfterMove,
    emptyBoard,
    getOwner,
) where

import qualified Data.Vector.Unboxed as Vector
import Data.Word
import Data.Int

import Types
import Offset
import Player
import Utils

boardBound = boardSize - 1

instance Show Board where 
    show board =
        let abbreviate owner = if owner == none then '.' else head $ show owner
            cellChar x y = abbreviate $ getOwner board (Coords x y)
            showRow x = [cellChar x y | y <- [0..boardBound]] ++ "\n"
         in concat [showRow x | x <- [0..boardBound]]

emptyBoard = Board $ Vector.replicate (fromIntegral boardSize) 0

getOwner (Board board) (Coords x y) = 
    let row = board `Vector.unsafeIndex` fromIntegral x
        flag = (row `div` (5 ^ y)) `mod` 5 
     in fromOwnershipFlag flag

getBoardAfterMove :: Board -> Player -> Coords -> Placement -> Board
getBoardAfterMove (Board board) player (Coords x y) (Placement _ _ offsets _ _) =
    let coords = (fromIntegral x,y):[(fromIntegral $ x+i, y+j) | (Offsets i j) <- offsets]
        player' = fromIntegral (fromEnum player)
        accumulate row y = row + ((5 ^ y) * player')
        board' = Vector.unsafeAccum accumulate board coords
     in Board board'
            
