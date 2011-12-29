module Placement (
    initialPlacements,
    getPlacementsAfterMove,
    -- below here visible only for testing
    getTransformations,
    legalCorners
) where

import Data.Int
import Data.List
import Test.QuickCheck

import Types
import Offset
import Utils

instance Eq PieceCorner where
    (PieceCorner x1 y1 d1) == (PieceCorner x2 y2 d2) = (x1 == x2) && (y1 == y2) && (d1 == d2)

touchesOn a b = (abs (a-b)) <= 1
touches x y (i,j) = ((touchesOn x i) && (y == j)) || ((touchesOn y j) && (x == i))
legal offsets (PieceCorner x y _) = not $ any (touches x y) offsets

corners (x, y) = (PieceCorner (x+1) (y+1) UpperRight):(PieceCorner (x+1) (y-1) LowerRight):(PieceCorner (x-1) (y+1) UpperLeft):(PieceCorner (x-1) (y-1) LowerLeft):[]
legalCorners offsets =
    let pieceCorners = concatMap corners offsets
     in nub $ filter (legal offsets) pieceCorners

getTransformations piece =
    let offsets = (0,0):(getOffsets piece)
        all = concatMap translations $ concatMap rotations $ reflections offsets
    in nub $ map sort all

buildPlacement piece offsets = Placement piece offsets (legalCorners offsets) (toBitmap offsets)

allPieces = [minBound..maxBound] :: [Piece]
allPlacements = [buildPlacement piece offsets | piece <- allPieces, offsets <- (getTransformations piece)]
initialPlacements = take numPlayers $ repeat allPlacements

hasPiece piece (Placement p _ _ _) = piece == p

getPlacementsAfterMove :: Move -> [[Placement]] -> [[Placement]]
getPlacementsAfterMove (Move player x y (Placement piece _ _ _)) placements =
    let moverPlacements = placements !! (fromIntegral player)
        moverPlacements' = filter (not . (hasPiece piece)) moverPlacements
     in replaceAt (fromIntegral player) placements moverPlacements'
