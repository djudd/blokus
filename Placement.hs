module Placement (
    initialPlacements,
    getPlacementsAfterMove,
    -- below here visible only for testing
    getTransformations,
    legalCorners
) where

import Data.Int
import Data.List

import Types
import Player
import Offset
import Utils

instance Eq PieceCorner where
    (PieceCorner (Offsets x1 y1) d1) == (PieceCorner (Offsets x2 y2) d2) = (x1 == x2) && (y1 == y2) && (d1 == d2)

touchesOn a b = (abs (a-b)) <= 1
touches x y (Offsets i j) = ((touchesOn x i) && (y == j)) || ((touchesOn y j) && (x == i))
legal offsets (PieceCorner (Offsets x y) _) = not $ any (touches x y) offsets

corners (Offsets x y) =
    [(PieceCorner (Offsets (x+1) (y+1)) UpperRight),
     (PieceCorner (Offsets (x+1) (y-1)) LowerRight),
     (PieceCorner (Offsets (x-1) (y+1)) UpperLeft),
     (PieceCorner (Offsets (x-1) (y-1)) LowerLeft)]

legalCorners offsets =
    let pieceCorners = concatMap corners $ (Offsets 0 0):offsets
     in nub $ filter (legal offsets) pieceCorners

getTransformations piece =
    let offsets = fromOffsets $ (Offsets 0 0):(getOffsets piece)
        transformed = concatMap translations $ concatMap rotations $ reflections offsets
        removeOrigin offsets = filter (\(x,y) -> not $ (x == 0) && (y == 0)) offsets
     in map toOffsets $ map removeOrigin $ nub $ map sort transformed

buildPlacement piece offsets = Placement piece offsets (legalCorners offsets) (toBitmap offsets)

allPieces = [minBound..maxBound] :: [Piece]
allPlacements = [buildPlacement piece offsets | piece <- allPieces, offsets <- (getTransformations piece)]
initialPlacements = take numPlayers $ repeat allPlacements

hasPiece piece (Placement p _ _ _) = piece == p

getPlacementsAfterMove :: Player -> Move -> [[Placement]] -> [[Placement]]
getPlacementsAfterMove player (Move _ (Placement piece _ _ _)) placements =
    let index = getIndex player
        moverPlacements = placements !! index
        moverPlacements' = filter (not . (hasPiece piece)) moverPlacements
     in replaceAt index placements moverPlacements'
