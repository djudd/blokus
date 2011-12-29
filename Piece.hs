module Piece (
    allPlacements,
    hasPiece,
    legalCorners -- for testing only
) where

import Data.Int
import Data.List
import Test.QuickCheck

import Types
import Utils
import Offset

getOffsets OnePiece = []
getOffsets TwoPiece = [(0,1)]
getOffsets ThreePiece = [(0,1),(0,2)]
getOffsets CrookedThree = [(0,1),(1,0)]
getOffsets SquarePiece = [(0,1),(1,0),(1,1)]
getOffsets ShortI = [(0,1),(0,2),(0,3)]
getOffsets ShortT = [(0,1),(1,1),(-1,1)]
getOffsets ShortL = [(0,1),(1,0),(0,2)]
getOffsets ShortZ = [(0,1),(1,1),(1,2)]
getOffsets LongI = [(0,1),(0,2),(0,3),(0,4)]
getOffsets LongT = [(0,1),(0,2),(1,2),(-1,2)]
getOffsets LongL = [(0,1),(1,0),(0,2),(0,3)]
getOffsets LongZ = [(0,1),(1,1),(0,-1),(-1,-1)]
getOffsets PPiece = [(0,1),(1,0),(1,1),(0,2)]
getOffsets FPiece = [(0,1),(1,0),(0,-1),(-1,-1)]
getOffsets XPiece = [(0,1),(1,0),(0,-1),(-1,0)]
getOffsets VPiece = [(0,1),(0,2),(1,0),(2,0)]
getOffsets UPiece = [(0,1),(0,-1),(1,1),(1,-1)]
getOffsets YPiece = [(0,1),(1,0),(-1,0),(-2,0)]
getOffsets NPiece = [(0,1),(1,1),(2,1),(3,1)]
getOffsets WPiece = [(0,1),(1,1),(-1,0),(-1,-1)]

touchesOn a b = (abs (a-b)) <= 1
touches x y (i,j) = ((touchesOn x i) && (y == j)) || ((touchesOn y j) && (x == i))
legal offsets (PieceCorner x y _) = not $ any (touches x y) offsets

corners (x, y) = (PieceCorner (x+1) (y+1) UpperRight):(PieceCorner (x+1) (y-1) LowerRight):(PieceCorner (x-1) (y+1) UpperLeft):(PieceCorner (x-1) (y-1) LowerLeft):[]
legalCorners offsets =
    let pieceCorners = concat $ map corners offsets
     in nub $ filter (legal offsets) pieceCorners

getPlacements piece =
    let offsets = (0,0):(getOffsets piece)
        all = concat $ map translations $ concat $ map rotations $ reflections offsets
    in nub $ map sort all

buildPlacement piece offsets = Placement piece offsets (legalCorners offsets) (toBitmap offsets)

allPieces = [minBound..maxBound] :: [Piece]

allPlacements = [buildPlacement piece offsets | piece <- allPieces, offsets <- (getPlacements piece)]

hasPiece piece (Placement p _ _ _) = piece == p
