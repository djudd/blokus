module Placement (
    PieceSquare,
    PieceCorner (PieceCorner),
    Placement (Placement),
    PlacementBitmap,
    Direction (UpperRight,LowerRight,UpperLeft,LowerLeft),
    hasLabel,
    allPlacements,
    initialPlacements,
    reachableOffsetsUpperRight,
    reachableOffsetsLowerRight,
    reachableOffsetsUpperLeft,
    reachableOffsetsLowerLeft,
    toBitmap,
    legalCorners
) where

import Data.Bits hiding (rotate)
import Data.Int
import Data.Word
import Data.List
import Test.QuickCheck

import Piece

type PieceSquare = (Offset,Offset) -- square occupied by a placement of a piece, labeled by offset from the origin of the placement
type PlacementBitmap = Word64

data Direction = UpperRight | LowerRight | UpperLeft | LowerLeft deriving (Show,Eq)
data PieceCorner = PieceCorner Offset Offset Direction deriving (Show)
data Placement = Placement PieceLabel [PieceSquare] [PieceCorner] PlacementBitmap deriving (Show)

instance Eq PieceCorner where
    (PieceCorner x1 y1 d1) == (PieceCorner x2 y2 d2) = (x1 == x2) && (y1 == y2) && (d1 == d2)

hasLabel test (Placement label _ _ _) = label == test

offsetsIdx :: Offset -> Offset -> Int8
offsetsIdx x y = case x+4 of
    0 -> 0
    1 -> 1 + (y+1) -- -1 <= y <= 1
    2 -> 4 + (y+2) -- -2 <= y <= 2
    3 -> 9 + (y+3) -- ...
    4 -> 16 + (y+4)
    5 -> 25 + (y+3)
    6 -> 32 + (y+2)
    7 -> 37 + (y+1)
    8 -> 40

toBitmap :: [(Offset,Offset)] -> PlacementBitmap
toBitmap offsets = foldl setBit' 0 offsets
    where setBit' bitmap (x,y) = setBit bitmap $ fromIntegral $ offsetsIdx x y

touchesOn a b = (abs (a-b)) <= 1
touches x y (i,j) = ((touchesOn x i) && (y == j)) || ((touchesOn y j) && (x == i))
legal offsets (PieceCorner x y direction) = not $ any (touches x y) offsets

corners (x, y) = (PieceCorner (x+1) (y+1) UpperRight):(PieceCorner (x+1) (y-1) LowerRight):(PieceCorner (x-1) (y+1) UpperLeft):(PieceCorner (x-1) (y-1) LowerLeft):[]
legalCorners offsets =
    let pieceCorners = concat $ map corners offsets
     in nub $ filter (legal offsets) pieceCorners

buildPlacement piece offsets = Placement (getLabel piece) offsets (legalCorners offsets) (toBitmap offsets)

allPlacements = [buildPlacement piece offsets | piece <- allPieces, offsets <- (getPlacements piece)]

initialPlacements = take 4 $ repeat allPlacements

reachableOffsets :: [(Offset,Offset)]
reachableOffsets = [(1,-3),(0,-2),(1,-2),(2,-2),(1,-1),(2,-1),(3,-1),(-2,0),(1,0),(2,0),(3,0),(4,0),(-3,1),(-2,1),(-1,1),(0,1),(1,1),(2,1),(3,1),(-2,2),(-1,2),(0,2),(1,2),(2,2),(-1,3),(0,3),(1,3),(0,4)]

reachableOffsetsUpperRight = rotate (1,1) reachableOffsets
reachableOffsetsUpperLeft = rotate (-1,1) reachableOffsets
reachableOffsetsLowerRight = rotate (1,-1) reachableOffsets
reachableOffsetsLowerLeft = rotate (-1,-1) reachableOffsets

