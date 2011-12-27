module Placement (
    PieceSquare,
    PieceCorner (PieceCorner),
    Placement (Placement),
    PlacementBitmap,
    Direction (UpperRight,LowerRight,UpperLeft,LowerLeft)
) where

import Data.Bits
import Data.Int
import Data.Word
import Data.List

import Piece

type PieceSquare = (Offset,Offset) -- square occupied by a placement of a piece, labeled by offset from the origin of the placement
type PlacementBitmap = Word64

data Direction = UpperRight | LowerRight | UpperLeft | LowerLeft deriving (Show,Eq)
data PieceCorner = PieceCorner Offset Offset Direction deriving (Show)
data Placement = Placement PieceLabel [PieceSquare] [PieceCorner] PlacementBitmap deriving (Show)

instance Eq PieceCorner where
    (PieceCorner x1 y1 d1) == (PieceCorner x2 y2 d2) = (x1 == x2) && (y1 == y2) && (d1 == d2)

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

toBitmap :: [PieceSquare] -> PlacementBitmap
toBitmap offsets = foldl setBit' 0 offsets 
    where setBit' bitmap (x,y) = setBit bitmap $ fromIntegral $ offsetsIdx x y

touchesOn a b = (abs (a-b)) <= 1
touches x y (i,j) = ((touchesOn x i) && (y == j)) || ((touchesOn y j) && (x == i))
legal offsets (PieceCorner x y direction) = not $ any (touches x y) offsets

corners (x, y) = (PieceCorner (x+1) (y+1) UpperRight):(PieceCorner (x+1) (y-1) LowerRight):(PieceCorner (x-1) (y+1) UpperLeft):(PieceCorner (x-1) (y-1) LowerLeft):[]
legalCorners offsets =
    let pieceCorners = concat $ map corners offsets
     in nub $ filter (legal offsets) pieceCorners

buildPlacement label offsets = Placement label offsets (legalCorners offsets) (toBitmap offsets)
buildPlacements piece = map (buildPlacement (getLabel piece)) (getPlacements piece)

allPlacements = concat $ map buildPlacements allPieces

