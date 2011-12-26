import Data.Bits
import Data.Int
import Data.Word

import Piece
import Data.List

type PieceSquare = (Offset,Offset) -- square occupied by a placement of a piece, labeled by offset from the origin of the placement
type PieceCorner = (Offset,Offset) -- square touching only the corner of a PieceSquare, labeled by offset from the origin of the placement
type PlacementBitmap = Word64

data Placement = Placement PieceLabel [PieceSquare] [PieceCorner] PlacementBitmap deriving (Show)

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
legal offsets (x, y) = not $ any (touches x y) offsets

diagonals (x, y) = (x+1,y+1):(x+1,y-1):(x-1,y+1):(x-1,y-1):[]

legalCorners :: [PieceSquare] -> [PieceCorner]
legalCorners offsets =
    let corners = concat $ map diagonals offsets
     in nub $ filter (legal offsets) corners

buildPlacement label offsets = Placement label offsets (legalCorners offsets) (toBitmap offsets)
buildPlacements piece = map (buildPlacement (getLabel piece)) (getPlacements piece)

allPlacements = concat $ map buildPlacements allPieces

main = print $ filter (\(Placement label _ _ _) -> label == '2') allPlacements
