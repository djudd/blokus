import Data.Int
import Data.List

import Piece
import Placement
import Board

type Coord = Int8

data TerritoryCorner = TerritoryCorner Coord Coord Direction PlacementBitmap deriving (Show)

instance Eq TerritoryCorner where
    (TerritoryCorner x1 y1 d1 _) == (TerritoryCorner x2 y2 d2 _) = (x1 == x2) && (y1 == y2) && (d1 == d2)

unowned board (TerritoryCorner x y _ _) = (getOwner board x y) == 0

touchesSide player board (TerritoryCorner x y _ _)
    | ((x+1) < boardSize) && hasOwner player board (x+1) y = True
    | ((x-1) >= 0) && hasOwner player board (x-1) y = True
    | ((y+1) < boardSize) && hasOwner player board x (y+1) = True
    | ((y-1) >= 0) && hasOwner player board x (y-1) = True
    | otherwise = False

legal player board corner = (unowned board corner) && (not $ touchesSide player board corner)

getCornersForNonMovingPlayer board corners = 
    filter (unowned board) corners

calculateValidityBitmap player board x y direction = 0

translate player board i j (PieceCorner x y direction) =
    let x' = i+x
        y' = j+y
        bitmap = calculateValidityBitmap player board x' y' direction
    in TerritoryCorner x' y' direction bitmap

getCornersForMovingPlayer player board prevCorners x y playedCorners = 
    let old = filter (legal player board) prevCorners
        new = filter (legal player board) $ map (translate player board x y) playedCorners
     in nub $ old ++ new

main = print ""
