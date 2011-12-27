module Corner (
    Coord,
    TerritoryCorner (TerritoryCorner),
    getCornersForMovingPlayer,
    getNotTakenCorners,
    initialCorners
) where

import Data.Int
import Data.List

import Piece
import Placement
import Board

data TerritoryCorner = TerritoryCorner Coord Coord Direction PlacementBitmap deriving (Show)

instance Eq TerritoryCorner where
    (TerritoryCorner x1 y1 d1 _) == (TerritoryCorner x2 y2 d2 _) = (x1 == x2) && (y1 == y2) && (d1 == d2)

bound = boardSize - 1
mkCorner player x y direction = [TerritoryCorner x y direction $ calculateValidityBitmap player empty x y direction]
initialCorners = (mkCorner 0 0 0 UpperRight):(mkCorner 1 bound 0 UpperLeft):(mkCorner 2 0 bound LowerRight):(mkCorner 3 bound bound LowerLeft):[]

unowned board (TerritoryCorner x y _ _) = (getOwner board x y) == 0

touchesSide player board (TerritoryCorner x y _ _)
    | ((x+1) < boardSize) && hasOwner player board (x+1) y = True
    | ((x-1) >= 0) && hasOwner player board (x-1) y = True
    | ((y+1) < boardSize) && hasOwner player board x (y+1) = True
    | ((y-1) >= 0) && hasOwner player board x (y-1) = True
    | otherwise = False

legal player board corner = (unowned board corner) && (not $ touchesSide player board corner)

getNotTakenCorners :: Board -> [[TerritoryCorner]] -> [[TerritoryCorner]] 
getNotTakenCorners board playerCorners = 
    map (filter (unowned board)) playerCorners

calculateValidityBitmap player board x y direction = 0

translate player board i j (PieceCorner x y direction) =
    let x' = i+x
        y' = j+y
        bitmap = calculateValidityBitmap player board x' y' direction
    in TerritoryCorner x' y' direction bitmap

getCornersForMovingPlayer :: Player -> Board -> [TerritoryCorner] -> Coord -> Coord -> [PieceCorner] -> [TerritoryCorner]
getCornersForMovingPlayer player board prevCorners x y playedCorners = 
    let old = filter (legal player board) prevCorners
        new = filter (legal player board) $ map (translate player board x y) playedCorners
     in nub $ old ++ new

