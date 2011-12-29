module Corner (
    getCornersForMovingPlayer,
    getNotTakenCorners,
    initialCorners
) where

import Data.Int
import Data.List

import Types
import Piece
import Board
import Offset

bound = boardSize - 1
mkCorner player x y cornerType = [TerritoryCorner x y cornerType $ calculateValidityBitmap player emptyBoard x y cornerType]
initialCorners = (mkCorner 0 0 0 UpperRight):(mkCorner 1 bound 0 UpperLeft):(mkCorner 2 0 bound LowerRight):(mkCorner 3 bound bound LowerLeft):[]

unowned board (x, y) = (getOwner board x y) == 0

touchesSide player board (x, y)
    | ((x+1) < boardSize) && hasOwner player board (x+1) y = True
    | ((x-1) >= 0) && hasOwner player board (x-1) y = True
    | ((y+1) < boardSize) && hasOwner player board x (y+1) = True
    | ((y-1) >= 0) && hasOwner player board x (y-1) = True
    | otherwise = False

legal player board coords = (unowned board coords) && (not $ touchesSide player board coords)

onBoard (x, y) = (x >= 0) && (x < boardSize) && (y >= 0) && (y < boardSize)

getCoords (TerritoryCorner x y _ _) = (x,y)

getNotTakenCorners :: Board -> [[TerritoryCorner]] -> [[TerritoryCorner]]
getNotTakenCorners board playerCorners =
    map (filter $ (unowned board) . getCoords) playerCorners

validOffset player board x y (i,j) =
    let coords = (x+i,y+j)
     in (onBoard coords) && (legal player board coords)

calculateValidityBitmap player board x y cornerType =
    toBitmap $ filter (validOffset player board x y) $ getReachableOffsets cornerType

translateCorner player board i j (PieceCorner x y cornerType) =
    let x' = i+x
        y' = j+y
        bitmap = calculateValidityBitmap player board x' y' cornerType
    in TerritoryCorner x' y' cornerType bitmap

getCornersForMovingPlayer :: Player -> Board -> [TerritoryCorner] -> Coord -> Coord -> [PieceCorner] -> [TerritoryCorner]
getCornersForMovingPlayer player board prevCorners x y playedCorners =
    let filter' = filter $ (legal player board) . getCoords
        old = filter' $ prevCorners
        new = filter' $ map (translateCorner player board x y) playedCorners
     in nub $ old ++ new

