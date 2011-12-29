module Territory (
    initialCorners,
    getCornersAfterMove,
    -- below here visible only for testing
    legal
) where

import Data.List

import Types
import Board
import Offset
import Utils

boardBound = boardSize - 1

instance Eq TerritoryCorner where
    (TerritoryCorner x1 y1 d1 _) == (TerritoryCorner x2 y2 d2 _) = (x1 == x2) && (y1 == y2) && (d1 == d2)

getCoords (TerritoryCorner x y _ _) = (x,y)

hasOwner player board x y = (getOwner board x y) == player
unowned board (x, y) = (getOwner board x y) == 0

touchesSide player board (x, y)
    | (x < boardBound) && (playerOwns (x+1) y) = True
    | (x > 0) && (playerOwns (x-1) y) = True
    | (y < boardBound) && (playerOwns x (y+1)) = True
    | (y > 0) && (playerOwns x (y-1)) = True
    | otherwise = False
    where playerOwns i j = hasOwner player board i j

legal player board coords = (unowned board coords) && (not $ touchesSide player board coords)

onBoard (x, y) = (x >= 0) && (x < boardSize) && (y >= 0) && (y < boardSize)

getNotTakenCorners board corners =
    map (filter $ (unowned board) . getCoords) corners

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

getCornersForMovingPlayer player board prevCorners x y addedCorners =
    let filter' = filter $ (legal player board) . getCoords
        old = filter' $ prevCorners
        new = filter' $ map (translateCorner player board x y) addedCorners
     in nub $ old ++ new

getCornersAfterMove :: Board -> Move -> [[TerritoryCorner]] -> [[TerritoryCorner]]
getCornersAfterMove board (Move player x y (Placement _ _ addedCorners _)) corners =
    let moverCorners = corners !! (fromIntegral player)
        moverCorners' = getCornersForMovingPlayer player board moverCorners x y addedCorners
        remainingCorners = getNotTakenCorners board corners
     in replaceAt (fromIntegral player) remainingCorners moverCorners'

initialCorners =
    let mkCorner player x y cornerType = [TerritoryCorner x y cornerType $ calculateValidityBitmap player emptyBoard x y cornerType]
     in [(mkCorner 0 0 0 UpperRight),
         (mkCorner 1 boardBound 0 UpperLeft),
         (mkCorner 2 0 boardBound LowerRight),
         (mkCorner 3 boardBound boardBound LowerLeft)]
