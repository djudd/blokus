module Territory (
    initialCorners,
    getCornersAfterMove,
    -- below here visible only for testing
    legal,
    getCornersForMovingPlayer
) where

import Data.List

import Types
import Board
import Player
import Offset
import Utils

boardBound = boardSize - 1

instance Eq TerritoryCorner where
    (TerritoryCorner (Coords x1 y1) d1 _) == (TerritoryCorner (Coords x2 y2) d2 _) = (x1 == x2) && (y1 == y2) && (d1 == d2)

getCoords (TerritoryCorner coords _ _) = coords

hasOwner player board coords = (getOwner board coords) == player
unowned board coords = (getOwner board coords) == None

touchesSide player board (Coords x y)
    | (x < boardBound) && (playerOwns (x+1) y) = True
    | (x > 0) && (playerOwns (x-1) y) = True
    | (y < boardBound) && (playerOwns x (y+1)) = True
    | (y > 0) && (playerOwns x (y-1)) = True
    | otherwise = False
    where playerOwns i j = hasOwner player board (Coords i j)

legal player board coords = (unowned board coords) && (not $ touchesSide player board coords)

onBoard (Coords x y) = (x >= 0) && (x < boardSize) && (y >= 0) && (y < boardSize)

getNotTakenCorners board corners =
    map (filter $ (unowned board) . getCoords) corners

validOffset player board (Coords x y) (Offsets i j) =
    let coords = Coords (x+i) (y+j)
    in (onBoard coords) && (legal player board coords)

calculateValidityBitmap player board coords cornerType =
    toBitmap $ filter (validOffset player board coords) $ getReachableOffsets cornerType

translateCorner player board (Coords x y) (PieceCorner (Offsets i j) cornerType) =
    let coords = Coords (x+i) (y+j)
        bitmap = calculateValidityBitmap player board coords cornerType
    in TerritoryCorner coords cornerType bitmap

getCornersForMovingPlayer player board prevCorners coords addedCorners =
    let translated = filter (onBoard . getCoords) $ map (translateCorner player board coords) addedCorners
        legalHere = (legal player board) . getCoords
     in nub $ filter legalHere $ prevCorners ++ translated

getCornersAfterMove :: Board -> Move -> [[TerritoryCorner]] -> [[TerritoryCorner]]
getCornersAfterMove boardAfterMove (Move player coords (Placement _ _ addedCorners _)) corners =
    let index = getIndex player
        moverCorners = corners !! index
        moverCorners' = getCornersForMovingPlayer player boardAfterMove moverCorners coords addedCorners
        remainingCorners = getNotTakenCorners boardAfterMove corners
     in replaceAt index remainingCorners moverCorners'

initialCorners =
    let mkCorner player x y cornerType = [TerritoryCorner (Coords x y) cornerType $ calculateValidityBitmap player emptyBoard (Coords x y) cornerType]
     in [(mkCorner Red 0 0 UpperRight),
         (mkCorner Green boardBound 0 UpperLeft),
         (mkCorner Yellow boardBound boardBound LowerLeft),
         (mkCorner Blue 0 boardBound LowerRight)]
