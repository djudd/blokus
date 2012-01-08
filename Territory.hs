module Territory (
    initialCorners,
    getCornersAfterMove,
    getCoords,
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

instance Show TerritoryCorner where
    show (TerritoryCorner (Coords x y) _ _) = show x ++ "," ++ show y

instance Eq TerritoryCorner where
    (TerritoryCorner (Coords x1 y1) _ _) == (TerritoryCorner (Coords x2 y2) _ _) = (x1 == x2) && (y1 == y2)

getCoords (TerritoryCorner coords _ _) = coords

hasOwner player board coords = getOwner board coords == player
unowned board coords = getOwner board coords == none

touchesSide player board (Coords x y)
    | x < boardBound && playerOwns (x+1) y  = True
    | x > 0 && playerOwns (x-1) y           = True
    | y < boardBound && playerOwns x (y+1)  = True
    | y > 0 && playerOwns x (y-1)           = True
    | otherwise                             = False
    where playerOwns i j = hasOwner player board (Coords i j)

legal player board coords = unowned board coords && not (touchesSide player board coords)

onBoard (Coords x y) = (x >= 0) && (x < boardSize) && (y >= 0) && (y < boardSize)

getNotTakenCorners board =
    filter (unowned board . getCoords)

validOffset player board (Coords x y) (Offsets i j) =
    let coords = Coords (x+i) (y+j)
     in onBoard coords && legal player board coords

translateCorner player board (Coords x y) (PieceCorner (Offsets i j) cornerType) =
    let coords = Coords (x+i) (y+j)
        bitmap = getBitmap cornerType (validOffset player board coords)
     in TerritoryCorner coords cornerType bitmap

getCornersForMovingPlayer player board coords addedCorners prevCorners =
    let translated = filter (onBoard . getCoords) $ map (translateCorner player board coords) addedCorners
        legalHere = legal player board . getCoords
     in nub $ filter legalHere $ prevCorners ++ translated

getCornersAfterMove boardAfterMove player coords (Placement _ _ _ addedCorners _) corners =
    let forMovingPlayer = getCornersForMovingPlayer player boardAfterMove coords addedCorners
        forRest = getNotTakenCorners boardAfterMove
     in forPlayerAndRest player forMovingPlayer forRest corners

initialCorners =
    let mkCorner player x y cornerType = [translateCorner player emptyBoard (Coords x y) (PieceCorner (Offsets 0 0) cornerType)]
     in (mkCorner red 0 0 UpperRight,
         mkCorner green boardBound 0 UpperLeft,
         mkCorner yellow boardBound boardBound LowerLeft,
         mkCorner blue 0 boardBound LowerRight)
