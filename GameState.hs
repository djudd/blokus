module GameState (
    newGame,
    getMoves,
    getChildren,
    -- below here visible only for testing
    legalAt
) where

import Data.Bits

import Types
import Player
import Board
import Territory
import Placement

instance Show GameState where
    show (State _ board _ _) = showBoard board    

newGame = State 0 emptyBoard initialCorners initialPlacements

legalAt (TerritoryCorner _ _ cornerBitmap) (Placement _ _ _ placementBitmap) = (placementBitmap .&. cornerBitmap) == placementBitmap

getMove (TerritoryCorner coords _ _) placement = Move coords placement

getMoves (State turn board corners placements) =
    let mover = fromTurn turn
        moverIndex = getIndex $ mover
        moverCorners = corners !! moverIndex
        moverPlacements = placements !! moverIndex
     in [getMove corner placement | corner <- moverCorners, placement <- moverPlacements, legalAt corner placement]

getChild (State turn board corners placements) move =
    let player = fromTurn turn
        turn' = turn + 1
        board' = getBoardAfterMove board player move
        corners' = getCornersAfterMove board' player move corners
        placements' = getPlacementsAfterMove player move placements
     in State turn' board' corners' placements'

getChildren state = map (getChild state) (getMoves state)


