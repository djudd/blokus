module GameState (
    newGame,
    getChild,
    getChildren,
    getCurrentPlayerPieces,
    getPlayableCorners,
    getPlayablePlacements,
    getPlayerIndex,
    -- below here visible only for testing
    legalAt
) where

import Data.Bits
import Data.List

import Types
import Player
import Board
import Territory
import Placement

instance Show GameState where
    show (State _ board _ _) = showBoard board    

newGame = State 0 emptyBoard initialCorners initialPlacements

legalAt (TerritoryCorner _ _ cornerBitmap) (Placement _ _ _ placementBitmap) = (placementBitmap .&. cornerBitmap) == placementBitmap

getChildren (State turn board corners placements) =
    let index = getIndex (fromTurn turn)
        myCorners = corners !! index
        myPlacements = placements !! index
        getMyChild (TerritoryCorner coords _ _) = getChild (State turn board corners placements) coords
     in [getMyChild corner placement | corner <- myCorners, placement <- myPlacements, legalAt corner placement]

getChild (State turn board corners placements) coords placement =
    let player = fromTurn turn
        turn' = turn + 1
        board' = getBoardAfterMove board player coords placement
        corners' = getCornersAfterMove board' player coords placement corners
        placements' = getPlacementsAfterMove player placement placements
     in State turn' board' corners' placements'

getPlayerIndex (State turn _ _ _) = 
    getIndex (fromTurn turn)

getCurrentPlayerPlacements (State turn _ _ placements) = 
    placements !! getIndex (fromTurn turn)

getCurrentPlayerPieces state = 
    nub $ map getPiece $ getCurrentPlayerPlacements state 

getCurrentPlayerPiecePlacements piece state = 
    filter (hasPiece piece) (getCurrentPlayerPlacements state)

getCurrentPlayerCorners (State turn _ corners _) =
    corners !! getIndex (fromTurn turn)

getPlayableCorners piece state =
    let myCorners = getCurrentPlayerCorners state
        playable corner = any (legalAt corner) (getCurrentPlayerPiecePlacements piece state)
     in filter playable myCorners

getPlayablePlacements corner piece state =
    let piecePlacements = getCurrentPlayerPiecePlacements piece state
     in filter (legalAt corner) piecePlacements
