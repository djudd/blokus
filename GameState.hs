module GameState (
    newGame,
    getChild,
    getChildren,
    getCurrentPlayerPieces,
    getPlayableCorners,
    getPlayablePlacements,
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

getMove (TerritoryCorner coords _ _) placement = Move coords placement

getMoves (State turn board corners placements) =
    let index = getIndex $ fromTurn turn
        myCorners = corners !! index
        myPlacements = placements !! index
     in [getMove corner placement | corner <- myCorners, placement <- myPlacements, legalAt corner placement]

getChild (State turn board corners placements) move =
    let player = fromTurn turn
        turn' = turn + 1
        board' = getBoardAfterMove board player move
        corners' = getCornersAfterMove board' player move corners
        placements' = getPlacementsAfterMove player move placements
     in State turn' board' corners' placements'

getChildren state = map (getChild state) (getMoves state)

getCurrentPlayerPlacements (State turn _ _ placements) = 
    placements !! (getIndex $ fromTurn turn)

getCurrentPlayerPieces state = 
    nub $ map getPiece $ getCurrentPlayerPlacements state 

getCurrentPlayerPiecePlacements piece state = 
    filter (hasPiece piece) (getCurrentPlayerPlacements state)

getCurrentPlayerCorners (State turn _ corners _) =
    corners !! (getIndex $ fromTurn turn)

getPlayableCorners piece state =
    let myCorners = getCurrentPlayerCorners state
        playable corner = any (legalAt corner) (getCurrentPlayerPiecePlacements piece state)
     in filter playable myCorners

getPlayablePlacements corner piece state =
    let piecePlacements = getCurrentPlayerPiecePlacements piece state
     in filter (legalAt corner) piecePlacements
