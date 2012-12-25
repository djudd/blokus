module GameState (
    newGame,
    getChild,
    getChildren,
    getPlayableCorners,
    getLegalPlacements,
    getPlayer,
) where

import Data.Bits
import Data.List

import Types
import Player
import Board
import Territory
import Placement

instance Show GameState where
    show (State _ board _ _) = show board    

newGame = State 0 emptyBoard initialCorners (allPieces,allPieces,allPieces,allPieces)

legalAt !(TerritoryCorner _ _ cornerBitmap) !(Placement _ _ _ _ placementBitmap) = 
    (placementBitmap .&. cornerBitmap) == placementBitmap

getPlacementsAt !(TerritoryCorner _ cornerType _) = getPlacementsFor cornerType

getChildren state =
    let (State turn _ corners pieces) = state
        getMy = getPlayers (fromTurn turn)
        myCorners = getMy corners
        myPieces = getMy pieces
        getChildAt (TerritoryCorner coords _ _) = getChild state coords
     in {-# SCC getChildren_loop #-} [
            getChildAt corner placement | 
            corner <- myCorners, 
            piece <- myPieces, 
            placement <- getPlacementsAt corner piece,
            legalAt corner placement
        ]

getPiecesAfterMove player (Placement piece _ _ _ _) = forPlayer player (filter (/= piece))

getChild (State turn board corners pieces) coords placement =
    let player = fromTurn turn
        turn' = turn + 1
        board' = getBoardAfterMove board player coords placement
        corners' = getCornersAfterMove board' player coords placement corners
        pieces' = getPiecesAfterMove player placement pieces
     in State turn' board' corners' pieces'

getLegalPlacements corner piece =
    filter (legalAt corner) (getPlacementsAt corner piece)

getPlayableCorners piece (State turn _ corners pieces) =
    let player = fromTurn turn
        myCorners = getPlayers player corners
        myPieces = getPlayers player pieces
        hasPlacementsOf corner piece = any (legalAt corner) (getPlacementsAt corner piece)
        playable corner = any (hasPlacementsOf corner) myPieces
     in filter playable myCorners

getPlayer (State turn _ _ _) = fromTurn turn
