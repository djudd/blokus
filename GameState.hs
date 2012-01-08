module GameState (
    newGame,
    getChild,
    getChildren,
    getPlayableCorners,
    getPlacementsAt,
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

getPlacementsAt (TerritoryCorner coords cornerType bitmap) piece =
    let legalHere = legalAt (TerritoryCorner coords cornerType bitmap)
        getPlacementsHere = getPlacementsFor cornerType
     in [placement | placement <- getPlacementsHere piece, legalHere placement]

legalAt (TerritoryCorner _ _ cornerBitmap) (Placement _ _ _ _ placementBitmap) = 
    (placementBitmap .&. cornerBitmap) == placementBitmap

getChildren (State turn board corners pieces) =
    let player = fromTurn turn
        myCorners = getPlayers player corners
        myPieces = getPlayers player pieces
        getMyChild (TerritoryCorner coords _ _) = getChild (State turn board corners pieces) coords
     in [getMyChild corner placement | corner <- myCorners, piece <- myPieces, placement <- getPlacementsAt corner piece]

getPiecesAfterMove player (Placement piece _ _ _ _) = forPlayer player (filter (/= piece))

getChild (State turn board corners pieces) coords placement =
    let player = fromTurn turn
        turn' = turn + 1
        board' = getBoardAfterMove board player coords placement
        corners' = getCornersAfterMove board' player coords placement corners
        pieces' = getPiecesAfterMove player placement pieces
     in State turn' board' corners' pieces'

getPlayableCorners piece (State turn _ corners pieces) =
    let player = fromTurn turn
        myCorners = getPlayers player corners
        myPieces = getPlayers player pieces
        hasPlacementsOf corner = not . null . getPlacementsAt corner
        playable corner = any (hasPlacementsOf corner) myPieces
     in filter playable myCorners

getPlayer (State turn _ _ _) = fromTurn turn
