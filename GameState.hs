module GameState (
    newGame,
    getChildren,
    -- below here visible only for testing
    legalAt
) where

import Data.Bits

import Types
import Board
import Territory
import Placement

instance Show GameState where
    show (State _ board _ _) = showBoard board

newGame = State 0 emptyBoard initialCorners initialPlacements

currentPlayer :: Turn -> Player
currentPlayer turn = (turn `mod` 4) + 1

getChild (State turn board corners placements) (TerritoryCorner coords direction _) placement =
    let player = currentPlayer turn
        move = Move player coords placement
        turn' = turn + 1
        board' = getBoardAfterMove board move
        corners' = getCornersAfterMove board' move corners
        placements' = getPlacementsAfterMove move placements
     in State turn' board' corners' placements'

legalAt (TerritoryCorner _ _ cornerBitmap) (Placement _ _ _ placementBitmap) = (placementBitmap .&. cornerBitmap) == placementBitmap

getChildren (State turn board corners placements) =
    let getMyChild = getChild (State turn board corners placements)
        mover = fromIntegral $ currentPlayer turn
        moverCorners = corners !! (mover - 1)
        moverPlacements = placements !! (mover - 1)
     in [getMyChild corner placement | corner <- moverCorners, placement <- moverPlacements, legalAt corner placement]
