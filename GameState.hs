--module GameState (

--) where

import Board
import Corner
import Placement

type Turn = Int

data GameState = State Turn Board [[TerritoryCorner]] [[Placement]]

instance Show GameState where 
    show (State _ board _ _) = showBoard board

newGame = State 0 emptyBoard initialCorners initialPlacements

replaceAt index list value = (take index list) ++ [value] ++ (drop (index+1) list)

currentPlayer turn = (turn `mod` 4) + 1

getChild :: GameState -> TerritoryCorner -> Placement -> GameState
getChild (State turn board playerCorners placements) (TerritoryCorner x y direction _) (Placement pieceLabel offsets pieceCorners _) =
    let turn' = turn+1
        player = fromIntegral $ currentPlayer turn
        board' = assign player x y offsets board
        moverPlacements = placements !! (fromIntegral player)
        moverPlacements' = filter (not . (hasLabel pieceLabel)) moverPlacements
        placements' = replaceAt (fromIntegral player) placements moverPlacements'
        moverCorners = playerCorners !! (fromIntegral player)
        moverCorners' = getCornersForMovingPlayer player board' moverCorners x y pieceCorners
        remainingCorners = getNotTakenCorners board playerCorners
        playerCorners' = replaceAt (fromIntegral player) remainingCorners moverCorners' 
     in State turn' board' playerCorners' placements'

getChildren (State turn board playerCorners placements) = 
    let player = currentPlayer turn
        getMyChild = getChild (State turn board playerCorners placements)
        moverPlacements = placements !! (fromIntegral player)
        moverCorners = playerCorners !! (fromIntegral player)
     in [getMyChild corner placement | corner <- moverCorners, placement <- moverPlacements]

main = print $ getChildren newGame
