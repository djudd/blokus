module GameState (

) where

import Board
import Corner
import Placement

type Turn = Int

data GameState = State Turn Board [[TerritoryCorner]]

newGame = State 0 empty initialCorners

afterMove :: GameState -> TerritoryCorner -> Placement -> GameState
afterMove (State turn board playerCorners) (TerritoryCorner x y direction _) (Placement _ offsets pieceCorners _) =
    let player = fromIntegral $ (turn `mod` 4) + 1
        board' = assign player x y offsets board
        remainingCorners = getNotTakenCorners board playerCorners
        moverCorners = getCornersForMovingPlayer player board' (playerCorners !! (fromIntegral player)) x y pieceCorners
        playerCorners' = (take (fromIntegral player) remainingCorners) ++ [moverCorners] ++ (drop (fromIntegral $ player+1) remainingCorners)
    in State (turn+1) board' playerCorners'

