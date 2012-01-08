module Cmdline (
    getNextMoveIfAny    
) where

import Data.Maybe
import Data.List

import Types
import Placement
import Territory
import Player
import GameState

getCommand options optionType optionSeparator parse =
    if length options == 1
        then return $ head options
        else do
            let description = concat $ intersperse optionSeparator $ map show options
            putStr $ "Enter " ++ optionType ++ " to play among:\n" ++ description ++ "\n"
            input <- getLine
            let choice = parse options input
            if isNothing choice
                then getCommand options optionType optionSeparator parse
                else return $ fromJust choice

parseByShow options input = 
    case elemIndex input $ map show options of
        Nothing -> Nothing
        Just index -> Just $ options !! index

parsePiece state options input =
    case parseByShow options input of
        Nothing -> Nothing
        Just piece -> case getPlayableCorners piece state of
            [] -> Nothing
            _ -> Just piece

maybeRead = fmap fst . listToMaybe . reads

parsePlacement options input = 
    let maybeIndex = maybeRead input :: Maybe Int
     in case maybeIndex of 
        Nothing -> Nothing
        Just index -> if index < 0 || index >= length options then Nothing else Just $ options !! index

getPlayedPiece (State turn board corners pieces) = 
    let myPieces = getPlayers (fromTurn turn) pieces
     in getCommand myPieces "piece" " " $ parsePiece (State turn board corners pieces)

getPlayedCorner piece state =
    let corners = getPlayableCorners piece state
     in getCommand corners "corner" " " parseByShow

getPlayedPlacement corner piece =
    let placements = getPlacementsAt corner piece
     in getCommand placements "index of placement" "\n" parsePlacement

confirm state (Move coords placement) = do
    let child = getChild state coords placement
    print child
    print "Is this correct (y/n)?"
    answer <- getLine
    if answer == "y"
        then return child
        else getNextMove state

getNextMove state = do
    print state
    piece <- getPlayedPiece state
    corner <- getPlayedCorner piece state
    placement <- getPlayedPlacement corner piece
    let move = Move (getCoords corner) placement
    child <- confirm state move
    getNextMoveIfAny child

getNextMoveIfAny state =
    if null $ getChildren state
        then return state
        else getNextMove state

