import Data.Maybe
import Data.List

import Types
import Placement
import Territory
import GameState

getCommand options request parse = do
    if (length options) == 1
        then return $ head options
        else do
            print $ "Enter " ++ request ++ " to play " ++ (show options) ++ ":"
            input <- getLine
            let choice = parse options input
            if isNothing choice
                then getCommand options request parse
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

getPlayedPiece state = 
    let pieces = getCurrentPlayerPieces state
     in getCommand pieces "piece" (parsePiece state)

getPlayedCorner piece state =
    let corners = getPlayableCorners piece state
     in getCommand corners "corner" parseByShow

maybeRead = fmap fst . listToMaybe . reads

parsePlacement options input = 
    let maybeIndex = maybeRead input :: Maybe Int
     in case maybeIndex of 
        Nothing -> Nothing
        Just index -> if index < 0 || index >= (length options) then Nothing else Just $ options !! index

getPlayedPlacement corner piece state =
    let placements = getPlayablePlacements corner piece state
     in getCommand placements "index of placement" parsePlacement

confirm state move = do
    print $ getChild state move
    print "Is this correct (y/n)?"
    answer <- getLine
    if answer == "y"
        then return $ getChild state move
        else getNextMove state

getNextMove state = do
    print state
    piece <- getPlayedPiece state
    corner <- getPlayedCorner piece state
    placement <- getPlayedPlacement corner piece state
    let move = Move (getCoords corner) placement
    child <- confirm state move
    getNextMoveIfAny child

getNextMoveIfAny state = do
    if null $ getChildren state
        then return state
        else getNextMove state

main = getNextMove newGame
    
