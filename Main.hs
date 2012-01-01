import Data.Maybe
import Data.List

import Types
import Placement
import Territory
import GameState

getPlayedPiece state = do
    let pieces = getCurrentPlayerPieces state
    print $ "Enter piece to play " ++ (show pieces) ++ ":"
    pieceName <- getLine
    let index = elemIndex pieceName $ map show pieces
    if isNothing index
        then getPlayedPiece state
        else do
            let piece = pieces !! (fromJust index)
            if null $ getPlayableCorners piece state
                then do
                    print "Piece cannot be played anywhere on this board"
                    getPlayedPiece state
                else return piece

getPlayedCorner piece state = do
    let corners = getPlayableCorners piece state
    print $ "Enter corner to play " ++ (concatMap (show . getCoords) corners) ++ ":"
    cornerName <- getLine
    let index = elemIndex cornerName $ map (show . getCoords) corners
    if isNothing index
        then getPlayedCorner piece state
        else return $ corners !! (fromJust index)

getPlayedPlacement corner piece state = do
    let placements = getPlayablePlacements corner piece state
    print $ "Enter index of placement to play " ++ (concatMap (show . getPlacementOffsets) placements) ++ ":"
    placementName <- getLine
    let placementIndex = read placementName :: Int
    if placementIndex < 0 || placementIndex >= (length placements)
        then getPlayedPlacement corner piece state
        else return $ placements !! placementIndex

confirm state move = do
    print $ getChild state move
    print "Is this correct (y/n)?"
    answer <- getLine
    if answer == "y"
        then return $ getChild state move
        else getNextMove state

getNextMove state = do
    let pieces = getCurrentPlayerPieces state
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
    
