module Placement (
    initialPlacements,
    getPlacementsAfterMove,
    getPiece,
    hasPiece,
    -- below here visible only for testing
    getTransformations,
    legalCorners,
) where

import Data.Int
import Data.List

import Types
import Player
import Offset
import Utils

instance Show Placement where
    show (Placement _ offsets _ _) =
        let char x y =
                if x == 0 && y == 0 then 'x'
                else if Offsets x y `elem` offsets then '.'
                else ' '
            showRow x = [char x y | y <- [-4..4]] ++ "\n"
            rows = map showRow [-4..4]
         in concat [row | row <- rows, ('.' `elem` row) || ('x' `elem` row)]

instance Eq Offsets where
    (Offsets x1 y1) == (Offsets x2 y2) = (x1 == x2) && (y1 == y2)

instance Eq PieceCorner where
    (PieceCorner o1 _) == (PieceCorner o2 _) = o1 == o2

touchesOn a b = abs (a-b) <= 1
touches x y (Offsets i j) = (touchesOn x i && (y == j)) || (touchesOn y j && (x == i))
legal offsets (PieceCorner (Offsets x y) _) = not $ any (touches x y) offsets

corners (Offsets x y) =
    [PieceCorner (Offsets (x+1) (y+1)) UpperRight,
     PieceCorner (Offsets (x+1) (y-1)) LowerRight,
     PieceCorner (Offsets (x-1) (y+1)) UpperLeft,
     PieceCorner (Offsets (x-1) (y-1)) LowerLeft]

legalCorners offsets =
    let pieceCorners = concatMap corners $ Offsets 0 0:offsets
     in nub $ filter (legal offsets) pieceCorners

getTransformations piece =
    let offsets = fromOffsets $ Offsets 0 0:getOffsets piece
        transformed = concatMap translations $ concatMap rotations $ reflections offsets
        removeOrigin = filter (\(x,y) -> not $ (x == 0) && (y == 0))
     in map (toOffsets . removeOrigin) $ nub $ map sort transformed

buildPlacement piece offsets = Placement piece offsets (legalCorners offsets) (toBitmap offsets)

allPieces = [minBound..maxBound] :: [Piece]
allPlacements = [buildPlacement piece offsets | piece <- allPieces, offsets <- getTransformations piece]
initialPlacements = replicate numPlayers allPlacements

getPiece (Placement piece _ _ _) = piece
hasPiece piece placement = piece == getPiece placement

getPlacementOffsets (Placement _ offsets _ _) = offsets

getPlacementsAfterMove :: Player -> Placement -> [[Placement]] -> [[Placement]]
getPlacementsAfterMove player (Placement piece _ _ _) placements =
    let index = getIndex player
        moverPlacements = placements !! index
        moverPlacements' = filter (not . hasPiece piece) moverPlacements
     in replaceAt index placements moverPlacements'
