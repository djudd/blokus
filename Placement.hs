module Placement (
    getPiecesAfterMove,
    getPlacementsFor,
    -- below here visible only for testing
    getTransformations,
    legalCorners,
) where

import Data.Int
import Data.List
import Data.Maybe

import Types
import Player
import Offset
import Utils

instance Show Placement where
    show (Placement _ _ offsets _ _) =
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
        removeOrigin = filter (\(x,y) -> (x /= 0) || (y /= 0))
     in map (toOffsets . removeOrigin) $ nub $ map sort transformed

allReachableAt offsets cornerType = 
    let reachable = getReachableOffsets cornerType
     in all (`elem` reachable) offsets

getOrigins offsets = filter (allReachableAt offsets) allCornerTypes 

buildPlacements piece offsets = [Placement piece origin offsets (legalCorners offsets) (toBitmap offsets) | origin <- getOrigins offsets]

allPlacements = concat [buildPlacements piece offsets | piece <- allPieces, offsets <- getTransformations piece]

-- TODO very inefficient
getPlacementsFor cornerType piece = 
    filter (\(Placement piece' cornerType' _ _ _) -> (piece == piece') && (cornerType == cornerType')) allPlacements

getPlacementOffsets (Placement _ _ offsets _ _) = offsets

getPiecesAfterMove player (Placement piece _ _ _ _) pieces =
    let index = getIndex player
        moverPieces = pieces !! index
        moverPieces' = filter (/= piece) moverPieces
     in replaceAt index pieces moverPieces'
