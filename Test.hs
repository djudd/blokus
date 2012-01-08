{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Data.Word
import Data.Int
import Data.Ord
import qualified Data.Bits as Bits
import qualified Data.Vector as Vector

import Types
import Piece
import Placement
import Board
import Territory
import Offset
import GameState
import Utils

import System.Exit

prop_firstBitSet_zero = 0 == firstBitSet 0
prop_firstBitSet_one = 1 == firstBitSet 1
prop_firstBitSet_two = 2 == firstBitSet 2
prop_firstBitSet_three = 1 == firstBitSet 3
prop_firstBitSet_four = 3 == firstBitSet 4

prop_iteratePieces_OnePiece = [OnePiece] == iteratePieces 1
prop_iteratePieces_all = allPieces == iteratePieces (head initialPieces)

prop_placements_one = length (getTransformations OnePiece) == 1
prop_placements_two = length (getTransformations TwoPiece) == 4
prop_placements_three = length (getTransformations ThreePiece) == 6
prop_placements_crookedThree = length (getTransformations CrookedThree) == 12

instance Arbitrary Offsets where
    arbitrary = do
        x <- elements [-4..4]
        y <- elements [-(4-abs x)..(4-abs x)]
        return (Offsets x y)

prop_legalCorners_noOverlap xs = all (\(PieceCorner (Offsets x y) _) -> notElem (Offsets x y) xs) (legalCorners xs)
prop_legalCorners_unique xs = legalCorners xs == nub (legalCorners xs)
prop_legalCorners_corners xs = all (\(PieceCorner (Offsets x y) _) -> any (\(Offsets i j) -> (abs(x-i) == 1) && (abs(y-j) == 1)) (Offsets 0 0:xs)) (legalCorners xs)
prop_legalCorners_one = (==4) $ length $ (\(Placement _ _ _ corners _) -> corners) $ head $ getPlacementsFor UpperRight OnePiece

prop_getPlacementsFor_neverNull = 
    let playableAt corner piece = not $ null $ getPlacementsFor corner piece
        allPlayableAt corner = all (playableAt corner) allPieces
     in all allPlayableAt allCornerTypes

allPlacements = 
    let allPlacementsFor cornerType = concatMap (getPlacementsFor cornerType) allPieces
     in concatMap allPlacementsFor allCornerTypes

prop_placements_vectorLength = Vector.length placementVector == (numPieces * numCorners)
prop_placements_listAndConcatedVectorLengthsEqual = length (concat $ Vector.toList placementVector) == length placementList
prop_placements_derivedAndListLengthsEqual = length placementList == length allPlacements

prop_toBitmap_bitsSet =
    let allOffsets = map (\(Placement _ _ offsets _ _) -> offsets) allPlacements
        number_bits_set offsets = fromIntegral $ bitsSet $ toBitmap offsets
        correct_number_bits_set offsets = number_bits_set offsets == length offsets + 1
    in all correct_number_bits_set allOffsets

instance Arbitrary Piece where
    arbitrary = elements allPieces

instance Arbitrary CornerType where
    arbitrary = elements allCornerTypes

prop_getKey_inRange piece corner = let key = getKey piece corner in key >= 0 && key < Vector.length placementVector

prop_getKey_pieceOrd corner p1 p2 = compare p1 p2 == comparing (`getKey` corner) p1 p2
prop_getKey_cornerOrd piece c1 c2 = compare c1 c2 == comparing (getKey piece) c1 c2

instance Eq Coords where
    (Coords x1 y1) == (Coords x2 y2) = (x1 == x2) && (y1 == y2)

instance Arbitrary Coords where
    arbitrary = do
        x <- elements [0..boardSize-1]
        y <- elements [0..boardSize-1]
        return (Coords x y)

simplePlacement = Placement OnePiece UpperRight [] [] 0

prop_getBoardAfterMove_owner coords =
    let makeMove coords = getBoardAfterMove emptyBoard red coords simplePlacement
     in (==red) $ getOwner (makeMove coords) coords

prop_getBoardAfterMove_unowned coords = 
    let makeMove coords = getBoardAfterMove emptyBoard red coords simplePlacement
        otherCoords = filter (not . (==coords)) [Coords x y | x <- [0..boardSize-1], y <- [0..boardSize-1]]
     in all (\owner -> (==red) owner || (==none) owner) $ map ((`getOwner` coords) . makeMove) otherCoords
    
boardWithOneMove = getBoardAfterMove emptyBoard red (Coords 0 0) simplePlacement

prop_legal_noOverlap = not $ legal red boardWithOneMove (Coords 0 0)
prop_legal_noSides = not $ legal red boardWithOneMove (Coords 0 1) || legal red boardWithOneMove (Coords 1 0)
prop_legal_corner = legal red boardWithOneMove (Coords 1 1)
prop_legal_distant = legal red boardWithOneMove (Coords (boardSize-1) (boardSize-1))

cornersAfterOneMove = getCornersForMovingPlayer red boardWithOneMove [TerritoryCorner (Coords 0 0) UpperRight 0] (Coords 0 0) [PieceCorner (Offsets 1 1) UpperRight]

prop_getCornersForMovingPlayer_simpleCase = cornersAfterOneMove == [TerritoryCorner (Coords 1 1) UpperRight 0]

prop_initialCorners_len = length initialCorners == numPlayers

prop_allPlacements_bitmapGtZero = all (\(Placement _ _ _ _ bits) -> bits > 0) allPlacements
prop_initialCorners_bitmapGtZero = all (\(TerritoryCorner _ _ bits) -> bits > 0) $ concat initialCorners

prop_getPlacementsAt_initialCorners_lensEqual =
    let allEqual xs = length (nub xs) <= 1
        initCornersEqual piece = allEqual $ map (length . (`getPlacementsAt` piece) . head) initialCorners
     in all initCornersEqual allPieces

prop_getPlacementsAt_initialCorners_nonXNeverNull =
    let nonXPieces = filter (/= XPiece) allPieces
        legalSomehow piece corner = not $ null $ getPlacementsAt corner piece
        legalSomehowEverywhere piece = all (legalSomehow piece) (concat initialCorners)
     in all legalSomehowEverywhere nonXPieces

prop_getChildren_nonEmpty = (>0) $ length $ getChildren newGame
prop_getChildren_corners_len = (==numPlayers) $ length $ (\(State _ _ corners _) -> corners) $ head $ getChildren newGame
prop_getChildren_placements_len = (==numPlayers) $ length $ (\(State _ _ _ pieces) -> pieces) $ head $ getChildren newGame
prop_getChildren_corners_allNotEmpty = all (not . null) $ (\(State _ _ corners _) -> corners) $ head $ getChildren newGame
prop_getChildren_placements_allNotEmpty = all (>0) $ (\(State _ _ _ pieces) -> pieces) $ head $ getChildren newGame

getNthGrandChildren 1 = getChildren newGame
getNthGrandChildren n = concatMap getChildren $ getNthGrandChildren (n-1)

prop_getFirstPlayerSecondMove_nonEmpty = not $ null $ getChildren $ head $ getNthGrandChildren 4

runTests = $(quickCheckAll)

main = do
    success <- runTests
    if success
        then exitSuccess
        else exitFailure 
