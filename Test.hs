{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Data.Word
import Data.Int
import Data.Array.Unboxed
import qualified Data.Bits as Bits

import Types
import Placement
import Board
import Territory
import Offset
import GameState
import Utils

prop_placements_one = length (getTransformations OnePiece) == 1
prop_placements_two = length (getTransformations TwoPiece) == 4
prop_placements_three = length (getTransformations ThreePiece) == 6
prop_placements_crookedThree = length (getTransformations CrookedThree) == 12

instance Eq Offsets where
    (Offsets x1 y1) == (Offsets x2 y2) = (x1 == x2) && (y1 == y2)

instance Arbitrary Offsets where
    arbitrary = do
        x <- elements [-4..4]
        y <- elements [-(4-abs(x))..(4-abs(x))]
        return (Offsets x y)

prop_legalCorners_noOverlap xs = all (\(PieceCorner (Offsets x y) _) -> not $ elem (Offsets x y) xs) (legalCorners xs)
prop_legalCorners_unique xs = (legalCorners xs) == (nub $ legalCorners xs)
prop_legalCorners_corners xs = all (\(PieceCorner (Offsets x y) _) -> any (\(Offsets i j) -> (abs(x-i) == 1) && (abs(y-j) == 1)) xs) (legalCorners xs)

prop_toBitmap_bitsSet =
    let allOffsets = map (\(Placement _ offsets _ _) -> offsets) $ head initialPlacements
        number_bits_set offsets = fromIntegral $ bitsSet $ toBitmap offsets
        correct_number_bits_set offsets = number_bits_set offsets == length offsets
    in all correct_number_bits_set allOffsets

instance Arbitrary Coords where
    arbitrary = do
        x <- elements [0..boardSize-1]
        y <- elements [0..boardSize-1]
        return (Coords x y)

prop_getBoardAfterMove_owner coords =
    let makeMove coords = getBoardAfterMove emptyBoard (Move 1 coords (Placement OnePiece [] [] 0))
    in (==1) $ getOwner (makeMove coords) coords

boardWithOneMove = getBoardAfterMove emptyBoard (Move 1 (Coords 0 0) (Placement OnePiece [] [] 0))

prop_legal_noOverlap = not $ legal 1 boardWithOneMove (Coords 0 0)
prop_legal_noSides = not $ (legal 1 boardWithOneMove (Coords 0 1)) || (legal 1 boardWithOneMove (Coords 1 0))
prop_legal_corner = legal 1 boardWithOneMove (Coords 1 1)
prop_legal_distant = legal 1 boardWithOneMove (Coords (boardSize-1) (boardSize-1))

prop_initialPlacements_len = length initialPlacements == numPlayers
prop_initialCorners_len = length initialCorners == numPlayers

main = $(quickCheckAll)

--main = print $ getChildren newGame
