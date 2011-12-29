{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Data.Word
import Data.Int
import qualified Data.Bits as Bits

import Types
import Placement
import Board
import Corner
import Offset
import Utils

prop_placements_one = length (filter (hasPiece OnePiece) allPlacements) == 1
prop_placements_two = length (filter (hasPiece TwoPiece) allPlacements) == 4
prop_placements_three = length (filter (hasPiece ThreePiece) allPlacements) == 6
prop_placements_crookedThree = length (filter (hasPiece CrookedThree) allPlacements) == 12

prop_legalCorners_noOverlap xs = all (\(PieceCorner x y _) -> not $ elem (x,y) xs) (legalCorners xs)
prop_legalCorners_unique xs = (legalCorners xs) == (nub $ legalCorners xs)
prop_legalCorners_corners xs = all (\(PieceCorner x y _) -> any (\(i,j) -> (abs(x-i) == 1) && (abs(y-j) == 1)) xs) (legalCorners xs)

prop_toBitmap_bitsSet =
    let allOffsets = map (\(Placement _ offsets _ _) -> offsets) allPlacements
        number_bits_set offsets = fromIntegral $ bitsSet $ toBitmap offsets
        correct_number_bits_set offsets = number_bits_set offsets == length offsets
    in all correct_number_bits_set allOffsets

coords :: Gen (Coord,Coord)
coords = do
    x <- elements [0..boardSize-1]
    y <- elements [0..boardSize-1]
    return (x,y)

prop_getBoardAfterMove_setsOwner =
    let makeMove x y = getBoardAfterMove emptyBoard (Move 1 x y (Placement OnePiece [(0,0)] [] 0))
    in forAll coords $ \(x,y) -> not $ unowned (makeMove x y) (x, y)

main = $(quickCheckAll)
