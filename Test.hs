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

prop_legalCorners_noOverlap xs = all (\(PieceCorner x y _) -> not $ elem (x,y) xs) (legalCorners xs)
prop_legalCorners_unique xs = (legalCorners xs) == (nub $ legalCorners xs)
prop_legalCorners_corners xs = all (\(PieceCorner x y _) -> any (\(i,j) -> (abs(x-i) == 1) && (abs(y-j) == 1)) xs) (legalCorners xs)

prop_toBitmap_bitsSet =
    let allOffsets = map (\(Placement _ offsets _ _) -> offsets) $ head initialPlacements
        number_bits_set offsets = fromIntegral $ bitsSet $ toBitmap offsets
        correct_number_bits_set offsets = number_bits_set offsets == length offsets
    in all correct_number_bits_set allOffsets

coords :: Gen (Coord,Coord)
coords = do
    x <- elements [0..boardSize-1]
    y <- elements [0..boardSize-1]
    return (x,y)

prop_getBoardAfterMove_owner =
    let makeMove x y = getBoardAfterMove emptyBoard (Move 1 x y (Placement OnePiece [] [] 0))
    in forAll coords $ \(x,y) -> (==1) $ getOwner (makeMove x y) x y

boardWithOneMove = getBoardAfterMove emptyBoard (Move 1 0 0 (Placement OnePiece [] [] 0))

prop_legal_noOverlap = not $ legal 1 boardWithOneMove (0,0)
prop_legal_noSides = not $ (legal 1 boardWithOneMove (0,1)) || (legal 1 boardWithOneMove (1,0))
prop_legal_corner = legal 1 boardWithOneMove (1,1)
prop_legal_distant = legal 1 boardWithOneMove (boardSize-1,boardSize-1)

prop_initialPlacements_len = length initialPlacements == numPlayers
prop_initialCorners_len = length initialCorners == numPlayers

--main = $(quickCheckAll)

main = print $ getChildren newGame
