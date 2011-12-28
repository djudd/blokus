{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Data.Word
import qualified Data.Bits as Bits

import Piece
import Placement
import Corner

assert_placements_piece_len idx len = length (getPlacements piece) == len
    where piece = allPieces !! idx

prop_placements_one = assert_placements_piece_len 0 1
prop_placements_two = assert_placements_piece_len 1 4
prop_placements_three = assert_placements_piece_len 2 6
prop_placements_crookedThree = assert_placements_piece_len 3 12

prop_legalCorners_noOverlap xs = all (\(PieceCorner x y _) -> not $ elem (x,y) xs) (legalCorners xs)
prop_legalCorners_unique xs = (legalCorners xs) == (nub $ legalCorners xs)
prop_legalCorners_corners xs = all (\(PieceCorner x y _) -> any (\(i,j) -> (abs(x-i) == 1) && (abs(y-j) == 1)) xs) (legalCorners xs)

bitsSet :: Word64 -> Word64
bitsSet 0 = 0
bitsSet v | v > 0 = ((Bits..&.) v 1) + (bitsSet $ Bits.shift v (-1))

prop_toBitmap_bitsSet = all correct_number_bits_set $ concat $ map getPlacements allPieces
    where correct_number_bits_set xs = (fromIntegral $ bitsSet $ toBitmap xs) == length xs

main = $(quickCheckAll)
