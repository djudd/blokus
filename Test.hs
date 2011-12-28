{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import Piece
import Placement
import Corner

assert_placements_piece_len idx len = length (getPlacements piece) == len
    where piece = allPieces !! idx

prop_placements_one = assert_placements_piece_len 0 1
prop_placements_two = assert_placements_piece_len 1 4
prop_placements_three = assert_placements_piece_len 2 6
prop_placements_crookedThree = assert_placements_piece_len 3 12

main = $(quickCheckAll)
