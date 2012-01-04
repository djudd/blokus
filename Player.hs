module Player where

import Data.Int
import Data.Word

import Types

fromTurn :: Turn -> Player
fromTurn turn = toEnum $ fromIntegral $ (turn `mod` 4) + 1

getIndex :: Player -> Int
getIndex player = fromEnum player - 1

getOwnershipFlag :: Player -> Word64 
getOwnershipFlag player = fromIntegral $ fromEnum player

fromOwnershipFlag :: Word64 -> Player
fromOwnershipFlag flag = toEnum $ fromIntegral flag
