module Offset (
    toBitmap,
    getReachableOffsets
) where

import Data.Bits hiding (rotate)
import Data.Int
import Data.Word
import Data.List

import Types
import Utils

offsetsIdx :: Offset -> Offset -> Int8
offsetsIdx x y = case x+4 of
    0 -> 0
    1 -> 1 + (y+1) -- -1 <= y <= 1
    2 -> 4 + (y+2) -- -2 <= y <= 2
    3 -> 9 + (y+3) -- ...
    4 -> 16 + (y+4)
    5 -> 25 + (y+3)
    6 -> 32 + (y+2)
    7 -> 37 + (y+1)
    8 -> 40

toBitmap :: [(Offset,Offset)] -> ValidityBitmap
toBitmap offsets = foldl setBit' 0 offsets
    where setBit' bitmap (x,y) = setBit bitmap $ fromIntegral $ offsetsIdx x y

reachableOffsets :: [(Offset,Offset)]
reachableOffsets = [(1,-3),(0,-2),(1,-2),(2,-2),(1,-1),(2,-1),(3,-1),(-2,0),(1,0),(2,0),(3,0),(4,0),(-3,1),(-2,1),(-1,1),(0,1),(1,1),(2,1),(3,1),(-2,2),(-1,2),(0,2),(1,2),(2,2),(-1,3),(0,3),(1,3),(0,4)]

getReachableOffsets UpperRight = rotate (1,1) reachableOffsets
getReachableOffsets UpperLeft = rotate (-1,1) reachableOffsets
getReachableOffsets LowerRight = rotate (1,-1) reachableOffsets
getReachableOffsets LowerLeft = rotate (-1,-1) reachableOffsets

