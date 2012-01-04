module Offset (
    toBitmap,
    getOffsets,
    getReachableOffsets,
    fromOffsets,
    toOffsets
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

toBitmap :: [Offsets] -> ValidityBitmap
toBitmap offsets = foldl setBit' initial offsets
    where setBit' bitmap (Offsets x y) = setBit bitmap $ fromIntegral $ offsetsIdx x y
          initial = setBit' 0 (Offsets 0 0)

getOffsets piece = toOffsets $ case piece of
    OnePiece -> []
    TwoPiece -> [(0,1)]
    ThreePiece -> [(0,1),(0,2)]
    CrookedThree -> [(0,1),(1,0)]
    SquarePiece -> [(0,1),(1,0),(1,1)]
    ShortI -> [(0,1),(0,2),(0,3)]
    ShortT -> [(0,1),(1,1),(-1,1)]
    ShortL -> [(0,1),(1,0),(0,2)]
    ShortZ -> [(0,1),(1,1),(1,2)]
    LongI -> [(0,1),(0,2),(0,3),(0,4)]
    LongT -> [(0,1),(0,2),(1,2),(-1,2)]
    LongL -> [(0,1),(1,0),(0,2),(0,3)]
    LongZ -> [(0,1),(1,1),(0,-1),(-1,-1)]
    PPiece -> [(0,1),(1,0),(1,1),(0,2)]
    FPiece -> [(0,1),(1,0),(0,-1),(-1,-1)]
    XPiece -> [(0,1),(1,0),(0,-1),(-1,0)]
    VPiece -> [(0,1),(0,2),(1,0),(2,0)]
    UPiece -> [(0,1),(0,-1),(1,1),(1,-1)]
    YPiece -> [(0,1),(1,0),(-1,0),(-2,0)]
    NPiece -> [(0,1),(1,1),(2,1),(3,1)]
    WPiece -> [(0,1),(1,1),(-1,0),(-1,-1)]

reachableOffsets :: [(Offset,Offset)]
reachableOffsets = [(1,-3),(0,-2),(1,-2),(2,-2),(1,-1),(2,-1),(3,-1),(-2,0),(1,0),(2,0),(3,0),(4,0),(-3,1),(-2,1),(-1,1),(0,1),(1,1),(2,1),(3,1),(-2,2),(-1,2),(0,2),(1,2),(2,2),(-1,3),(0,3),(1,3),(0,4)]

getReachableOffsets cornerType = toOffsets $ case cornerType of
    UpperRight -> rotate (1,1) reachableOffsets
    UpperLeft -> rotate (-1,1) reachableOffsets
    LowerRight -> rotate (1,-1) reachableOffsets
    LowerLeft -> rotate (-1,-1) reachableOffsets

fromOffsets = map (\(Offsets x y) -> (x,y))
toOffsets = map (uncurry Offsets)
