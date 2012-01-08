module Offset (
    getBitmap,
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

getBitmap :: CornerType -> (Offsets -> Bool) -> ValidityBitmap
getBitmap cornerType valid = 
    fst $ foldl setNextBit (0,0) (getReachableOffsets cornerType)
    where
        setNextBit (bitmap, bit) offsets = (setBitIfValid offsets bitmap bit, bit + 1)
        setBitIfValid offsets bitmap bit = if valid offsets then setBit bitmap bit else bitmap

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

rotatedBy directions = toOffsets $ rotate directions reachableOffsets

getReachableOffsets UpperRight = rotatedBy (1,1)
getReachableOffsets UpperLeft = rotatedBy (-1,1)
getReachableOffsets LowerRight = rotatedBy (1,-1)
getReachableOffsets LowerLeft = rotatedBy (-1,-1)

fromOffsets = map (\(Offsets x y) -> (x,y))
toOffsets = map (uncurry Offsets)
