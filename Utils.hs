module Utils (
    reflect,
    reflections,
    rotate,
    rotations,
    translate,
    translations,
    bitsSet
) where

import qualified Data.Bits as Bits
import Data.Word

reflect (i,j) offsets = [(i*x,j*y) | (x,y) <- offsets]
reflections offsets =
    [reflect reflector offsets | reflector <- [(1,1),(1,-1),(-1,1),(-1,-1)]]

rotate (i,j) offsets =
    let offsets' = if i == j then [(y,x) | (x,y) <- offsets] else offsets
    in [(i*x,j*y) | (x,y) <- offsets']
rotations offsets =
    [rotate rotator offsets | rotator <- [(1,1),(-1,1),(1,-1),(-1,-1)]]

translate (i,j) offsets = [(x+i,y+j) | (x,y) <- offsets]
translations offsets =
    let all = [translate (-i,-j) offsets | (i,j) <- offsets]
    in filter (elem (0,0)) all

bitsSet :: Word32 -> Int
_bitsSet 0 = 0
_bitsSet v | v > 0 = (Bits..&.) v 1 + _bitsSet (Bits.shift v (-1))
bitsSet = fromIntegral . _bitsSet
