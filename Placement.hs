import Data.Bits
import Data.Int
import Data.Word

import Piece

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

toBitmap :: [(Offset,Offset)] -> Word64
toBitmap offsets = foldl setBit' 0 offsets 
    where setBit' bitmap (x,y) = setBit bitmap $ fromIntegral $ offsetsIdx x y

main = print $ toBitmap [(0,0),(0,1)]
