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

forPlayer player f = forPlayerAndRest player f id 

forPlayerAndRest player f g (a, b, c, d)
    | player == red     = (f a, g b, g c, g d)
    | player == green   = (g a, f b, g c, g d)
    | player == yellow  = (g a, g b, f c, g d)
    | player == blue    = (g a, g b, g c, f d)

getPlayers player (a, b, c, d)
    | player == red     = a
    | player == green   = b
    | player == yellow  = c
    | player == blue    = d
