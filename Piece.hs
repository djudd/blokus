module Piece (
    Offset,
    allPieces,
    placements
) where

import Data.Int
import Data.Set as Set

type Offset = Int8

data Piece = Piece [(Offset, Offset)] deriving Show

one = Piece $ []
two = Piece $ (0,1):[]
three = Piece $ (0,1):(0,2):[]
shortI = Piece $ (0,1):(0,2):(0,3):[]
longI = Piece $ (0,1):(0,2):(0,3):(0,4):[]
x = Piece $ (0,1):(1,0):(0,-1):(-1,0):[]
square = Piece $ (0,1):(1,0):(1,1):[]
crookedThree = Piece $ (0,1):(1,0):[]
shortL = Piece $ (0,1):(1,0):(0,2):[]
longL = Piece $ (0,1):(1,0):(0,2):(0,3):[]
p = Piece $ (0,1):(1,0):(1,1):(0,2):[]
shortT = Piece $ (0,1):(1,1):(-1,1):[]
longT = Piece $ (0,1):(0,2):(1,2):(-1,2):[]
shortZ = Piece $ (0,1):(1,1):(1,2):[]
longZ = Piece $ (0,1):(1,1):(0,-1):(-1,-1):[]
y = Piece $ (0,1):(1,0):(-1,0):(-2,0):[]
n = Piece $ (0,1):(1,1):(2,1):(3,1):[]
f = Piece $ (0,1):(1,0):(0,-1):(-1,-1):[]
v = Piece $ (0,1):(0,2):(1,0):(2,0):[]
w = Piece $ (0,1):(1,1):(-1,0):(-1,-1):[]
u = Piece $ (0,1):(0,-1):(1,1):(1,-1):[]

allPieces = one:two:three:crookedThree:square:shortI:shortT:shortL:shortZ:p:longI:longT:longL:longZ:f:x:v:u:y:n:w:[]

reflect (i,j) offsets = [(i*x,j*y) | (x,y) <- offsets]
reflections offsets = 
    [reflect reflector offsets | reflector <- (1,1):(1,-1):(-1,1):(-1,-1):[]]

rotate (i,j) offsets = 
    let offsets' = if i == j then [(y,x) | (x,y) <- offsets] else offsets
    in [(i*x,j*y) | (x,y) <- offsets']
rotations offsets = 
    [rotate rotator offsets | rotator <- (1,1):(-1,1):(1,-1):(-1,-1):[]]

translate (i,j) offsets = [(i+x,j+y) | (x,y) <- offsets]
translations offsets =
    let all = [translate translation offsets | translation <- offsets]
    in Prelude.filter (elem (0,0)) all

placements (Piece offsets) = 
    let offsets' = (0,0):offsets
        all = concat $ Prelude.map translations $ concat $ Prelude.map rotations $ reflections offsets'
    in Set.fromList all


