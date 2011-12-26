module Piece (
    Piece,
    Offset,
    PieceLabel,
    allPieces,
    getLabel,
    getPlacements
) where

import Data.Int
import Data.List

type Offset = Int8
type PieceLabel = Char

data Piece = Piece PieceLabel [(Offset, Offset)] deriving Show

one = Piece '1' $ []
two = Piece '2' $ (0,1):[]
three = Piece '3' $ (0,1):(0,2):[]
shortI = Piece 'i' $ (0,1):(0,2):(0,3):[]
longI = Piece 'I' $ (0,1):(0,2):(0,3):(0,4):[]
x = Piece 'x' $ (0,1):(1,0):(0,-1):(-1,0):[]
square = Piece 's' $ (0,1):(1,0):(1,1):[]
crookedThree = Piece 'c' $ (0,1):(1,0):[]
shortL = Piece 'l' $ (0,1):(1,0):(0,2):[]
longL = Piece 'L' $ (0,1):(1,0):(0,2):(0,3):[]
p = Piece 'p' $ (0,1):(1,0):(1,1):(0,2):[]
shortT = Piece 't' $ (0,1):(1,1):(-1,1):[]
longT = Piece 'T' $ (0,1):(0,2):(1,2):(-1,2):[]
shortZ = Piece 'z' $ (0,1):(1,1):(1,2):[]
longZ = Piece 'Z' $ (0,1):(1,1):(0,-1):(-1,-1):[]
y = Piece 'y' $ (0,1):(1,0):(-1,0):(-2,0):[]
n = Piece 'n' $ (0,1):(1,1):(2,1):(3,1):[]
f = Piece 'f' $ (0,1):(1,0):(0,-1):(-1,-1):[]
v = Piece 'v' $ (0,1):(0,2):(1,0):(2,0):[]
w = Piece 'w' $ (0,1):(1,1):(-1,0):(-1,-1):[]
u = Piece 'u' $ (0,1):(0,-1):(1,1):(1,-1):[]

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
    in filter (elem (0,0)) all

getLabel (Piece label _) = label

getPlacements :: Piece -> [[(Offset,Offset)]]
getPlacements (Piece _ offsets) = 
    let offsets' = (0,0):offsets
        all = concat $ map translations $ concat $ map rotations $ reflections offsets'
    in nub all


