module Types where

import Data.Int
import Data.Word
import Data.Array.Unboxed

numPlayers :: Int
numPlayers = 4

boardSize :: Int8
boardSize = 20

type Player = Int8 -- TODO range 1..4
type Turn = Int8 -- TODO >= 0

type Coord = Int8 -- TODO range 0..19
type Offset = Int8 -- TODO range -4..4

data CornerType = UpperRight | LowerRight | UpperLeft | LowerLeft deriving (Show,Eq)
type ValidityBitmap = Word64

data Piece =
    OnePiece |
    TwoPiece |
    ThreePiece |
    CrookedThree |
    SquarePiece |
    ShortI |
    ShortT |
    ShortL |
    ShortZ |
    LongI |
    LongT |
    LongL |
    LongZ |
    PPiece |
    FPiece |
    XPiece |
    VPiece |
    UPiece |
    YPiece |
    NPiece |
    WPiece
    deriving (Show,Eq,Enum,Bounded)

data PieceCorner = PieceCorner Offset Offset CornerType deriving (Show)

data Placement = Placement Piece [(Offset,Offset)] [PieceCorner] ValidityBitmap deriving (Show)

type Board = Array Int8 Word64

data TerritoryCorner = TerritoryCorner Coord Coord CornerType ValidityBitmap deriving (Show)

data Move = Move Player Coord Coord Placement

data GameState = State Turn Board [[TerritoryCorner]] [[Placement]]
