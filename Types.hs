module Types where

import Data.Int
import Data.Word
import Data.Array.Unboxed

numPlayers :: Int
numPlayers = 4

boardSize :: Int8
boardSize = 20

type Turn = Int8

newtype Player = Player Int deriving (Eq,Ord,Enum,Bounded,Show)
(none:red:green:yellow:blue:_) = [Player 0 ..]

type Coord = Int8
type Offset = Int8

data Coords = Coords !Coord !Coord deriving (Show)
data Offsets = Offsets !Offset !Offset deriving (Show)

data CornerType = UpperRight | UpperLeft | LowerRight | LowerLeft deriving (Eq,Ord,Enum,Bounded,Show)

allCornerTypes = [minBound..maxBound] :: [CornerType]

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

allPieces = [minBound..maxBound] :: [Piece]

data PieceCorner = PieceCorner !Offsets !CornerType deriving (Show)

data Placement = Placement !Piece !CornerType ![Offsets] ![PieceCorner] !ValidityBitmap

type Board = Array Int8 Word64

data TerritoryCorner = TerritoryCorner !Coords !CornerType !ValidityBitmap

data Move = Move Coords Placement

data GameState = State Turn Board [[TerritoryCorner]] [[Piece]]
