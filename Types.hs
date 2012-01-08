module Types where

import Data.Int
import Data.Word
import Data.Vector.Unboxed (Vector)

boardSize = 20 :: Int8

type Turn = Int8

newtype Player = Player Int deriving (Eq,Ord,Enum,Bounded,Show)
(none:red:green:yellow:blue:_) = [Player 0 ..]

allPlayers = [minBound..maxBound] :: [Player]
numPlayers = 4 :: Int

type Coord = Int8
type Offset = Int8

data Coords = Coords !Coord !Coord deriving (Show)
data Offsets = Offsets !Offset !Offset deriving (Show)

data CornerType = UpperRight | UpperLeft | LowerRight | LowerLeft deriving (Eq,Ord,Enum,Bounded,Show)

allCornerTypes = [minBound..maxBound] :: [CornerType]
numCorners = length allCornerTypes

type ValidityBitmap = Word32

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
    deriving (Show,Eq,Enum,Bounded,Ord)

allPieces = [minBound..maxBound] :: [Piece]
numPieces = length allPieces

data PieceCorner = PieceCorner !Offsets !CornerType deriving (Show)

data Placement = Placement !Piece !CornerType ![Offsets] ![PieceCorner] !ValidityBitmap

newtype Board = Board (Vector Word64)

data TerritoryCorner = TerritoryCorner Coords CornerType ValidityBitmap

data Move = Move Coords Placement

data GameState = State !Turn Board !([TerritoryCorner],[TerritoryCorner],[TerritoryCorner],[TerritoryCorner]) !([Piece],[Piece],[Piece],[Piece])
