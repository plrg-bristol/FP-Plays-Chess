module Chess.Model where

import Data.Map.Strict (Map) -- Requires `containers`

-- Inspired by https://www.chessprogramming.org/Forsyth-Edwards_Notation#FEN_Syntax

-- Pieces
--------------

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Show, Eq, Ord)

data Player = White | Black
  deriving (Show, Eq, Ord)

data Piece = MkPiece Player PieceType
  deriving (Show, Eq, Ord)

getPlayer :: Piece -> Player
getPlayer (MkPiece player _) = player

getPieceType :: Piece -> PieceType
getPieceType (MkPiece _ pieceType) = pieceType

-- Positions
--------------

data File = A | B | C | D | E | F | G | H
  deriving (Show, Eq, Ord, Enum)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Show, Eq, Ord, Enum)

data Pos = MkPos File Rank
  deriving (Show, Eq, Ord)

{-
Why not `data Pos = MkPos Int Int`?
'Parse, don't validate' https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
This way, the Map only stores valid positions.
If the key was two ints, we could have (-1, -4) etc.
which is not a valid position on a chess board.
-}

-- toPos :: File -> Int -> Pos
-- toPos file rankIndex = MkPos file (toEnum (rankIndex - 1)) -- Enum starts at 0, but rank starts at 1, so we need to subtract 1

-- -- >>> toPos D 3
-- -- MkPos D R3

-- Board
----------

type Board = Map Pos Piece

-- Castling ability
--------------------

{-
This is not whether a player can castle in this particular position,
but their right to castle at all during the rest of the game.
Moving a rook forfeits castling rights on its corresponding side.
Moving the king forfeits all castling rights.
-}

data CastlingAbility = MkCA
  { whiteKingside  :: Bool
  , whiteQueenside :: Bool
  , blackKingside  :: Bool
  , blackQueenside :: Bool
  }
  deriving (Show, Eq, Ord)

-- En Passant
---------------

{-
Good candidate for putting in the sheets.

En Passant explanation: https://www.youtube.com/watch?v=c_KRIH0wnhE
Wiki page: https://www.chessprogramming.org/En_passant
-}


data EnPassantTarget = MkEPT File EPRank
  deriving (Show, Eq, Ord)

data EPRank = ER3 | ER6 -- Only the 3rd and 6th ranks can ever be en passant targets
  deriving (Show, Eq, Ord)

fromEPRank :: EPRank -> Rank
fromEPRank ER3 = R3
fromEPRank ER6 = R6

fromEnPassantTarget :: EnPassantTarget -> Pos
fromEnPassantTarget (MkEPT file epRank)
  = MkPos file (fromEPRank epRank)

toEPRank :: Rank -> Maybe EPRank
toEPRank R3 = Just ER3
toEPRank R6 = Just ER6
toEPRank _  = Nothing

toEnPassantTarget :: Pos -> Maybe EnPassantTarget
toEnPassantTarget (MkPos file rank)
  = case toEPRank rank of
      Just epRank -> Just (MkEPT file epRank)
      Nothing     -> Nothing

-- Functor version
toEnPassantTarget' :: Pos -> Maybe EnPassantTarget
toEnPassantTarget' (MkPos file rank) = fmap (MkEPT file) (toEPRank rank)

-- Monad version
toEnPassantTarget'' :: Pos -> Maybe EnPassantTarget
toEnPassantTarget'' (MkPos file rank) = do
  epRank <- toEPRank rank
  Just (MkEPT file epRank)


-- State
----------

data State = MkState
  { board :: Board
  , sideToMove :: Player
  , castlingAbility :: CastlingAbility
  , enPassantTarget :: Maybe EnPassantTarget
  , halfMoveClock :: Int  -- https://www.chessprogramming.org/Halfmove_Clock
  , fullMoveCounter :: Int -- Number of full moves. Incremented after black's move
  }
  deriving (Show, Eq, Ord)

modifyBoard :: (Board -> Board) -> State -> State
modifyBoard f state = state{ board = f (board state)}

modifySideToMove :: (Player -> Player) -> State -> State
modifySideToMove f state = state{ sideToMove = f (sideToMove state)}

modifyCastlingAbility :: (CastlingAbility -> CastlingAbility) -> State -> State
modifyCastlingAbility f state = state{ castlingAbility = f (castlingAbility state)}

modifyEnPassantTarget :: (Maybe EnPassantTarget -> Maybe EnPassantTarget) -> State -> State
modifyEnPassantTarget f state = state{ enPassantTarget = f (enPassantTarget state)}

modifyHalfMoveClock :: (Int -> Int) -> State -> State
modifyHalfMoveClock f state = state{ halfMoveClock = f (halfMoveClock state)}

modifyFullMoveCounter :: (Int -> Int) -> State -> State
modifyFullMoveCounter f state = state{ fullMoveCounter = f (fullMoveCounter state)}
