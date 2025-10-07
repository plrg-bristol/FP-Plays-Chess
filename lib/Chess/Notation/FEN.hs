-- The OverloadedRecordDot extension lets us use OOP-like syntax for records
{-# language OverloadedRecordDot #-}
-- The DuplicateRecordFields extension lets us use the same field names for different record types,
-- even when both are in scope
{-# language DuplicateRecordFields #-}
module Chess.Notation.FEN where

-- Parsing and printing FEN notation

import Chess.Model as Model
import Chess.Prelude

import Chess.Notation.Prelude

import Text.Yoda
import Data.List (delete, intersperse)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Bifunctor (second)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Directly modeling FEN notation as a data type
-- Adapted from https://www.chessprogramming.org/Forsyth-Edwards_Notation#FEN_Syntax

-- Piece Placement
--------------------

{-
<Piece Placement> ::= <rank8>'/'<rank7>'/'<rank6>'/'<rank5>'/'<rank4>'/'<rank3>'/'<rank2>'/'<rank1>
<ranki>           ::= (<digit17> | <piece>)+
<piece>           ::= <white Piece> | <black Piece>
<digit17>         ::= '1' | '2' | '3' | '4' | '5' | '6' | '7'
<white Piece>     ::= 'P' | 'N' | 'B' | 'R' | 'Q' | 'K'
<black Piece>     ::= 'p' | 'n' | 'b' | 'r' | 'q' | 'k'
-}

data PiecePlacement
  = MkPiecePlacement
      RankPlacement
      RankPlacement
      RankPlacement
      RankPlacement
      RankPlacement
      RankPlacement
      RankPlacement
      RankPlacement
  deriving (Show, Eq, Ord)

listRankPlacements :: PiecePlacement -> [RankPlacement]
listRankPlacements (MkPiecePlacement r8 r7 r6 r5 r4 r3 r2 r1)
  = [r8, r7, r6, r5, r4, r3, r2, r1]

newtype RankPlacement = MkRP {unRP :: [PieceOrSpaces]}
  deriving (Show, Eq, Ord)

data PieceOrSpaces
  = RPiece Piece
  | RSpace Int
  deriving (Show, Eq, Ord)


pieceOrSpacesP :: Parser PieceOrSpaces
pieceOrSpacesP = RPiece <$> pieceP
             <|> RSpace <$> digitP

rankPlacementP :: Parser RankPlacement
rankPlacementP = MkRP <$> some pieceOrSpacesP

-- >>> parseMaybe rankPlacementP "rnbqkbnr"
-- Just (MkRP [RPiece (MkPiece Black Rook),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Queen),RPiece (MkPiece Black King),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Rook)])
-- >>> parseMaybe rankPlacementP "4P3"
-- Just (MkRP [RSpace 4,RPiece (MkPiece White Pawn),RSpace 3])

-- Invalid rank not caught, need monadic parsing:
-- >>> parseMaybe rankPlacementP "4PPP3"
-- Just (MkRP [RSpace 4,RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RSpace 3])

{-
Monad extension
~~~~~~~~~~~~~~~
-}

-- If the value returned by the parser doesn't meet some
-- validation predicate `p`, the parser fails.
-- This requires Monad because it changes the structure (parsing success or failure)
-- based on the value returned by the parser
validate :: (a -> Bool) -> Parser a -> Parser a
validate p parser = do
  x <- parser
  if p x
    then pure x
    else empty

has8squares :: RankPlacement -> Bool
has8squares xs
  = (sum $ map numberOfSquares $ unRP xs) == 8

numberOfSquares :: PieceOrSpaces -> Int
numberOfSquares (RSpace n) = n
numberOfSquares _ = 1

rankPlacementValidatedP :: Parser RankPlacement
rankPlacementValidatedP = validate has8squares rankPlacementP

-- Correctly prevents invalid rank:
-- >>> parseMaybe rankPlacementValidatedP "4PPP3"
-- Nothing



piecePlacementP :: Parser PiecePlacement
piecePlacementP
  = MkPiecePlacement <$> rankPlacementValidatedP <* char '/'
                     <*> rankPlacementValidatedP <* char '/'
                     <*> rankPlacementValidatedP <* char '/'
                     <*> rankPlacementValidatedP <* char '/'
                     <*> rankPlacementValidatedP <* char '/'
                     <*> rankPlacementValidatedP <* char '/'
                     <*> rankPlacementValidatedP <* char '/'
                     <*> rankPlacementValidatedP

-- >>> parseMaybe piecePlacementP "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R"
-- Just (MkPiecePlacement (MkRP [RPiece (MkPiece Black Rook),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Queen),RPiece (MkPiece Black King),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Rook)]) (MkRP [RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RSpace 1,RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn)]) (MkRP [RSpace 8]) (MkRP [RSpace 2,RPiece (MkPiece Black Pawn),RSpace 5]) (MkRP [RSpace 4,RPiece (MkPiece White Pawn),RSpace 3]) (MkRP [RSpace 5,RPiece (MkPiece White Knight),RSpace 2]) (MkRP [RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RSpace 1,RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn)]) (MkRP [RPiece (MkPiece White Rook),RPiece (MkPiece White Knight),RPiece (MkPiece White Bishop),RPiece (MkPiece White Queen),RPiece (MkPiece White King),RPiece (MkPiece White Bishop),RSpace 1,RPiece (MkPiece White Rook)]))

-- Side to move
------------------

sideToMoveP :: Parser Player
sideToMoveP = White <$ char 'w'
          <|> Black <$ char 'b'

-- Castling ability
---------------------

castlingAbilityP :: Parser CastlingAbility
castlingAbilityP
  =  MkCA False False False False <$ char '-'
 <|> MkCA <$> flag 'K' <*> flag 'Q' <*> flag 'k' <*> flag 'q'

flag :: Char -> Parser Bool
flag c = True <$ char c
    --  <|> pure False -- Simple version which parses correct strings fine,
                       -- but fails to prevent incorrect strings like "BKQkq" or "KQBkq"

    -- Extension which correctly fails for incorrect strings 
     <|> False <$ (peekSatisfy (`elem` (delete c "KQkq")) -- important to delete the character being checked for in the `True` case, otherwise both `True` (+ `c` consumed) or `False` (+ `c` not consumed) could be picked when a flag is present
                   <|> eof
                   <|> (() <$ char ' '))

-- Return the next character without consuming it
peek :: Parser Char
peek = Parser (\cs -> case cs of
  []    -> []
  (c:_) -> [(c, cs)])

peekSatisfy :: (Char -> Bool) -> Parser ()
peekSatisfy p = Parser (\cs -> case cs of
  []    -> []
  (c:_) -> if p c
           then [((), cs)]
           else [])

-- Alternative definitions using monads and `look`
peek' :: Parser Char
peek' = do
  cs <- look
  case cs of
    []    -> empty
    (c:_) -> pure c

peekSatisfy' :: (Char -> Bool) -> Parser ()
peekSatisfy' p = () <$ validate p peek'

-- >>> parseMaybe castlingAbilityP "-"
-- Just (MkCA {whiteKingside = False, whiteQueenside = False, blackKingside = False, blackQueenside = False})
-- >>> parseMaybe castlingAbilityP "KQkq"
-- Just (MkCA {whiteKingside = True, whiteQueenside = True, blackKingside = True, blackQueenside = True})
-- >>> parseMaybe castlingAbilityP "Kkq"
-- Just (MkCA {whiteKingside = True, whiteQueenside = False, blackKingside = True, blackQueenside = True})
-- >>> parseMaybe castlingAbilityP "BKQkq"
-- Nothing
-- >>> parse castlingAbilityP "KQBkq"
-- []

-- En Passant target square
-----------------------------

data EnPassantFEN = Dash | EPTarget EnPassantTarget
  deriving (Show, Eq, Ord)

epRankP :: Parser EPRank
epRankP = ER3 <$ char '3'
      <|> ER6 <$ char '6'

enPassantFENP :: Parser EnPassantFEN
enPassantFENP
  =  EPTarget <$> (MkEPT <$> fileP <*> epRankP)
 <|> Dash <$ char '-'

-- >>> parseMaybe enPassantFENP "e6"
-- Just (EPTarget (MkEPT E ER6))
-- >>> parseMaybe enPassantFENP "d3"
-- Just (EPTarget (MkEPT D ER3))
-- >>> parseMaybe enPassantFENP "n3"
-- Nothing
-- >>> parseMaybe enPassantFENP "a5"
-- Nothing

foldEnPassantFEN :: a -> (EnPassantTarget -> a) -> EnPassantFEN -> a
foldEnPassantFEN dash epTarget epf = case epf of
  Dash -> dash
  EPTarget t -> epTarget t

toEnPassantFEN :: Maybe EnPassantTarget -> EnPassantFEN
toEnPassantFEN = maybe Dash EPTarget

fromEnPassantFEN :: EnPassantFEN -> Maybe EnPassantTarget
fromEnPassantFEN = foldEnPassantFEN Nothing Just

---------
-- FEN --
---------

{-
<FEN> ::=  <Piece Placement>
       ' ' <Side to move>
       ' ' <Castling ability>
       ' ' <En passant target square>
       ' ' <Halfmove clock>
       ' ' <Fullmove counter>
-}

data FEN = MkFEN
  { piecePlacement  :: PiecePlacement
  , sideToMove      :: Player
  , castlingAbility :: CastlingAbility
  , enPassantFEN    :: EnPassantFEN
  , halfMoveClock   :: Int
  , fullMoveCounter :: Int
  }
  deriving (Show, Eq, Ord)

fenP :: Parser FEN
fenP = MkFEN <$> piecePlacementP  <* char ' '
             <*> sideToMoveP      <* char ' '
             <*> castlingAbilityP <* char ' '
             <*> enPassantFENP    <* char ' '
             <*> intP             <* char ' '
             <*> intP

-- >>> parseMaybe fenP "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
-- Just (MkFEN {piecePlacement = MkPiecePlacement (MkRP [RPiece (MkPiece Black Rook),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Queen),RPiece (MkPiece Black King),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Rook)]) (MkRP [RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RSpace 1,RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn)]) (MkRP [RSpace 8]) (MkRP [RSpace 2,RPiece (MkPiece Black Pawn),RSpace 5]) (MkRP [RSpace 4,RPiece (MkPiece White Pawn),RSpace 3]) (MkRP [RSpace 8]) (MkRP [RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RSpace 1,RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn)]) (MkRP [RPiece (MkPiece White Rook),RPiece (MkPiece White Knight),RPiece (MkPiece White Bishop),RPiece (MkPiece White Queen),RPiece (MkPiece White King),RPiece (MkPiece White Bishop),RPiece (MkPiece White Knight),RPiece (MkPiece White Rook)]), sideToMove = White, castlingAbility = MkCA {whiteKingside = True, whiteQueenside = True, blackKingside = True, blackQueenside = True}, enPassantTarget = Just (MkEPT C ER6), halfMoveClock = 0, fullMoveCounter = 2})

-- Converting FEN to State and back again
-------------------------------------------

toState :: FEN -> State
toState fen
  = MkState
      { board = toBoard (piecePlacement fen)
        -- We used the same record field names for both `FEN` and `State`,
        -- which would normally cause an 'ambiguous occurence' error, but if we enable
        -- the language extensions `OverloadedRecordDot` and `DuplicateRecordFields`
        -- we can use OOP-like syntax to use them unambiguously
      , sideToMove      = fen.sideToMove
      , castlingAbility = fen.castlingAbility
      , enPassantTarget = fromEnPassantFEN fen.enPassantFEN
      , halfMoveClock   = fen.halfMoveClock
      , fullMoveCounter = fen.fullMoveCounter
      }

-- Good data transformation example:

toBoard :: PiecePlacement -> Board
toBoard
  = listRankPlacements                     -- [RankPlacement]
 .> map expandRank                         -- [[Maybe Piece]]
 .> zip2DKey MkPos [A .. H] [R8, R7 .. R1] -- [[(Pos, Maybe Piece)]]
 .> concat                                 -- [(Pos, Maybe Piece)]
 .> filter (snd .> isJust)                 -- [(Pos, Maybe Piece)]
 .> map (\(k, mp) -> (k, fromJust mp))     -- [(Pos, Piece)]
--  |> map (second fromJust)-- Alternative
 .> Map.fromList                             -- Map Pos Piece a.k.a. Board

expandRank :: RankPlacement -> [Maybe Piece]
expandRank (MkRP []) = []
expandRank (MkRP (x:xs)) = case x of
  RSpace n -> replicate n Nothing ++ expandRank (MkRP xs)
  RPiece p -> Just p : expandRank (MkRP xs)

zip2DKey :: (a -> b -> key) -> [a] -> [b] -> [[c]] -> [[(key, c)]]
zip2DKey keyFunc xs ys zs
  = map (zip xs) zs
 |> zipWith (\y -> map (\(x,z) -> (keyFunc x y, z))) ys

-- >>> zip2DKey (,) ['a'..'c'] [1..3] [[1,2,3], [4,5,6], [7,8,9]]
-- [[(('a',1),1),(('b',1),2),(('c',1),3)],[(('a',2),4),(('b',2),5),(('c',2),6)],[(('a',3),7),(('b',3),8),(('c',3),9)]]

stateFromFEN :: String -> Maybe State
stateFromFEN str = toState <$> parseMaybe fenP str

-- >>> stateFromFEN "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
-- Just (MkState {board = fromList [(MkPos A R1,MkPiece White Rook),(MkPos A R2,MkPiece White Pawn),(MkPos A R7,MkPiece Black Pawn),(MkPos A R8,MkPiece Black Rook),(MkPos B R1,MkPiece White Knight),(MkPos B R2,MkPiece White Pawn),(MkPos B R7,MkPiece Black Pawn),(MkPos B R8,MkPiece Black Knight),(MkPos C R1,MkPiece White Bishop),(MkPos C R2,MkPiece White Pawn),(MkPos C R5,MkPiece Black Pawn),(MkPos C R8,MkPiece Black Bishop),(MkPos D R1,MkPiece White Queen),(MkPos D R2,MkPiece White Pawn),(MkPos D R7,MkPiece Black Pawn),(MkPos D R8,MkPiece Black Queen),(MkPos E R1,MkPiece White King),(MkPos E R4,MkPiece White Pawn),(MkPos E R7,MkPiece Black Pawn),(MkPos E R8,MkPiece Black King),(MkPos F R1,MkPiece White Bishop),(MkPos F R2,MkPiece White Pawn),(MkPos F R7,MkPiece Black Pawn),(MkPos F R8,MkPiece Black Bishop),(MkPos G R1,MkPiece White Knight),(MkPos G R2,MkPiece White Pawn),(MkPos G R7,MkPiece Black Pawn),(MkPos G R8,MkPiece Black Knight),(MkPos H R1,MkPiece White Rook),(MkPos H R2,MkPiece White Pawn),(MkPos H R7,MkPiece Black Pawn),(MkPos H R8,MkPiece Black Rook)], sideToMove = White, castlingAbility = MkCA {whiteKingside = True, whiteQueenside = True, blackKingside = True, blackQueenside = True}, enPassantTarget = Just (MkEPT C ER6), halfMoveClock = 0, fullMoveCounter = 2})


deflateRank :: [Maybe Piece] -> RankPlacement
deflateRank = MkRP . go 0
  where
    go n (Nothing : xs) = go (n + 1) xs
    go n (Just p  : xs)
      | n >= 1    = RSpace n : RPiece p : go 0 xs
      | otherwise =            RPiece p : go 0 xs
    go n []
      | n >= 1    = [RSpace n]
      | otherwise = []

-- >>> deflateRank [Nothing, Nothing, Nothing, Just (MkPiece White Pawn), Nothing, Nothing,Just (MkPiece White Pawn),Just (MkPiece White Pawn)]
-- [RSpace 3,RPiece (MkPiece White Pawn),RSpace 2,RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn)]

fromBoard :: Map Pos Piece -> PiecePlacement
fromBoard b
  = [ [ b Map.!? MkPos file rank
      | file <- [A .. H]
      ]
    | rank <- [R8, R7 .. R1]
    ]                          -- [[Maybe Piece]]
    |> map deflateRank         -- [RankPlacement] a.k.a. [[PieceOrSpaces]]
    |> \[r8, r7, r6, r5, r4, r3, r2, r1] -- Partial function, but we know the list will always have 8 elements because we created it with the list comprehension
        -> MkPiecePlacement r8 r7 r6 r5 r4 r3 r2 r1

-- >>> fromBoard $ Map.fromList [(MkPos A R1,MkPiece White Rook),(MkPos A R2,MkPiece White Pawn),(MkPos A R7,MkPiece Black Pawn),(MkPos A R8,MkPiece Black Rook),(MkPos B R1,MkPiece White Knight),(MkPos B R2,MkPiece White Pawn),(MkPos B R7,MkPiece Black Pawn),(MkPos B R8,MkPiece Black Knight),(MkPos C R1,MkPiece White Bishop),(MkPos C R2,MkPiece White Pawn),(MkPos C R5,MkPiece Black Pawn),(MkPos C R8,MkPiece Black Bishop),(MkPos D R1,MkPiece White Queen),(MkPos D R2,MkPiece White Pawn),(MkPos D R7,MkPiece Black Pawn),(MkPos D R8,MkPiece Black Queen),(MkPos E R1,MkPiece White King),(MkPos E R4,MkPiece White Pawn),(MkPos E R7,MkPiece Black Pawn),(MkPos E R8,MkPiece Black King),(MkPos F R1,MkPiece White Bishop),(MkPos F R2,MkPiece White Pawn),(MkPos F R7,MkPiece Black Pawn),(MkPos F R8,MkPiece Black Bishop),(MkPos G R1,MkPiece White Knight),(MkPos G R2,MkPiece White Pawn),(MkPos G R7,MkPiece Black Pawn),(MkPos G R8,MkPiece Black Knight),(MkPos H R1,MkPiece White Rook),(MkPos H R2,MkPiece White Pawn),(MkPos H R7,MkPiece Black Pawn),(MkPos H R8,MkPiece Black Rook)]
-- MkPiecePlacement [RPiece (MkPiece Black Rook),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Queen),RPiece (MkPiece Black King),RPiece (MkPiece Black Bishop),RPiece (MkPiece Black Knight),RPiece (MkPiece Black Rook)] [RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RSpace 1,RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn),RPiece (MkPiece Black Pawn)] [RSpace 8] [RSpace 2,RPiece (MkPiece Black Pawn),RSpace 5] [RSpace 4,RPiece (MkPiece White Pawn),RSpace 3] [RSpace 8] [RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RSpace 1,RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn),RPiece (MkPiece White Pawn)] [RPiece (MkPiece White Rook),RPiece (MkPiece White Knight),RPiece (MkPiece White Bishop),RPiece (MkPiece White Queen),RPiece (MkPiece White King),RPiece (MkPiece White Bishop),RPiece (MkPiece White Knight),RPiece (MkPiece White Rook)]

fromState :: State -> FEN
fromState state
  = MkFEN
      { piecePlacement = fromBoard (board state)
      , sideToMove      = Model.sideToMove      state
      , castlingAbility = Model.castlingAbility state
      , enPassantFEN    = toEnPassantFEN state.enPassantTarget
      , halfMoveClock   = Model.halfMoveClock   state
      , fullMoveCounter = Model.fullMoveCounter state
      }

-- Round-trip test:

-- >>> fmap (pretty . fromState) (stateFromFEN "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2")
-- Just "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

parseBoard :: String -> Maybe Board
parseBoard str = toBoard <$> parseMaybe piecePlacementP str

-- Printing
-------------

instance Pretty FEN where
  pretty (MkFEN piecePlacement sideToMove castlingAbility enPassantTarget halfMoveClock  fullMoveCounter)
    = concat 
        [ pretty piecePlacement
        , " "
        , pretty sideToMove
        , " "
        , pretty castlingAbility
        , " "
        , pretty enPassantTarget
        , " "
        , show halfMoveClock
        , " "
        , show fullMoveCounter
        ]

instance Pretty PiecePlacement where
  pretty
    = listRankPlacements
   .> map pretty
   .> intersperse "/"
   .> concat

instance Pretty RankPlacement where
  pretty = unRP .> concatMap pretty

instance Pretty PieceOrSpaces where
  pretty (RPiece p) = pretty p
  pretty (RSpace n) = show n


instance Pretty EnPassantFEN where
  pretty Dash = "-"
  pretty (EPTarget (MkEPT file epRank)) = pretty file ++ pretty epRank
