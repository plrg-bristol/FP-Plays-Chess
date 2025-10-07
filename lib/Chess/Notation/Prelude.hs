{-# language FlexibleInstances #-}
module Chess.Notation.Prelude where

import Chess.Model

import Text.Yoda

import Data.Char (isDigit)

digitP :: Parser Int
digitP = (\c -> read [c]) <$> satisfy isDigit

-- >>> parseMaybe digitP "4"
-- Just 4
-- >>> parseMaybe digitP "8"
-- Just 8

intP :: Parser Int
intP = read <$> some (satisfy isDigit)

-- >>> parseMaybe intP "432"
-- Just 432
-- >>> parseMaybe intP "841"
-- Just 841

fileP :: Parser File
fileP = choice $ zipWith (\file c -> file <$ char c) [A .. H] ['a' .. 'h']

-- >>> parseMaybe fileP "g"
-- Just G

rankP :: Parser Rank
rankP = choice $ zipWith (\rank c -> rank <$ char c) [R1 .. R8] ['1' .. '8']

posP :: Parser Pos
posP = MkPos <$> fileP <*> rankP

pieceP :: Parser Piece
pieceP = MkPiece White Pawn   <$ char 'P'
     <|> MkPiece White Knight <$ char 'N'
     <|> MkPiece White Bishop <$ char 'B'
     <|> MkPiece White Rook   <$ char 'R'
     <|> MkPiece White Queen  <$ char 'Q'
     <|> MkPiece White King   <$ char 'K'
     <|> MkPiece Black Pawn   <$ char 'p'
     <|> MkPiece Black Knight <$ char 'n'
     <|> MkPiece Black Bishop <$ char 'b'
     <|> MkPiece Black Rook   <$ char 'r'
     <|> MkPiece Black Queen  <$ char 'q'
     <|> MkPiece Black King   <$ char 'k'

-- >>> parseMaybe pieceP "q"
-- Just (MkPiece Black Queen)
-- >>> parseMaybe pieceP "R"
-- Just (MkPiece White Rook)
-- >>> parseMaybe pieceP "L"
-- Nothing

-- Pretty printing --
---------------------

class Pretty a where
  pretty :: a -> String

instance Pretty Piece where
  pretty p = case p of
    MkPiece White Pawn   -> "P"
    MkPiece White Knight -> "N"
    MkPiece White Bishop -> "B"
    MkPiece White Rook   -> "R"
    MkPiece White Queen  -> "Q"
    MkPiece White King   -> "K"
    MkPiece Black Pawn   -> "p"
    MkPiece Black Knight -> "n"
    MkPiece Black Bishop -> "b"
    MkPiece Black Rook   -> "r"
    MkPiece Black Queen  -> "q"
    MkPiece Black King   -> "k"

instance Pretty Player where
  pretty White = "w"
  pretty Black = "b"

instance Pretty CastlingAbility where
  pretty (MkCA False False False False) = "-"
  pretty (MkCA wK wQ bK bQ)
    = concat [foo wK "K", foo wQ "Q", foo bK "k", foo bQ "q"]
    where
      foo b c = if b then c else ""


instance Pretty File where
  pretty f = case f of
    A -> "a"
    B -> "b"
    C -> "c"
    D -> "d"
    E -> "e"
    F -> "f"
    G -> "g"
    H -> "h"

instance Pretty EPRank where
  pretty ER3 = "3"
  pretty ER6 = "6"

instance Pretty Rank where
  pretty rank = case rank of
    R1 -> "1"
    R2 -> "2"
    R3 -> "3"
    R4 -> "4"
    R5 -> "5"
    R6 -> "6"
    R7 -> "7"
    R8 -> "8"

instance Pretty Pos where
  pretty (MkPos file rank) = pretty file ++ pretty rank
