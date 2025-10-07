module Chess.Notation.LAN where

-- Long Algebraic Notation (LAN)
-- Used in the Universal Chess Interface (UCI) specification:
-- Examples:  e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion)
-- https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf#file-uci-protocol-specification-txt-L45

import Chess.Model

import Chess.Notation.Prelude

import Text.Yoda
import Data.Maybe (catMaybes)

data LAN = MkLAN Pos Pos (Maybe Piece)
  deriving (Show, Eq)

lanP :: Parser LAN
lanP = MkLAN <$> posP
             <*> posP
             <*> optional pieceP

-- Pretty printing --
---------------------

instance Pretty LAN where
  pretty (MkLAN pos pos' promotion)
    = concat $ catMaybes [Just (pretty pos), Just (pretty pos'), fmap pretty promotion]
