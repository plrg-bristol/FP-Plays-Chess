module Chess.View where

import Chess.Model
import Chess.Prelude
import Chess.Notation.Prelude
import qualified Chess.Notation.FEN as FEN

import Data.Maybe

import qualified Data.Map.Strict as Map
import Data.List (intersperse)


-- Displaying pieces --
-----------------------

data Theme = DarkTheme | LightTheme

-- https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode
-- https://symbolsdb.com/dot-symbol
viewPiece :: Theme -> SquareColour -> Maybe Piece -> Char
viewPiece theme squareColour maybePiece
  = case theme of
      LightTheme -> case maybePiece of
        Nothing -> case squareColour of
          DarkSquare  -> '•'
          LightSquare -> '◦'
        Just (MkPiece player pieceType) -> case player of
          White -> case pieceType of
            Pawn   -> '♙'
            Knight -> '♘'
            Bishop -> '♗'
            Rook   -> '♖'
            Queen  -> '♕'
            King   -> '♔'
          Black -> case pieceType of
            Pawn   -> '♟'
            Knight -> '♞'
            Bishop -> '♝'
            Rook   -> '♜'
            Queen  -> '♛'
            King   -> '♚'

      -- Annoyingly, dark themes in VSCode or the terminal invert the piece colours,
      -- so white pieces look black and black pieces look white.
      -- We can correct that by switching them around if the `Theme` is `Dark`.
      DarkTheme -> case maybePiece of
        Nothing -> case squareColour of
          DarkSquare  -> '◦'
          LightSquare -> '•'
        Just (MkPiece player pieceType) -> case player of
          White -> case pieceType of
            Pawn   -> '♟'
            Knight -> '♞'
            Bishop -> '♝'
            Rook   -> '♜'
            Queen  -> '♛'
            King   -> '♚'
          Black -> case pieceType of
            Pawn   -> '♙'
            Knight -> '♘'
            Bishop -> '♗'
            Rook   -> '♖'
            Queen  -> '♕'
            King   -> '♔'

data SquareColour = DarkSquare | LightSquare
  deriving (Show, Eq, Ord, Enum)

-- >>> toEnum 0 :: SquareColour
-- DarkSquare
-- >>> toEnum 1 :: SquareColour
-- LightSquare

positionColour :: Pos -> SquareColour
positionColour (MkPos file rank)
  -- Square colour corresponds to the parity of `rank + file`, where a=1, b=2 etc.
  = toEnum ((fromEnum file + fromEnum rank) `mod` 2)

-- >>> map positionColour [MkPos A R1,MkPos A R2,MkPos A R3]
-- [DarkSquare,LightSquare,DarkSquare]
-- >>> map positionColour [MkPos A R1,MkPos B R1,MkPos C R1]
-- [DarkSquare,LightSquare,DarkSquare]
-- >>> map positionColour [MkPos A R1,MkPos B R2,MkPos C R3]
-- [DarkSquare,DarkSquare,DarkSquare]

-- Displaying the board --
--------------------------

viewBoard :: Theme -> Board -> String
viewBoard theme b
  = boardPositionGrid
    |> map (map (\pos -> viewPiece theme (positionColour pos) (b Map.!? pos)))
    |> zipWith (++) (map show [8 :: Int, 7 .. 1]) -- Add Rank to each row
    |> (++ [' ' : ['a' .. 'h']])                  -- Add File to each column
    |> map (intersperse ' ')                      -- Add spaces between grid
    |> unlines

boardPositionGrid :: [[Pos]]
boardPositionGrid = [[MkPos file rank | file <- [A .. H]] | rank <- [R8, R7 .. R1]]

-- >>> boardPositionGrid
-- [[MkPos A R8,MkPos B R8,MkPos C R8,MkPos D R8,MkPos E R8,MkPos F R8,MkPos G R8,MkPos H R8],[MkPos A R7,MkPos B R7,MkPos C R7,MkPos D R7,MkPos E R7,MkPos F R7,MkPos G R7,MkPos H R7],[MkPos A R6,MkPos B R6,MkPos C R6,MkPos D R6,MkPos E R6,MkPos F R6,MkPos G R6,MkPos H R6],[MkPos A R5,MkPos B R5,MkPos C R5,MkPos D R5,MkPos E R5,MkPos F R5,MkPos G R5,MkPos H R5],[MkPos A R4,MkPos B R4,MkPos C R4,MkPos D R4,MkPos E R4,MkPos F R4,MkPos G R4,MkPos H R4],[MkPos A R3,MkPos B R3,MkPos C R3,MkPos D R3,MkPos E R3,MkPos F R3,MkPos G R3,MkPos H R3],[MkPos A R2,MkPos B R2,MkPos C R2,MkPos D R2,MkPos E R2,MkPos F R2,MkPos G R2,MkPos H R2],[MkPos A R1,MkPos B R1,MkPos C R1,MkPos D R1,MkPos E R1,MkPos F R1,MkPos G R1,MkPos H R1]]

printBoard :: Theme -> Board -> IO ()
printBoard theme b = putStrLn (viewBoard theme b)

printBoardFromFEN :: Theme -> String -> IO ()
printBoardFromFEN theme
  = FEN.parseBoard
 .> fmap (viewBoard theme)
 .> fromMaybe "Error: viewBoard returned `Nothing`"
 .> putStrLn

-- In GHCI:
-- > printBoardFromFEN DarkTheme "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR"
-- Or if using a light mode theme:
-- > printBoardFromFEN LightTheme "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR"

-- Displaying the full state --
-------------------------------

viewState :: Theme -> State -> String
viewState theme state = unlines
  [ viewBoard theme (board state)
  , "Castling: " ++ pretty (castlingAbility state)
  , "En passant: " ++ pretty (FEN.toEnPassantFEN $ enPassantTarget state)
  , "Half move clock: " ++ show (halfMoveClock state)
  , "Full move counter: " ++ show (fullMoveCounter state)
  , ""
  , show (sideToMove state) ++ " to play"
  ]

printState :: Theme -> State -> IO ()
printState theme = viewState theme .> putStrLn

printStateFromFEN :: Theme -> String -> IO ()
printStateFromFEN theme
  = FEN.stateFromFEN
 .> fmap (viewState theme)
 .> fromMaybe "Error: Invalid FEN"
 .> putStrLn

-- In GHCI:
-- > printStateFromFEN DarkTheme "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
-- Or if using a light mode theme:
-- > printStateFromFEN LightTheme "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

-- TODO: GUI?