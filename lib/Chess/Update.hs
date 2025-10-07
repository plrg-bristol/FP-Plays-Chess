-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html
-- 'When OverloadedRecordDot is enabled one can write a.b to mean the b field of the a record expression.'
{-# language OverloadedRecordDot #-}

module Chess.Update where

import Chess.Prelude
import Chess.Model
import Chess.View

import Chess.Notation.Prelude
import qualified Chess.Notation.FEN as FEN
import Chess.Notation.LAN
import Text.Yoda

import qualified Chess.Engine.Stockfish as Stockfish

import Data.Maybe ( catMaybes, fromJust )
import qualified Data.Map.Strict as Map
import Control.Monad (guard)
import Control.Concurrent (threadDelay)
import Data.List ( nub)
import Data.Either ( isRight )

-- Piece moves --
-----------------

-- https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Disambiguating_moves
data Move = MkMove Piece Pos CaptureIndicator Pos
  deriving (Show, Eq)

data CaptureIndicator = NoCapture | Capture
  deriving (Show, Eq, Ord, Enum)

-- isasdCapture :: CaptureIndicator -> Bool
-- isasdCapture Capture   = True
-- isasdCapture NoCapture = False

makeMove :: Move -> State -> Either String State
makeMove move@(MkMove piece@(MkPiece player _) pos captureIndicator pos') state = do
  validatePieceAtPos piece pos (board state)

  validateIsCapture piece captureIndicator pos' (board state)

  guardWithError (player == state.sideToMove)
                 ("Error: " ++ show state.sideToMove ++ " can't move "
                            ++ show player ++ "'s piece")

  -- TODO: validate piece can move to pos'
  guardWithError (move `elem` psuedoLegalMovesAtPos state.board pos)
                 ("Error: " ++ pretty move ++ " is not a valid move")

  let board' = board state
               |> Map.delete pos
               |> Map.insert pos' piece

      checkedPlayers = playersInCheck board'

  -- Check checks
  guardWithError (player `notElem` checkedPlayers)
                 ("Error: You cannot make a move that would leave you in check")

  let opponentInCheck = nextPlayer player `elem` checkedPlayers

  -- TODO: add current checks to state


  Right (state { board = board'
               , sideToMove = nextPlayer state.sideToMove
               , halfMoveClock = stepHalfMoveClock piece captureIndicator state.halfMoveClock
               , fullMoveCounter = stepFullMoveCounter piece state.fullMoveCounter
               -- TODO: update castlingAbility and enPassantTarget
               })

makeMoveLAN :: LAN -> State -> Either String State
makeMoveLAN lan state = do
  move <- fromLAN state.board lan
  makeMove move state

fromLAN :: Board -> LAN -> Either String Move
fromLAN b (MkLAN pos pos' promotion) = do
  piece@(MkPiece player _) <- getPieceAtPos pos b
  captureIndicator <- getCaptureIndicator player pos' b
  Right (MkMove piece pos captureIndicator pos')

  -- do
  -- piece@(MkPiece player _) <- getPieceAtPos pos (board state)

  -- captureIndicator <- getCaptureIndicator player pos' (board state)
  -- -- validateIsCapture piece captureIndicator pos' (board state)

  -- guardWithError (player == state.sideToMove)
  --                ("Error: " ++ show state.sideToMove ++ " can't move "
  --                           ++ show player ++ "'s piece")

  -- let move = MkMove piece pos captureIndicator pos'

  -- -- TODO: validate piece can move to pos'
  -- guardWithError (move `elem` psuedoLegalMovesAtPos state.board pos)
  --                ("Error: " ++ pretty move ++ " is not a valid move")

  -- let board' = board state
  --              |> Map.delete pos
  --              |> Map.insert pos' piece

  -- Right (state { board = board'
  --              , sideToMove = nextPlayer state.sideToMove
  --              , halfMoveClock = stepHalfMoveClock piece captureIndicator state.halfMoveClock
  --              , fullMoveCounter = stepFullMoveCounter piece state.fullMoveCounter
  --              -- TODO: update castlingAbility and enPassantTarget
  --              })

getPieceAtPos :: Pos -> Board -> Either String Piece
getPieceAtPos pos b
  = case b Map.!? pos of
      Nothing     -> Left ("Error: No piece at position " ++ pretty pos)
      Just piece -> Right piece

stepFullMoveCounter :: Piece -> Int -> Int
stepFullMoveCounter (MkPiece Black _) n = n + 1
stepFullMoveCounter _ n = n

-- https://www.chessprogramming.org/Halfmove_Clock
stepHalfMoveClock :: Piece -> CaptureIndicator -> Int -> Int
stepHalfMoveClock (MkPiece _ pieceType) captureIndicator n
  | captureIndicator == Capture = 0
  | pieceType == Pawn           = 0
  | otherwise                   = n + 1

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White

-- 'If it's true, let it through', same as filter
guardWithError :: Bool -> a -> Either a ()
guardWithError b errorMessage = if b then Right () else Left errorMessage
-- guardWithError b errorMessage = if b then Left errorMessage else Right ()

validatePieceAtPos :: Piece -> Pos -> Board -> Either String ()
validatePieceAtPos piece pos b
  = case b Map.!? pos of
      Nothing -> Left ("Error: No piece at position " ++ pretty pos)
      Just piece' -> guardWithError (piece == piece')
                                    ("Error: Pieces don't match: "
                                          ++ pretty piece ++ " /= " ++ pretty piece')
      -- Just piece' -> if piece == piece'
      --                then Right ()
      --                else Left ("Error: Pieces don't match: "
      --                             ++ pretty piece ++ " /= " ++ pretty piece')

validateIsCapture :: Piece -> CaptureIndicator -> Pos -> Board -> Either String ()
validateIsCapture _ NoCapture pos b
  = guardWithError (Map.notMember pos b)
                   ("Error: NoCapture specified, but there is a piece at " ++ pretty pos)
validateIsCapture (MkPiece player _) Capture pos b
  = case b Map.!? pos of
      Nothing -> Left ("Error: Capture specified, but there is no piece at " ++ pretty pos)
      Just (MkPiece player' pieceType) -> do
        guardWithError (player /= player') "Error: You can't capture your own piece"
        guardWithError (pieceType /= King) "Error: You can't capture the king"

getCaptureIndicator :: Player -> Pos -> Board -> Either String CaptureIndicator
getCaptureIndicator player pos b
  = case b Map.!? pos of
      Nothing -> Right NoCapture
      Just (MkPiece player' pieceType) -> do
        guardWithError (player /= player') "Error: You can't capture your own piece"
        guardWithError (pieceType /= King) "Error: You can't capture the king"
        Right Capture

-- Fixed offset moves --
------------------------

posToCoord :: Pos -> (Int, Int)
posToCoord (MkPos file rank) = (fromEnum file, fromEnum rank)

coordToPos :: (Int, Int) -> Maybe Pos
coordToPos (x,y)
  | 0 <= x && x < 8 &&
    0 <= y && y < 8    = Just (MkPos (toEnum x) (toEnum y))
  | otherwise          = Nothing

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x,y) (x',y') = (x + x', y + y')

offsetPos :: Pos -> (Int, Int) -> Maybe Pos
offsetPos pos dxdy
  = posToCoord pos
    |> addTuples dxdy
    |> coordToPos

posOffsets :: Pos -> [(Int, Int)] -> [Pos]
posOffsets pos
  = map (offsetPos pos)
 .> catMaybes

fixedMovesFromOffsets :: Board -> Piece -> Pos -> [(Int, Int)] -> [Move]
fixedMovesFromOffsets b piece@(MkPiece player _) pos
  = posOffsets pos
 .> map (\pos' -> case b Map.!? pos' of
                    Nothing -> Just (MkMove piece pos NoCapture pos')
                    Just (MkPiece player' _)
                      -> if player /= player'
                         then Just (MkMove piece pos Capture pos')
                         else Nothing
        )
 .> catMaybes

knightOffsets :: [(Int, Int)]
knightOffsets = [(1,2), (2,1), (2, -1), (1,-2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

kingOffsets :: [(Int, Int)]
kingOffsets = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
-- kingOffsets = [(dx,dy) | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0)]


egBoard :: Board
egBoard = fromJust (FEN.parseBoard "rnbqkbnr/pp1pp2p/8/5pP1/3pP3/2P5/PP3PP1/RNBQKBNR")

-- ghci> printBoard DarkTheme egBoard

-- >>> map pretty $ fixedMovesFromOffsets egBoard (MkPiece White Knight) (MkPos G R1) knightOffsets
-- ["Ng1h3","Ng1e2","Ng1f3"]

-- Sliding piece moves  --
--------------------------

data Direction = North | NE | East | SE | South | SW | West | NW
  deriving (Show, Eq, Ord, Enum)

dirToOffset :: Direction -> (Int, Int)
dirToOffset dir = case dir of
  North  -> (0  , 1)
  NE     -> (1  , 1)
  East   -> (1  , 0)
  SE     -> (1  , -1)
  South  -> (0  , -1)
  SW     -> (-1 , -1)
  West   -> (-1 , 0)
  NW     -> (-1 , 1)

stepPosInDirection :: Pos ->  Direction -> Maybe Pos
stepPosInDirection pos d = offsetPos pos (dirToOffset d)

slidingMovesInDirection :: Board -> Piece -> Pos -> Direction -> [Move]
slidingMovesInDirection b piece@(MkPiece player _) startPos d = go startPos
  where
    go pos = case stepPosInDirection pos d of
      Nothing   -> []
      Just pos' -> case b Map.!? pos' of
        Just (MkPiece player' _)
          -> if player == player'
             then []
             else [MkMove piece startPos Capture pos']
        Nothing -> MkMove piece startPos NoCapture pos' : go pos'

-- >>> map pretty $ slidingMovesInDirection egBoard (MkPiece White Queen) (MkPos D R1) North
-- ["Qd1d2","Qd1d3","Qd1xd4"]


-- Pawn Moves --
----------------



pawnMoves :: Board -> Player -> Pos -> [Move]
pawnMoves b player pos@(MkPos _ rank) = catMaybes $ concat
    -- Single space forward
  [ [ do pos' <- offsetPos pos (0, direction)
         guard (pos' `Map.notMember` b)
         Just (MkMove piece pos NoCapture pos')
    ]

    -- Double move, only if the pawn is on the starting rank
  , [ do guard (onStartingRank player rank)

         pos' <- offsetPos pos (0, direction)
         guard (pos' `Map.notMember` b)
         pos'' <- offsetPos pos' (0, direction)
         guard (pos'' `Map.notMember` b)

         Just (MkMove piece pos NoCapture pos'')
    ]

    -- Diagonal captures
  , [ do posDiag <- offsetPos pos (dx, direction)
         MkPiece player' _ <- b Map.!? posDiag

         guard (player /= player') -- Can't capture your own piece

         Just (MkMove piece pos Capture posDiag)
    | dx <- [1, -1] -- +1 is for one of the adjacent files, -1 is for the other
    ]

    -- TODO: En passant
    -- TODO: Pawn promotion
  ]
  where
    piece = MkPiece player Pawn
    direction = case player of
                  White ->  1
                  Black -> -1

    onStartingRank White R2 = True
    onStartingRank Black R7 = True
    onStartingRank _     _  = False



-- Pseudo-legal moves  --
-------------------------

-- https://www.chessprogramming.org/Pseudo-Legal_Move

psuedoLegalMovesAtPos :: Board -> Pos -> [Move]
psuedoLegalMovesAtPos b pos
  = case b Map.!? pos of
      Nothing -> []
      Just piece@(MkPiece player pieceType)
        -> case pieceType of
             Pawn   -> pawnMoves b player pos
             Knight -> fixedMovesFromOffsets b piece pos knightOffsets
             King   -> fixedMovesFromOffsets b piece pos kingOffsets -- TODO: Castling
             Bishop -> slidingMoves [NE, SW, SE, NW]
             Rook   -> slidingMoves [North, East, South, West]
             Queen  -> slidingMoves [North .. NW]
        where
          slidingMoves directions = concat [slidingMovesInDirection b piece pos dir
                                           | dir <- directions
                                           ]

-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos A R1)
-- []
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos B R1)
-- ["Nb1d2","Nb1a3"]
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos C R1)
-- ["Bc1d2","Bc1e3","Bc1f4"]
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos D R1)
-- ["Qd1d2","Qd1d3","Qd1xd4","Qd1e2","Qd1f3","Qd1g4","Qd1h5","Qd1c2","Qd1b3","Qd1a4"]
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos E R1)
-- ["Ke1d2","Ke1e2"]
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos H R1)
-- ["Rh1h2","Rh1h3","Rh1h4","Rh1h5","Rh1h6","Rh1xh7"]
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos C R2)
-- []
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos E R4)
-- ["Pe4e5","Pe4xf5"]
-- >>> map pretty $ psuedoLegalMovesAtPos egBoard (MkPos F R5)
-- []

-- >>> map pretty $ psuedoLegalMovesAtPos (fromJust (FEN.parseBoard "3N1Q1k/Bp3P1P/p7/3np1p1/2B1rrNP/K4p2/R3P3/8")) (MkPos H R8)
-- ["kh8g7","kh8g8","kh8xh7"]

-- Check checks --
------------------

playersInCheck :: Board -> [Player]
playersInCheck b
  = Map.keys b
    |> map (psuedoLegalMovesAtPos b)
    |> concat
    |> filter (\(MkMove _ _ capIndicator _) -> capIndicator == Capture)
    |> map (\(MkMove _ _ _ pos) -> b Map.! pos) -- Index should always be in-bounds if `psuedoLegalMovesAtPos` generates Moves correctly
    |> filter (getPieceType .> (== King))
    |> map getPlayer
    -- |> nub -- We could remove duplicates with `nub`, but this prevents our function from being lazy
              -- because it needs to evaluate the entire list to remove duplicates from it.

hasNolegalMoves :: Player -> State -> Bool
hasNolegalMoves player state
    -- List all pieces on the board with their positions
  = Map.toAscList state.board

    -- Keep only the current player's pieces (with positions)
    |> filter (snd .> getPlayer .> (== player))

    -- For each position, list the psuedo-legal moves, and concat them into a single list of moves
    |> concatMap (fst .> psuedoLegalMovesAtPos state.board)
    -- Alt:
    -- |> map (fst .> psuedoLegalMovesAtPos state.board)
    -- |> concat
    -- Alt 2:
    -- |> (>>= (fst .> psuedoLegalMovesAtPos state.board))

    -- For each move, try to make that move (returning `Either String State`)
    |> map (\move -> makeMove move state)

    -- Keep only the results which succeeded and returned a `Right`,
    -- i.e. only the cases where the move was valid
    |> filter isRight

    -- Check if there are any elements of the list, i.e. if the list is `null`
    |> null

    -- All of that might seem like a lot of work, and it probably would be
    -- if we had to `makeMove` *every* psuedo-legal move
    -- for *every* piece the player has *every* turn.

    -- But remember, Haskell is *lazy*. Why do all that work if you don't have to?
    -- Let's work backwards to see what work actually needs to be done:

    -- - `null` only needs to see a single element in the list to return `False`
    -- - `filter isRight` only needs to let through a single `Right` element
    -- - `map (\move -> makeMove move state)` only needs to produce a single `Right` element,
    --   i.e. a single valid next state
    -- - `concatMap (fst .> psuedoLegalMovesAtPos state.board)` only needs to produce a single move
    --   which makes it past `makeMove`, i.e. a single truly legal move
    -- - `filter (snd .> getPlayer .> (== player))` only needs to produce a single position+piece for
    --   the current player which goes on to be a legal move
    -- - `Map.toAscList state.board` only needs to produce a single position+piece which makes it all
    --   the way through the pipeline

    -- In practice, this function will go piece-by-piece and exit as soon as it finds one which can move.
    -- In a chess game, most of the time most of your pieces can move, so this function will be quite efficient.
    -- The only time it will check *every* piece is if *none* of them (or only the last one checked) can move,
    -- e.g. in checkmate.

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe _         = Nothing

data GameOver = Checkmate | Stalemate
  deriving (Show, Eq)

checkGameOver :: State -> Maybe GameOver
checkGameOver state
  = if hasNolegalMoves state.sideToMove state
    then Just (if isCheck then Checkmate else Stalemate)
    else Nothing
  where
    isCheck = state.sideToMove `elem` playersInCheck state.board


-- Play  --
-----------

-- ghci> playFromFEN DarkTheme "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

playFromFEN :: Theme -> String -> IO ()
playFromFEN theme str = do
  case FEN.stateFromFEN str of
    Nothing -> putStrLn "Invalid FEN"
    Just state -> gameLoop theme state

gameLoop :: Theme -> State -> IO ()
gameLoop theme state = do
  printState theme state
  state' <- playMove state
  printState theme state'

  case checkGameOver state' of
    Just Checkmate -> putStrLn "Checkmate. White wins!"
    Just Stalemate -> putStrLn "Stalemate. It's a draw."
    Nothing -> do
      state'' <- stockfishMakeUnderpoweredMove state'
      case checkGameOver state'' of
        Just Checkmate -> putStrLn "Checkmate. Black wins!"
        Just Stalemate -> putStrLn "Stalemate. It's a draw."
        Nothing -> gameLoop theme state''

-- TODO: Terse notation for moves

playMove :: State -> IO State
playMove state = do
  putStrLn "Enter move:"
  str <- getLine
  case parseMaybe moveP str of
    Nothing -> do
      putStrLn "Error: Failed to parse move"
      playMove state
    Just move -> case makeMove move state of
      Left errStr -> do
        putStrLn errStr
        playMove state
      Right state' -> pure state'

stockfishMakeUnderpoweredMove :: State -> IO State
stockfishMakeUnderpoweredMove state = do
  putStrLn "Stockfish is thinking..."
  threadDelay 5000000 -- Arbitrary 5 second delay
  lanMove <- Stockfish.calculateBestMove cfg (FEN.fromState state)
  putStrLn $ "Stockfish's move (LAN): " ++ pretty lanMove
  case makeMoveLAN lanMove state of
    Left errStr -> do
        putStrLn errStr
        ioError (userError "Something went wrong during Stockfish's turn")
    Right state' -> pure state'
  where
    cfg = Stockfish.MkConfig
      { Stockfish.elo = Just 1320
      , Stockfish.stopCondition = Stockfish.Depth 3
      , Stockfish.cpuThreads = 1
      }

-- TODO: Check for checks and checkmates

-- Extra parsing  --
--------------------

-- >>> parseMaybe moveP "Qh4xe1"
-- Just (MkMove (MkPiece White Queen) (MkPos H R4) Capture (MkPos E R1))
-- >>> parseMaybe moveP "Pd2d4"
-- Just (MkMove (MkPiece White Pawn) (MkPos D R2) NoCapture (MkPos D R4))

-- https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Disambiguating_moves
moveP :: Parser Move
moveP = MkMove <$> pieceP
               <*> posP
               <*> captureIndicatorP
               <*> posP


captureIndicatorP :: Parser CaptureIndicator
captureIndicatorP = Capture <$ char 'x'
         <|> pure NoCapture



instance Pretty Move where
  pretty (MkMove piece pos captureIndicator pos') = concat
    [ pretty piece
    , pretty pos
    , pretty captureIndicator
    , pretty pos'
    ]

instance Pretty CaptureIndicator where
  pretty Capture = "x"
  pretty NoCapture = ""
