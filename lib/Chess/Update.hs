-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html
-- 'When OverloadedRecordDot is enabled one can write a.b to mean the b field of the a record expression.'
{-# language OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Chess.Update where

import Debug.Trace (traceShowId)

import Chess.Prelude
import Chess.Model
import Chess.View

import Chess.Notation.Prelude
import qualified Chess.Notation.FEN as FEN
import Chess.Notation.LAN
import Text.Yoda

import qualified Chess.Engine.Stockfish as Stockfish

import Data.Maybe ( catMaybes, fromJust, mapMaybe )
import qualified Data.Map.Strict as Map
import Control.Monad (guard)
import Control.Concurrent (threadDelay)
import Data.List (find)
import Data.Either ( isRight )

-- Piece moves --
-----------------

-- https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Disambiguating_moves
data Move = MkMove Piece Pos CaptureIndicator Pos
          | PawnPromotion File PromotionRank PromotionType
          | Castle CastlingSide
  deriving (Show, Eq)

data CaptureIndicator = NoCapture | Capture
  deriving (Show, Eq, Ord, Enum)

onlyMove :: Piece -> Pos -> Pos -> Move
onlyMove piece pos pos' = MkMove piece pos NoCapture pos'

captureMove :: Piece -> Pos -> Pos -> Move
captureMove piece pos pos' = MkMove piece pos Capture pos'

isCapture :: Move -> Bool
isCapture (MkMove _ _ Capture _) = True
isCapture _ = False

data CastlingSide = Kingside | Queenside
  deriving (Show, Eq, Ord, Enum)

data PromotionRank = PR1 | PR8
  deriving (Show, Eq, Ord, Enum)

fromPromRank :: PromotionRank -> Rank
fromPromRank PR8 = R8
fromPromRank PR1 = R1

toPromRank :: Rank -> Maybe PromotionRank
toPromRank R8 = Just PR8
toPromRank R1 = Just PR1
toPromRank _  = Nothing

data PromotionType
  = PromKnight
  | PromBishop
  | PromRook
  | PromQueen
  deriving (Show, Eq, Ord, Enum)

fromPromotionType :: PromotionType -> PieceType
fromPromotionType = \case
  PromKnight -> Knight
  PromBishop -> Bishop
  PromRook   -> Rook
  PromQueen  -> Queen

toPromotionType :: PieceType -> Maybe PromotionType
toPromotionType = \case
  Knight -> Just PromKnight
  Bishop -> Just PromBishop
  Rook   -> Just PromRook
  Queen  -> Just PromQueen
  _      -> Nothing 

-- isasdCapture :: CaptureIndicator -> Bool
-- isasdCapture Capture   = True
-- isasdCapture NoCapture = False

makeMove :: Move -> State -> Either String State
makeMove move state = do
  (piece, pos, captureIndicator) <- case move of
    MkMove piece@(MkPiece player _) pos captureIndicator pos' -> do
      validatePieceAtPos piece pos (board state)

      validateIsCapture piece captureIndicator pos' (board state)

      guardWithError (player == state.sideToMove)
                    ("Error: " ++ show state.sideToMove ++ " can't move "
                                ++ show player ++ "'s piece")

      pure (piece, pos, captureIndicator)
    PawnPromotion file pRank _ -> do
      case state.sideToMove of
        White -> do
          guardWithError (pRank == PR8)
                         ("Error: Can't promote pawn unless it reaches the opposite rank")
          pure (MkPiece White Pawn, MkPos file R7, NoCapture)
        Black -> do
          guardWithError (pRank == PR1)
                         ("Error: Can't promote pawn unless it reaches the opposite rank")
          pure (MkPiece Black Pawn, MkPos file R2, NoCapture)

    Castle _ -> case state.sideToMove of
        White -> pure (MkPiece White King, MkPos E R1, NoCapture)
        Black -> pure (MkPiece Black King, MkPos E R8, NoCapture)


  -- TODO: validate piece can move to pos'
  -- guardWithError (move `elem` psuedoLegalMovesAtPos state.board pos)
  --                ("Error: " ++ pretty move ++ " is not a valid move")

  (_, stateUpdate) <- maybeToEither
                        (find (\(move',_) -> move' == move)
                              (psuedoLegalMovesAtPos state pos))
                        ("Error: " ++ pretty move ++ " is not a valid move") 

  -- let board' = board state
  --              |> Map.delete pos
  --              |> Map.insert pos' piece
  let state' = state
               -- step half move clock first because some moves will reset it in stateUpdate
               |> modifyHalfMoveClock (stepHalfMoveClock piece captureIndicator)
               |> modifyEnPassantTarget (const Nothing) -- En passant target always Nothing unless stateUpdate makes it Just
               |> stateUpdate

  let checkedPlayers = playersInCheck state'

  -- Check checks
  guardWithError (state.sideToMove `notElem` checkedPlayers)
                 ("Error: You cannot make a move that would leave you in check")

  -- let opponentInCheck = nextPlayer state.sideToMove `elem` checkedPlayers

  -- TODO: add current checks to state


  Right (state' { sideToMove = nextPlayer state.sideToMove
                , fullMoveCounter = stepFullMoveCounter piece state.fullMoveCounter
                -- TODO: update castlingAbility and enPassantTarget
                })
-- makeMove cMove@(Castle _) = 
makeMove _ state = Left "TODO: Implement promotion and castling"

maybeToEither :: Maybe b -> a -> Either a b
maybeToEither m e = maybe (Left e) Right m

makeMoveLAN :: LAN -> State -> Either String State
makeMoveLAN lan state = do
  move <- fromLAN state lan
  makeMove move state

fromLAN :: State -> LAN -> Either String Move
fromLAN _ (MkLAN _ (MkPos file rank) (Just (MkPiece _ pieceType))) = do
  promType <- maybeToEither
                (toPromotionType pieceType)
                ("Error: Can't promote to " ++ show pieceType)
  promRank <- maybeToEither
                (toPromRank rank)
                ("Error: Can't promote on rank " ++ show rank)
  Right (PawnPromotion file promRank promType)
fromLAN state (MkLAN pos pos' _) = do
  piece@(MkPiece player pieceType) <- getPieceAtPos pos state.board

  let maybeSpecialMove = case pieceType of
        King -> case player of
          White -> if | pos == MkPos E R1 && pos' == MkPos G R1 -> Just (Castle Kingside)
                      | pos == MkPos E R1 && pos' == MkPos C R1 -> Just (Castle Queenside)
                      | otherwise                               -> Nothing
          Black -> if | pos == MkPos E R8 && pos' == MkPos G R8 -> Just (Castle Kingside)
                      | pos == MkPos E R8 && pos' == MkPos C R8 -> Just (Castle Queenside)
                      | otherwise                               -> Nothing
        Pawn -> do
          epTarget <- enPassantTarget state
          pos'EPTarget <- toEnPassantTarget pos'

          if epTarget == pos'EPTarget
            then Just (MkMove piece pos Capture pos')
            else Nothing
        _    -> Nothing
  
  case maybeSpecialMove of
    Just specialMove -> Right specialMove
    _ -> do
      captureIndicator <- getCaptureIndicator player pos' state.board
      Right (MkMove piece pos captureIndicator pos')

-- fromLAN _ ShortCastle = Right (Castle Kingside)
-- fromLAN _ LongCastle = Right (Castle Queenside)

-- fromLAN _ _ = Left "Error: Full LAN not yet implemented"
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
      Nothing -> Right ()
                 -- En passant complicates this: Left ("Error: Capture specified, but there is no piece at " ++ pretty pos)
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
                    Nothing -> Just ( onlyMove piece pos pos')
                    Just (MkPiece player' _)
                      -> if player /= player'
                         then Just (captureMove piece pos pos')
                         else Nothing
        )
 .> catMaybes

knightOffsets :: [(Int, Int)]
knightOffsets = [(1,2), (2,1), (2, -1), (1,-2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

kingOffsets :: [(Int, Int)]
kingOffsets = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
-- kingOffsets = [(dx,dy) | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0)]

castlingMoves :: State -> [(Move, State -> State)]
castlingMoves state = catMaybes [ castlingMove state castlingSide
                                | castlingSide <- [Kingside, Queenside]]

castlingMove :: State -> CastlingSide -> Maybe (Move, State -> State)
castlingMove state castlingSide = do
  guard rightToCastle
  guard (allSquaresClear [ MkPos file backRank
                         | file <- case castlingSide of
                             Kingside  -> [F, G]
                             Queenside -> [B, C, D]
                         ])

  -- These shouldn't be necessary if `castlingAbility` is tracked correctly
  king `onSquare` kingPos
  rook `onSquare` rookPos

  Just ( Castle castlingSide
       ,   modifyBoard (  Map.delete kingPos
                       .> Map.delete rookPos
                       .> Map.insert kingPos' king
                       .> Map.insert rookPos' rook
                       )
        .> removePlayerCastlingRights player
       )
  where
    player = state.sideToMove
    rightToCastle = case player of
      White -> case castlingSide of
        -- OverloadedRecordDot extension for OOP-like accessing of record fields
        Kingside  -> state.castlingAbility.whiteKingside
        Queenside -> state.castlingAbility.whiteQueenside
      Black -> case castlingSide of
        Kingside  -> state.castlingAbility.blackKingside
        Queenside -> state.castlingAbility.blackQueenside

    backRank = case player of
      White -> R1
      Black -> R8

    king = MkPiece player King
    rook = MkPiece player Rook

    kingPos = MkPos E backRank
    (rookPos, kingPos', rookPos') = case castlingSide of
      Kingside  -> (MkPos H backRank, MkPos G backRank, MkPos F backRank)
      Queenside -> (MkPos A backRank, MkPos C backRank, MkPos D backRank)

    allSquaresClear = all (`Map.notMember` state.board)

    onSquare piece p = do
      piece' <- state.board Map.!? p
      guard (piece == piece')


removePlayerCastlingRights :: Player -> State -> State
removePlayerCastlingRights player
  = modifyCastlingAbility $ \ca -> case player of
      White -> ca{ whiteKingside  = False -- Record update
                 , whiteQueenside = False
                 }
      Black -> ca{ blackKingside  = False
                 , blackQueenside = False
                 }


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
             else [captureMove piece startPos pos']
        Nothing -> onlyMove piece startPos pos' : go pos'

-- >>> map pretty $ slidingMovesInDirection egBoard (MkPiece White Queen) (MkPos D R1) North
-- ["Qd1d2","Qd1d3","Qd1xd4"]


-- Pawn Moves --
----------------



-- pawnMoves :: Board -> Player -> Pos -> [(Move)]
-- pawnMoves b player pos@(MkPos _ rank) = catMaybes $ concat
--     -- Single space forward
--   [ [ do pos' <- offsetPos pos (0, direction)
--          guard (pos' `Map.notMember` b)
--          Just (onlyMove piece pos pos')
--     ]

--     -- Double move, only if the pawn is on the starting rank
--   , [ do guard (onStartingRank player rank)

--          pos' <- offsetPos pos (0, direction)
--          guard (pos' `Map.notMember` b)
--          pos'' <- offsetPos pos' (0, direction)
--          guard (pos'' `Map.notMember` b)

--          Just (onlyMove piece pos pos'')
--     ]

--     -- Diagonal captures
--   , [ do posDiag <- offsetPos pos (dx, direction)
--          MkPiece player' _ <- b Map.!? posDiag

--          guard (player /= player') -- Can't capture your own piece

--          Just (captureMove piece pos posDiag)
--     | dx <- [1, -1] -- +1 is for one of the adjacent files, -1 is for the other
--     ]

--     -- TODO: En passant
--     -- TODO: Pawn promotion
--   ]
--   where
--     piece = MkPiece player Pawn
--     direction = case player of
--                   White ->  1
--                   Black -> -1

--     onStartingRank White R2 = True
--     onStartingRank Black R7 = True
--     onStartingRank _     _  = False

pawnMoves' :: State -> Player -> Pos -> [(Move, State -> State)]
pawnMoves' state player pos@(MkPos file rank)
  = let b = board state
        piece = MkPiece player Pawn
        direction = case player of
                      White ->  1
                      Black -> -1
    in catMaybes $ concat

    -- Single space forward
  [ [do pos' <- offsetPos pos (0, direction)
        guard (pos' `Map.notMember` b)
        Just ( onlyMove piece pos pos'
             , movePieceOnBoard piece pos pos'
             )
    ]

    -- Double move, only if the pawn is on the starting rank
  , [do guard (onStartingRank player rank)

        pos' <- offsetPos pos (0, direction)
        guard (pos' `Map.notMember` b)
        pos'' <- offsetPos pos' (0, direction)
        guard (pos'' `Map.notMember` b)

        -- We could use `toEnPassantTarget` directly in the lambda for `modifyEnPassantTarget`,
        -- but that's semantically different. It should never fail, but if it does,
        -- doing it here will exclude the move, rather than include the move without
        -- an en passant target
        epTarget <- toEnPassantTarget pos'

        Just ( onlyMove piece pos pos''
             , movePieceOnBoard piece pos pos''
               .> modifyEnPassantTarget (\_ -> Just epTarget) -- TODO when to reset enpassant for every other move?
             )
    ]

    -- Diagonal captures
  , [do posDiag <- offsetPos pos (dx, direction)

        case b Map.!? posDiag of
          -- Normal capture
          Just (MkPiece player' _) -> do
            guard (player /= player') -- Can't capture your own piece

            Just ( captureMove piece pos posDiag
                 , movePieceOnBoard piece pos posDiag
                 )
          Nothing -> do
            -- En passant capture
            posDiagEPTarget <- toEnPassantTarget posDiag
            epTarget <- enPassantTarget state
            guard (posDiagEPTarget == epTarget)

            -- This was what the code was initially, but this does something very different:
            -- guard (toEnPassantTarget posDiag == enPassantTarget state)

            -- It never checks if there's a valid en passant target
            -- or whether posDiag is in the correct rank for en passant.
            -- The bug is most apparent when BOTH conditions fail,
            -- i.e. `toEnPassantTarget posDiag` and `enPassantTarget state` both return `Nothing`
            -- and therefore `toEnPassantTarget posDiag == enPassantTarget state` returns `True`


            -- The pawn being en-passant-ed will always be one square advanced from
            -- where the capturing pawn is moving to
            capturedPawnPos <- offsetPos posDiag (0, negate direction)

            MkPiece player' Pawn <- b Map.!? capturedPawnPos

            guard (player /= player')
            

            Just ( captureMove piece pos posDiag
                 , movePieceOnBoard piece pos posDiag
                   .> modifyBoard (Map.delete capturedPawnPos)
                 )
    | dx <- [1, -1] -- +1 is for one of the adjacent files, -1 is for the other
    ]

    -- Promotion
    -- Using 'MultiWayIf' extension we can use guard syntax for 'if' expressions
    -- though it is slightly modified to use '->' instead of '='
  , [if | rank == R7 -> do guard (player == White)
                           Just ( PawnPromotion file PR8 promType
                                , modifyBoard (  Map.delete pos
                                              .> Map.insert (MkPos file R8)
                                                            (MkPiece White
                                                                    (fromPromotionType promType))
                                              )
                                )
        | rank == R2 -> do guard (player == Black)
                           Just ( PawnPromotion file PR1 promType
                                , modifyBoard (  Map.delete pos
                                              .> Map.insert (MkPos file R1)
                                                            (MkPiece Black
                                                                    (fromPromotionType promType))
                                              )
                                )
        | otherwise -> Nothing

    | promType <- [PromKnight, PromBishop, PromRook, PromQueen]
    ]
    -- TODO: En passant
    -- TODO: Pawn promotion
  ]
  where
    onStartingRank White R2 = True
    onStartingRank Black R7 = True
    onStartingRank _     _  = False

movePieceOnBoard :: Piece -> Pos -> Pos -> State -> State
movePieceOnBoard piece pos pos' = modifyBoard (Map.delete pos .> Map.insert pos' piece)

-- Pseudo-legal moves  --
-------------------------

-- https://www.chessprogramming.org/Pseudo-Legal_Move

psuedoLegalMovesAtPos_ :: State -> Pos -> [Move]
psuedoLegalMovesAtPos_ state = psuedoLegalMovesAtPos state .> map fst

psuedoLegalMovesAtPos :: State -> Pos -> [(Move, State -> State)]
psuedoLegalMovesAtPos state pos
  = case state.board Map.!? pos of
      Nothing -> []
      Just piece@(MkPiece player pieceType)
        -> if player /= state.sideToMove
             then []
             else case pieceType of
                    Pawn   -> pawnMoves' state player pos
                    Knight -> fixedMovesFromOffsets state.board piece pos knightOffsets
                              |> map addPieceMoveStateUpdate
                    King   -> concat
                                [ fixedMovesFromOffsets state.board piece pos kingOffsets
                                  |> map addPieceMoveStateUpdate
                                  |> map (\(m, f) -> (m, f .> removePlayerCastlingRights player))
                                , castlingMoves state
                                ]
                    Bishop -> slidingMoves [NE, SW, SE, NW] |> map addPieceMoveStateUpdate
                    Rook   -> slidingMoves [North, East, South, West]
                              |> map (addPieceMoveStateUpdate .> rookCastlingInvalidation)
                    Queen  -> slidingMoves [North .. NW] |> map addPieceMoveStateUpdate
        where
          slidingMoves directions = concat [ slidingMovesInDirection state.board piece pos dir
                                           | dir <- directions
                                           ]
                                           
rookCastlingInvalidation :: (Move, State -> State) -> (Move, State -> State)
rookCastlingInvalidation (move, f)
  = ( move
    , f .> modifyCastlingAbility updateFunc
    )
  where
    updateFunc ca = case move of
      MkMove (MkPiece White Rook) (MkPos A R1) _ _ -> ca{ whiteQueenside = False }
      MkMove (MkPiece White Rook) (MkPos H R1) _ _ -> ca{ whiteKingside = False }
      MkMove (MkPiece Black Rook) (MkPos A R8) _ _ -> ca{ blackQueenside = False }
      MkMove (MkPiece White Rook) (MkPos A R8) _ _ -> ca{ blackKingside = False }
      _ -> ca

addPieceMoveStateUpdate :: Move -> (Move, State -> State)
addPieceMoveStateUpdate move@(MkMove piece pos _ pos')
  = (move, movePieceOnBoard piece pos pos')
addPieceMoveStateUpdate move = (move, id)

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

playersInCheck :: State -> [Player]
playersInCheck state
  = Map.keys b
    |> map (\pos -> psuedoLegalMovesAtPos_ (state{sideToMove = White}) pos
                 ++ psuedoLegalMovesAtPos_ (state{sideToMove = Black}) pos)
    |> concat
    |> filter isCapture
    -- En passant doesn't capture on the same square it moves to,
    -- so `b Map.!? pos` may return `Nothing` even for capture moves
    |> mapMaybe (\case (MkMove _ _ _ pos) -> b Map.!? pos; _ -> Nothing)

    -- Debugging version:
    -- |> (\moves -> map (\mv@(MkMove _ _ _ pos) -> case b Map.!? pos of
    --                                  Just piece -> piece
    --                                  Nothing -> error $ unlines [ "ERROR!"
    --                                                             , "offending move = " ++ show mv
    --                                                             , "state = " ++ show state
    --                                                             , "moves = " ++ show moves
    --                                                             ])
    --                   moves)

    |> filter (getPieceType .> (== King))
    |> map getPlayer
  where
    b = board state

hasNolegalMoves :: Player -> State -> Bool
hasNolegalMoves player state
    -- List all pieces on the board with their positions
  = Map.toAscList state.board

    -- Keep only the current player's pieces (with positions)
    |> filter (snd .> getPlayer .> (== player))

    -- For each position, list the psuedo-legal moves, and concat them into a single list of moves
    |> concatMap (fst .> psuedoLegalMovesAtPos_ state)
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

data GameOver = Checkmate | Stalemate | FiftyMoveRule
  deriving (Show, Eq)

checkGameOver :: State -> Maybe GameOver
checkGameOver state
  = if hasNolegalMoves state.sideToMove state
    then Just (if isCheck then Checkmate else Stalemate)

    else if state.halfMoveClock >= 100
    then Just FiftyMoveRule

    else Nothing
  where
    isCheck = state.sideToMove `elem` playersInCheck state


-- Play  --
-----------

-- In the lecture:
-- ghci> humanVsHumanFromFEN LightTheme startingFEN

-- Play against stockfish from an example position
-- ghci> humanVsStockfishFromFEN DarkTheme "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

-- Watch stockfish play itself, slowly
-- ghci> stockfishVsStockfishFromFEN DarkTheme startingFEN

-- Watch stockfish play itself quickly for rapid error finding
-- ghci> stockfishVsStockfishQuickFromFEN DarkTheme startingFEN


humanVsHumanFromFEN :: Theme -> String -> IO ()
humanVsHumanFromFEN theme = playFromFEN theme playMove playMove

humanVsStockfishFromFEN :: Theme -> String -> IO ()
humanVsStockfishFromFEN theme = playFromFEN theme playMove stockfishMakeUnderpoweredMove

stockfishVsStockfishFromFEN :: Theme -> String -> IO ()
stockfishVsStockfishFromFEN theme = playFromFEN theme stockfishMakeUnderpoweredMove stockfishMakeUnderpoweredMove

stockfishVsStockfishQuickFromFEN :: Theme -> String -> IO ()
stockfishVsStockfishQuickFromFEN theme = playFromFEN theme stockfishQuick stockfishQuick

startingFEN :: String
startingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

playFromFEN :: Theme -> (State -> IO State) -> (State -> IO State) -> String -> IO ()
playFromFEN theme player1 player2 str = do
  case FEN.stateFromFEN str of
    Nothing -> putStrLn "Invalid FEN"
    Just state -> gameLoop theme player1 player2 state

-- gameLoop :: Theme -> State -> IO ()
-- gameLoop theme state = do
--   printState theme state
--   state' <- playMove state
--   printState theme state'

--   case checkGameOver state' of
--     Just Checkmate -> putStrLn "Checkmate. White wins!"
--     Just Stalemate -> putStrLn "Stalemate. It's a draw."
--     Nothing -> do
--       state'' <- stockfishMakeUnderpoweredMove state'
--       case checkGameOver state'' of
--         Just Checkmate -> putStrLn "Checkmate. Black wins!"
--         Just Stalemate -> putStrLn "Stalemate. It's a draw."
--         Nothing -> gameLoop theme state''

gameLoop :: Theme -> (State -> IO State) -> (State -> IO State) -> State -> IO ()
gameLoop theme player1 player2 state = do
  printState theme state
  state' <- player1 state
  printState theme state'

  case checkGameOver state' of
    Just Checkmate -> putStrLn "Checkmate!"
    Just Stalemate -> putStrLn "Stalemate. It's a draw."
    Just FiftyMoveRule -> putStrLn "Fifty-move rule. It's a draw."
    Nothing -> do
      state'' <- player2 state'
      case checkGameOver state'' of
        Just Checkmate -> putStrLn "Checkmate!"
        Just Stalemate -> putStrLn "Stalemate. It's a draw."
        Just FiftyMoveRule -> putStrLn "Fifty-move rule. It's a draw."
        Nothing -> gameLoop theme player1 player2 state''

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

stockfishQuick :: State -> IO State
stockfishQuick state = do
  putStrLn "Stockfish is thinking..."
  -- threadDelay 5000000 -- Arbitrary 5 second delay
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
-- >>> parseMaybe moveP "d8q"
-- Just (PawnPromotion D PR8 PromQueen)
-- >>> parseMaybe moveP "O-O"
-- Just (Castle Kingside)
-- >>> parseMaybe moveP "O-O-O"
-- Just (Castle Queenside)


-- https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Disambiguating_moves
moveP :: Parser Move
moveP = mkMoveP
    <|> pawnPromotionP
    <|> castleP

mkMoveP :: Parser Move
mkMoveP = MkMove <$> pieceP
                 <*> posP
                 <*> captureIndicatorP
                 <*> posP

pawnPromotionP :: Parser Move
pawnPromotionP
  = PawnPromotion <$> fileP
                  <*> promotionRankP
                  <*> promTypeP

castleP :: Parser Move
castleP = Castle <$> castlingSideP

castlingSideP :: Parser CastlingSide
castlingSideP
  =  Queenside <$ string "O-O-O"
 <|> Kingside  <$ string "O-O"

captureIndicatorP :: Parser CaptureIndicator
captureIndicatorP = Capture <$ char 'x'
         <|> pure NoCapture

promotionRankP :: Parser PromotionRank
promotionRankP = PR1 <$ char '1'
             <|> PR8 <$ char '8'

promTypeP :: Parser PromotionType
promTypeP
  =  PromKnight <$ char 'n'
 <|> PromBishop <$ char 'b'
 <|> PromRook   <$ char 'r'
 <|> PromQueen  <$ char 'q'

instance Pretty Move where
  pretty (MkMove piece pos captureIndicator pos') = concat
    [ pretty piece
    , pretty pos
    , pretty captureIndicator
    , pretty pos'
    ]
  pretty (PawnPromotion file pRank promType) = concat
    [ pretty file
    , pretty pRank
    , pretty promType
    ]
  pretty (Castle castlingSide) = case castlingSide of
    Kingside  -> "O-O"
    Queenside -> "O-O-O"

instance Pretty CaptureIndicator where
  pretty Capture = "x"
  pretty NoCapture = ""

instance Pretty PromotionRank where
  pretty PR1 = "1"
  pretty PR8 = "8"

instance Pretty PromotionType where
  pretty promType = case promType of
    PromKnight -> "n"
    PromBishop -> "b"
    PromRook   -> "r"
    PromQueen  -> "q"
