
module Chess.Engine.Stockfish
  ( Config(..), StopCondition(..)
  , withStockfish, egStockfish, calculateBestMove, defaultConfig
  ) where

{-
This code relies on `stockfish` being installed and on your `PATH`.

To install it on Ubuntu/WSL: `sudo apt install stockfish`

Stockfish UCI and commands documentation: https://github.com/official-stockfish/Stockfish/wiki/UCI-&-Commands

-}

import Chess.Notation.Prelude
import Chess.Notation.FEN (FEN)
import qualified Chess.Notation.FEN as FEN
import Chess.Notation.LAN

import Text.Yoda


-- Requires `process` library for running shell commands
import System.Process
    ( shell,
      withCreateProcess,
      CreateProcess(std_out, std_in),
      StdStream(CreatePipe) )
import System.IO (hPutStrLn, hGetLine, hFlush, Handle)
import Control.Monad (void)

egStockfish :: IO ()
egStockfish
  = withStockfish $ \runCmd' _ -> do
        runCmd' "position startpos" >>= putLines
        runCmd' "d" >>= putLines

 

data Config = MkConfig
  { stopCondition :: StopCondition
  , elo :: Maybe Int
  , cpuThreads :: Int
  } deriving (Show, Eq)

data StopCondition = Depth Int | MoveTimeMilliSecs Int
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = MkConfig
  { elo = Nothing
  , stopCondition = MoveTimeMilliSecs 1000
  , cpuThreads = 1
  }

calculateBestMove :: Config -> FEN -> IO LAN
calculateBestMove config fen = withStockfish $ \runCmd' calcMove' -> do

  -- set options
  case elo config of
    Nothing -> pure ()
    Just elo' -> do void $ runCmd' $ "setoption UCI_Elo " ++ show elo'

  _ <- runCmd' $ "setoption Threads " ++ show (cpuThreads config)

  -- set board state:
  _ <- runCmd' $ "position fen " ++ pretty fen
  
  -- find best move with stopping condition for search
  calcMove' (stopCondition config)

-- ghci> expectJust (parseMaybe FEN.fenP "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 0") >>= calculateBestMove defaultConfig

withStockfish :: ((String -> IO [String]) -> (StopCondition -> IO LAN) -> IO a) -> IO a
withStockfish stockfishCommands
  = withCreateProcess (shell "stockfish"){std_in = CreatePipe, std_out = CreatePipe}
  $ \mStdout mStdin _ _ph ->
        case (mStdout, mStdin) of
          (Just stdin, Just stdout) -> do
            -- use UCI interface
            sendCmd stdin "uci"
            _ <- getLinesUntil stdout (== "uciok")

            -- Run desired commands
            out <- stockfishCommands (runCmd stdin stdout) (calcMove stdin stdout) -- provide run function with the correct stdin and stdout handles to stockfish

            -- Close stockfish
            sendCmd stdin "quit"

            -- return output from commands
            pure out
          _ -> ioError (userError "This error should never happen because we used `CreatePipe` for all handles")

-- Internal --
--------------

sendCmd :: Handle -> String -> IO ()
sendCmd handle str = do
  hPutStrLn handle str
  hFlush handle -- Makes sure to flush the buffer and definitely send the command

getLinesUntil :: Handle -> (String -> Bool) -> IO [String]
getLinesUntil handle predicate = go
  where
    go = do
      outLn <- hGetLine handle
      if predicate outLn
      then pure [outLn]
      else do outLns <- go
              pure (outLn : outLns)

runCmd :: Handle -> Handle -> String -> IO [String]
runCmd stdin stdout cmd = do
  sendCmd stdin cmd
  sendCmd stdin "isready"
  getLinesUntil stdout (== "readyok")


-- `isready` doesn't work for checking if the calculation is done since UCI specifies:
-- "This command must always be answered with "readyok" and can be sent also when the engine is calculating
--  in which case the engine should also immediately answer with "readyok" without stopping the search."
calcMove :: Handle -> Handle -> StopCondition -> IO LAN
calcMove stdin stdout stopCond = do
  sendCmd stdin $ "go " ++ goConfig
  out <- getLinesUntil stdout (\str -> take 8 str == "bestmove")

  let bestMoveStr = drop 9 (last out)
  
  case parseMaybe lanP bestMoveStr of
    Nothing   -> ioError (userError $ "Failed to parse best move from stockfish: " ++ bestMoveStr)
    Just move -> pure move
  where
    goConfig = case stopCond of
                 Depth d -> "depth " ++ show d
                 MoveTimeMilliSecs ms -> "movetime " ++ show ms

-- Utility --
-------------

putLines :: [String] -> IO ()
putLines outs = putStr (unlines outs)

expectJust :: Maybe a -> IO a
expectJust (Just x) = pure x
expectJust Nothing = ioError (userError "Expected a `Just` but found `Nothing`")
