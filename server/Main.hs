{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Chess.Model
import Chess.Update
import Chess.Notation.LAN

import Text.Yoda

import Web.Scotty
import Network.Wai

import Control.Concurrent
import Control.Concurrent.STM (newTVarIO, TVar, readTVarIO, atomically, readTVar, writeTVar, STM, modifyTVar)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy (Text, pack)
import Data.Text qualified as StrictText
import Data.Time.Clock

import Lens.Micro.Platform

-- HSX imports: https://hackage.haskell.org/package/ihp-hsx
import IHP.HSX.QQ
import IHP.HSX.ConvertibleStrings () -- <- Helper instancess
import IHP.HSX.ToHtml () -- <- More helper instances
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad.IO.Class (MonadIO)
import Flow
import Control.Monad (forever)
import Debug.Todo (todo)

data AppState = MkAppState
  { _rooms :: Map RoomID Room
  } deriving Show

type RoomID = Text
data Room = MkRoom
  { _leader       :: SessionId
  , _members      :: Set SessionId
  , _votes        :: Map LAN UInt
  , _creationTime :: UTCTime
  } deriving Show

-- Unsigned Int, i.e. only positive integers
type UInt = Word

makeLenses ''Room
makeLenses ''AppState

emptyAppState :: AppState
emptyAppState = MkAppState{ _rooms = Map.empty }

main :: IO ()
main = do
  ----------------
  -- Initialise --
  ----------------

  appStateRef <- newTVarIO emptyAppState
  sessionJar <- createSessionJar

  ----------------------
  -- Periodic Cleanup --
  ----------------------

  maintainSessions sessionJar


  ----------------
  -- Run server --
  ----------------

  scotty 3001 $ do

    ----------------
    -- Middleware --
    ----------------

    middleware loggingMiddleware

    ---------
    -- GET --
    ---------

    -- e.g. `curl -X GET $'http://($localIP):3001/vars'` (nushell string interpolation syntax)
    get "/state" $ do
      appState <- liftIO $ readTVarIO appStateRef
      text (pack $ show appState)


    get "/chess/:roomID" $ do
      roomID <- pathParam @RoomID "roomID"

      session <- getOrCreateSession sessionJar (Just 86400) -- 1 day expirery

      atomicallyMIO $ do
        appState <- readTVar appStateRef
        if appState |> has (rooms . at roomID) then do
          todo "Deal with join or lead logic"
        else do
          writeTVar appStateRef (appState |> rooms %~ Map.insert roomID (todo "initialise room"))


      fenMaybe <- queryParamMaybe @StrictText "fen"

      let fen = case fenMaybe of
                  Just fenText -> fenText
                  Nothing      -> "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

      serveHTML $ chessboardWithFENhtml fen

    -- Chess piece images
    get "/img/chesspieces/wikipedia/:imgName" $ do
      imgName <- pathParam "imgName"
      setHeader "Content-Type" "image/png"
      file ("img/" ++ imgName)

    ----------
    -- POST --
    ----------

    post "/vote/:roomID/:move" $ do
      roomID <- pathParam @RoomID "roomID"
      move <- pathParam "move"
      case parseMaybe lanP move of
        Nothing -> liftIO $ putStrLn ("move invalid: " ++ move)
        Just lanMove -> do
          modifyAppState appStateRef
            (rooms . ix roomID . votes %~ addVote lanMove)
          -- ^ This definition uses 'lenses', which are like
          --   fancy getters and setters. The types are complicated
          --   but using them is more intuitive once you get the hang of them.
          --   You could use record syntax and `Map` functions instead,
          --   but things get a bit cumbersome when the data is deeply nested.
          --   Lenses compose much better, and functional programmers love
          --   when things compose.

          --   The `%~` operator lets us 'focus' in on a subcomponent of a data
          --   structure, using the lenses on the left-hand side,
          --   and modify it with the update function on the right.

          --   The left-hand side of `%~` is composed of multiple
          --   smaller lenses:

          --   `room` focuses in on the `_room` field of `AppState`, i.e.
          --   the `Map RoomID Room`.
          --   `ix roomID` then focuses in on the `Room` corresponding to
          --    the `roomID` key in the `Map`.
          --    `votes` then focuses in on the `_votes` field of `Room`,
          --    which is the thing we want to modify.

          --    If you're interested in learning more, try reading this
          --    quick introductory tutorial: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
          --    Or watching this talk from Simon Peyton Jones:
          --      https://www.youtube.com/watch?v=dxGaKn4REaY


          -- modifyAppState (modifyRoom roomID (modifyVotes (addVote lanMove)))

addVote :: LAN -> Map LAN UInt -> Map LAN UInt
addVote lan voteMap = Map.insertWith (+) lan 1 voteMap

-- modifyRoom :: RoomID -> (Room -> Room) -> AppState -> AppState
-- modifyRoom roomID f appState
--   = appState{ _rooms = Map.adjust f roomID appState.rooms }

-- modifyVotes :: (Map LAN UInt -> Map LAN UInt) -> Room -> Room
-- modifyVotes f room' = room'{_votes = f room'.votes}

modifyAppState :: TVar AppState -> (AppState -> AppState) -> ActionM ()
modifyAppState appStateRef f = atomicallyMIO $ do
  appState <- readTVar appStateRef
  writeTVar appStateRef (f appState)

      -- liftIO $ updateMVar udfMVar (Map.insertWith (+) move 1)

atomicallyMIO :: MonadIO m => STM a -> m a
atomicallyMIO = liftIO . atomically

-- updateMVar :: MVar a -> (a -> a) -> IO ()
-- updateMVar v f = modifyMVar_ v (pure . f)

loggingMiddleware :: Middleware
loggingMiddleware callback = \req respond -> do
  print req
  callback req respond

getOrCreateSession :: SessionJar () -> Maybe Int -> ActionM (Session ())
getOrCreateSession sessionJar expiry = do
  eitherSession <- getUserSession sessionJar
  case eitherSession of
    Right session -> pure session
    Left  _       -> createUserSession sessionJar expiry ()

-- Adapted from maintainSessions code:
maintainAppState :: TVar AppState -> IO ()
maintainAppState appStateRef =
  forever $ do
    now <- getCurrentTime
    let stillValid sess = sessExpiresAt sess > now
    atomically $ modifyTVar appStateRef $ \state -> todo "filter expired sessions out of state"
    threadDelay 3600000000 -- 1 hour

---------
-- HSX --
---------

-- It's like React's JSX, but for Haskell. It's a way of writing HTML templates
-- Install the 'Haskell HSX' VSCode extension to get the correct HTML syntax highlighting

serveHTML :: Html -> ActionM ()
serveHTML htmlCode = html (renderHtml htmlCode)

type StrictText = StrictText.Text

chessboardWithFENhtml :: StrictText -> Html
chessboardWithFENhtml fenStr = [hsx|
  <!DOCTYPE html>
  <html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
    <title>Chess</title>
    <link rel="stylesheet" href="https://unpkg.com/@chrisoakman/chessboardjs@1.0.0/dist/chessboard-1.0.0.min.css" integrity="sha384-q94+BZtLrkL1/ohfjR8c6L+A6qzNH9R2hBLwyoAfu3i/WCvQjzL2RQJ3uNHDISdU" crossorigin="anonymous">

  </head>
  <body>
    <h1>Chess!</h1>

    <div id="myBoard" style="width: 400px;"></div>
    <script src="https://code.jquery.com/jquery-3.5.1.min.js"
            integrity="sha384-ZvpUoO/+PpLXR1lu4jmpXWu80pZlYUAfxl5NsBMWOEPSjUn/6Z/hRTt8+pR6L4N2"
            crossorigin="anonymous"></script>

    <script src="https://unpkg.com/@chrisoakman/chessboardjs@1.0.0/dist/chessboard-1.0.0.min.js"
            integrity="sha384-8Vi8VHwn3vjQ9eUHUxex3JSN/NFqUg3QbPyX8kWyb93+8AC/pPWTzj+nHtbC5bxD"
            crossorigin="anonymous"></script>



    <script data-fen-str={fenStr}>
      const fenStr = document.currentScript.dataset.fenStr

      console.log('fenStr: ' + fenStr)

      // --- Begin Example JS --------------------------------------------------------
      function onDrop (source, target, piece, newPos, oldPos, orientation) {
        jQuery.post('/vote/' + source + target)
      }

      var config = {
        draggable: true,
        position: fenStr,
        onDrop: onDrop,
        sparePieces: true,
        showNotation: true
      }

      console.log(config)
      var board = Chessboard('myBoard', config)
    // --- End Example JS ----------------------------------------------------------
    </script>

  </body>
  </html>
|]