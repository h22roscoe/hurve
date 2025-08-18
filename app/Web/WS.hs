{-# LANGUAGE OverloadedStrings #-}

module Web.WS where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Text (Text)
import qualified Data.Vector as V
import Game.Types hiding (pid)
import qualified Network.WebSockets as WS
import Room.Runtime

-- Attach a single socket to a room, registering the player and streaming states.
roomWebSocket :: RoomGame -> PlayerId -> Text -> WS.ServerApp
roomWebSocket rg pid name pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 25 (pure ()) $ do
    -- register client
    outQ <- newTBQueueIO 128
    aliveVar <- newTVarIO True
    let client = Client pid name outQ aliveVar
    atomically $ addClient rg client
    -- welcome
    w0 <- readTVarIO (rgWorld rg)
    WS.sendTextData conn (A.encode [A.String "welcome", A.toJSON (unPid pid), worldObj w0])

    let reader = forever $ do
          msg <- WS.receiveData conn
          case A.decode msg of
            Just (A.Array arr) | length arr >= 4 ->
              case (arr V.!? 0, arr V.!? 1, arr V.!? 2, arr V.!? 3) of
                (Just t, Just _seqV, Just _pidV, Just turnV) | t == A.String "input" ->
                  case AT.parseMaybe A.parseJSON turnV of
                    Just trn -> atomically $ writeTChan (rgInputs rg) (pid, trn)
                    _ -> pure ()
                _ -> pure ()
            _ -> pure ()
        writer = forever $ do
          bs <- atomically $ readTBQueue outQ
          WS.sendTextData conn bs
    finally
      (race_ reader writer)
      (atomically (removeClient rg pid))
