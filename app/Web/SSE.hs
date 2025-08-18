{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.SSE where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Concurrent.STM
import qualified Data.Aeson as A
import Data.ByteString.Builder (lazyByteString)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.UUID (fromText)
import Game.Types
import Network.Wai (Application, pathInfo)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Room.Runtime

-- Helper: JSON encode an event
encodeEvent :: RoomEvent -> A.Value
encodeEvent ev = case ev of
  PlayerJoined pid nm -> A.object ["type" A..= ("player_joined" :: T.Text), "pid" A..= unPid pid, "name" A..= nm]
  PlayerLeft pid -> A.object ["type" A..= ("player_left" :: T.Text), "pid" A..= unPid pid]
  PhaseChanged ph -> A.object ["type" A..= ("phase_changed" :: T.Text), "phase" A..= show ph]
  RoundStart n -> A.object ["type" A..= ("round_start" :: T.Text), "round" A..= n]
  RoundOver n ps -> A.object ["type" A..= ("round_over" :: T.Text), "round" A..= n, "survivors" A..= map unPid ps]
  ScoreUpdate xs -> A.object ["type" A..= ("score_update" :: T.Text), "score" A..= [[unPid p, s] | (p, s) <- xs]]

roomSSEApp :: LobbyState -> Application
roomSSEApp LobbyState {..} req send = do
  -- Path is /rooms/:rid/sse (already routed in Main)
  let parts = pathInfo req
  case parts of
    ["rooms", ridTxt, "sse"] ->
      case fromText ridTxt of
        Nothing -> eventSourceAppIO (pure (ServerEvent (Just "error") Nothing ["bad room id"])) req send
        Just rid -> do
          rms <- readTVarIO lsRooms
          case M.lookup (RoomId rid) rms of
            Nothing -> eventSourceAppIO (pure (ServerEvent (Just "error") Nothing ["room not found"])) req send
            Just rg -> do
              -- Duplicate the event channel so this stream has its own cursor
              ch <- atomically $ dupTChan (rgEvents rg)
              -- Send an initial snapshot (clients + phase + round)
              snapshot <- atomically $ do
                cls <- clientsSnapshot rg
                ph <- readTVar (rgPhase rg)
                rn <- readTVar (rgRound rg)
                pure $
                  A.encode $
                    A.object
                      [ "type" A..= ("snapshot" :: T.Text),
                        "phase" A..= show ph,
                        "round" A..= rn,
                        "clients" A..= [A.object ["pid" A..= unPid p, "name" A..= nm] | (p, nm) <- cls]
                      ]
              let firstEvent = ServerEvent {eventName = Just "snapshot", eventId = Nothing, eventData = [lazyByteString snapshot]}
              -- Bridge TChan RoomEvent -> SSE stream
              let pump = do
                    ev <- atomically $ readTChan ch
                    pure $ ServerEvent (Just "room") Nothing [lazyByteString $ A.encode (encodeEvent ev)]
              sentVar <- newMVar False
              let next = do
                    first <- modifyMVar sentVar (\s -> pure (True, not s))
                    if first then pure firstEvent else pump
              eventSourceAppIO next req send
    _ -> eventSourceAppIO (pure (ServerEvent (Just "error") Nothing ["bad path"])) req send
