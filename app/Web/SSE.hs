{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.SSE where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Concurrent.STM
import Control.Monad (forM)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.ByteString.Builder (lazyByteString)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.UUID (fromText)
import Game.Types
import Network.HTTP.Types (status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Room.Runtime

-- Helper: JSON encode an event
encodeEvent :: RoomEvent -> A.Value
encodeEvent ev = case ev of
  PlayerJoined pid nm -> A.object ["type" .= ("player_joined" :: T.Text), "pid" .= unPid pid, "name" .= nm]
  PlayerLeft pid -> A.object ["type" .= ("player_left" :: T.Text), "pid" .= unPid pid]
  ReadyChanged pid r -> A.object ["type" .= ("ready_changed" :: T.Text), "pid" .= unPid pid, "ready" .= r]
  PhaseChanged ph -> A.object ["type" .= ("phase_changed" :: T.Text), "phase" .= show ph]
  RoundStart n -> A.object ["type" .= ("round_start" :: T.Text), "round" .= n]
  RoundOver n ps -> A.object ["type" .= ("round_over" :: T.Text), "round" .= n, "survivors" .= map unPid ps]
  ScoreUpdate xs -> A.object ["type" .= ("score_update" :: T.Text), "score" .= [[unPid p, s] | (p, s) <- xs]]

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
                rmap <- readTVar (rgReady rg)
                pure $
                  A.encode $
                    A.object
                      [ "type" .= ("snapshot" :: T.Text),
                        "phase" .= show ph,
                        "round" .= rn,
                        "clients" .= [A.object ["pid" .= unPid p, "name" .= nm, "ready" .= M.findWithDefault False p rmap] | (p, nm) <- cls]
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

lobbySSEApp :: LobbyState -> Application
lobbySSEApp st req send = do
  case pathInfo req of
    ["rooms", "sse"] -> do
      -- initial snapshot
      snapshot <- atomically $ do
        rms <- readTVar (lsRooms st)
        forM (M.toList rms) $ \(rid, rg) -> do
          cs <- readTVar (rgClients rg)
          ph <- readTVar (rgPhase rg)
          pure $
            A.object
              [ "roomId" .= rid,
                "players" .= M.size cs,
                "phase" .= show ph
              ]
      let firstEvent = ServerEvent (Just "snapshot") Nothing [lazyByteString $ A.encode snapshot]
      -- event pump
      ch <- atomically $ dupTChan (lsLobbyEvents st)
      let pump = do
            ev <- atomically $ readTChan ch
            let v = case ev of
                  LobbyRoomUpsert r n ph ->
                    A.object
                      [ "type" .= ("upsert" :: T.Text),
                        "roomId" .= r,
                        "players" .= n,
                        "phase" .= show ph
                      ]
                  LobbyRoomRemoved r ->
                    A.object
                      [ "type" .= ("removed" :: T.Text),
                        "roomId" .= r
                      ]
            pure (ServerEvent (Just "room") Nothing [lazyByteString $ A.encode v])
      sentVar <- newMVar False
      let next = do
            first <- modifyMVar sentVar (\s -> pure (True, not s))
            if first then pure firstEvent else pump
      eventSourceAppIO next req send
    _ -> send $ responseLBS status404 [] "not found"
