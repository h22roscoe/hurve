{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lobby.Server where

import Control.Concurrent.STM
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Data.Unique (hashUnique, newUnique)
import Game.Types
import Lobby.API
import Network.Wai.Application.Static (StaticSettings, defaultWebAppSettings, ssIndices, ssRedirectToIndex)
import Network.Wai.Middleware.Cors (simpleCors)
import Room.Runtime
import Servant
import WaiAppStatic.Types (unsafeToPiece)

lobbyApi :: Proxy LobbyAPI
lobbyApi = Proxy

staticSettings :: StaticSettings
staticSettings =
  (defaultWebAppSettings "static")
    { ssIndices = [unsafeToPiece "index.html"]
    , ssRedirectToIndex = True
    }

server :: LobbyState -> Server LobbyAPI
server st = createRoomH st :<|> listRoomsH st :<|> joinRoomH st :<|> readyH st :<|> deleteH st :<|> serveDirectoryWith staticSettings

mkApp :: LobbyState -> Application
mkApp st = simpleCors $ serve lobbyApi (server st)

createRoomH :: LobbyState -> CreateRoomReq -> Handler CreateRoomRes
createRoomH LobbyState{..} CreateRoomReq{} = do
  rid <- RoomId <$> liftIO nextRandom
  rg <- liftIO (startRoomGame rid >>= flip launchTick 60)
  liftIO . atomically $ do
    modifyTVar' lsRooms (M.insert rid rg)
    writeTChan lsLobbyEvents (LobbyRoomUpsert rid 0 Waiting)
  let ws = T.pack ("ws://localhost:9160/ws/" <> show rid)
  pure CreateRoomRes{roomId = rid, wsUrl = ws}

listRoomsH :: LobbyState -> Handler [RoomMeta]
listRoomsH LobbyState{..} = do
  rms <- liftIO $ readTVarIO lsRooms
  liftIO $ forM (M.toList rms) $ \(rid, rg) -> atomically $ do
    clients <- readTVar (rgClients rg)
    ph <- readTVar (rgPhase rg)
    let names = map (cName . snd) (M.toList clients)
    pure
      RoomMeta
        { roomId = rid
        , name = "Room"
        , maxPlayers = 8
        , isPrivate = False
        , status = T.pack (show ph)
        , players = names
        }

joinRoomH :: LobbyState -> RoomId -> JoinReq -> Handler JoinRes
joinRoomH LobbyState{..} rid JoinReq{..} = do
  rms <- liftIO $ readTVarIO lsRooms
  case M.lookup rid rms of
    Nothing -> throwError err404
    Just _ -> do
      tok <- JoinToken <$> liftIO nextRandom
      pid <- hashUnique <$> liftIO newUnique
      liftIO . atomically $ modifyTVar' lsTokens (M.insert tok (rid, PlayerId pid, displayName))
      let ws = T.pack ("ws://localhost:9160/ws/" <> show rid <> "/" <> show tok)
      pure JoinRes{token = tok, pid = pid, wsUrl = ws}

readyH :: LobbyState -> RoomId -> ReadyReq -> Handler NoContent
readyH LobbyState{..} rid ReadyReq{..} = do
  rms <- liftIO $ readTVarIO lsRooms
  case M.lookup rid rms of
    Nothing -> throwError err404
    Just rg -> do
      mt <- liftIO . atomically $ do
        m <- readTVar lsTokens
        pure (M.lookup token m)
      case mt of
        Just (rid', pid, _nm) | rid' == rid -> do
          liftIO . atomically $ do
            cs <- readTVar (rgClients rg)
            when (M.member pid cs) $ do
              modifyTVar' (rgReady rg) (M.insert pid ready)
              writeTChan (rgEvents rg) (ReadyChanged pid ready)
              q <- hasQuorum rg
              ph <- readTVar (rgPhase rg)
              when (q && ph == Waiting) $ do
                w <- readTVar (rgWorld rg)
                let deadline = tick w + 180
                writeTVar (rgPhase rg) (Countdown deadline)
                writeTChan (rgEvents rg) (PhaseChanged (Countdown deadline))
          pure NoContent
        _ -> throwError err403

deleteH :: LobbyState -> RoomId -> Handler NoContent
deleteH LobbyState{..} rid = do
  mrt <- liftIO . atomically $ do
    rms <- readTVar lsRooms
    writeTVar lsRooms (M.delete rid rms)
    writeTChan lsLobbyEvents (LobbyRoomRemoved rid)
    pure (M.lookup rid rms)
  case mrt of
    Nothing -> pure NoContent
    Just rg -> liftIO (stopRoom rg) >> pure NoContent
