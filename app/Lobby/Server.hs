{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lobby.Server where

import Control.Concurrent.STM
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Data.Unique (hashUnique, newUnique)
import Game.Types
import Lobby.API
import Network.Wai.Middleware.Cors (simpleCors)
import Room.Runtime
import Servant

lobbyApi :: Proxy LobbyAPI
lobbyApi = Proxy

server :: LobbyState -> Server LobbyAPI
server st = createRoomH st :<|> listRoomsH st :<|> joinRoomH st :<|> startH st :<|> deleteH st :<|> serveDirectoryWebApp "static"

mkApp :: LobbyState -> Application
mkApp st = simpleCors $ serve lobbyApi (server st)

createRoomH :: LobbyState -> CreateRoomReq -> Handler CreateRoomRes
createRoomH LobbyState {..} CreateRoomReq {} = do
  rid <- RoomId <$> liftIO nextRandom
  rg <- liftIO (startRoomGame rid >>= flip launchTick 60)
  liftIO . atomically $ do
    modifyTVar' lsRooms (M.insert rid rg)
    writeTChan lsLobbyEvents (LobbyRoomUpsert rid 0 Waiting)
  let ws = T.pack ("ws://localhost:9160/ws/" <> show rid)
  pure CreateRoomRes {roomId = rid, wsUrl = ws}

listRoomsH :: LobbyState -> Handler [RoomMeta]
listRoomsH LobbyState {..} = do
  rms <- liftIO $ readTVarIO lsRooms
  liftIO $ forM (M.toList rms) $ \(rid, rg) -> atomically $ do
    clients <- readTVar (rgClients rg)
    ph <- readTVar (rgPhase rg)
    let names = map (cName . snd) (M.toList clients)
    pure
      RoomMeta
        { roomId = rid,
          name = "Room",
          maxPlayers = 8,
          isPrivate = False,
          status = T.pack (show ph),
          players = names
        }

joinRoomH :: LobbyState -> RoomId -> JoinReq -> Handler JoinRes
joinRoomH LobbyState {..} rid JoinReq {..} = do
  rms <- liftIO $ readTVarIO lsRooms
  case M.lookup rid rms of
    Nothing -> throwError err404
    Just _ -> do
      tok <- JoinToken <$> liftIO nextRandom
      pid <- hashUnique <$> liftIO newUnique
      liftIO . atomically $ modifyTVar' lsTokens (M.insert tok (rid, PlayerId pid, displayName))
      let ws = T.pack ("ws://localhost:9160/ws/" <> show rid <> "/" <> show tok)
      pure JoinRes {token = tok, pid = pid, wsUrl = ws}

startH :: LobbyState -> RoomId -> Handler NoContent
startH LobbyState {..} rid = do
  rms <- liftIO $ readTVarIO lsRooms
  case M.lookup rid rms of
    Nothing -> throwError err404
    Just rg -> liftIO . atomically $ do
      w <- readTVar (rgWorld rg)
      let deadline = tick w + 120 -- ~2s at 60Hz
      writeTVar (rgPhase rg) (Countdown deadline)
      writeTChan (rgEvents rg) (PhaseChanged (Countdown deadline))
  pure NoContent

deleteH :: LobbyState -> RoomId -> Handler NoContent
deleteH LobbyState {..} rid = do
  mrt <- liftIO . atomically $ do
    rms <- readTVar lsRooms
    writeTVar lsRooms (M.delete rid rms)
    writeTChan lsLobbyEvents (LobbyRoomRemoved rid)
    pure (M.lookup rid rms)
  case mrt of
    Nothing -> pure NoContent
    Just rg -> liftIO (stopRoom rg) >> pure NoContent
