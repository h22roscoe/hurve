{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lobby.Server where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
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
  rg <- liftIO (startRoomGame >>= flip launchTick 60)
  liftIO . atomically $ modifyTVar' lsRooms (M.insert rid rg)
  let ws = T.pack ("ws://localhost:9160/ws/" <> show rid)
  pure CreateRoomRes {roomId = rid, wsUrl = ws}

listRoomsH :: LobbyState -> Handler [RoomMeta]
listRoomsH LobbyState {..} = do
  rms <- liftIO $ readTVarIO lsRooms
  pure
    [ RoomMeta {roomId = rid, name = "Room", maxPlayers = 8, isPrivate = False, status = "Waiting", players = []}
      | (rid, _) <- M.toList rms
    ]

joinRoomH :: LobbyState -> RoomId -> JoinReq -> Handler JoinRes
joinRoomH LobbyState {..} rid JoinReq {..} = do
  rms <- liftIO $ readTVarIO lsRooms
  case M.lookup rid rms of
    Nothing -> throwError err404
    Just _ -> do
      tok <- JoinToken <$> liftIO nextRandom
      let pid = 1 -- naive demo: client sends its pid later; real impl allocates free id here
      liftIO . atomically $ modifyTVar' lsTokens (M.insert tok (rid, PlayerId pid, displayName))
      let ws = T.pack ("ws://localhost:9160/ws/" <> show rid <> "/" <> show tok)
      pure JoinRes {token = tok, pid = pid, wsUrl = ws}

startH :: LobbyState -> RoomId -> Handler NoContent
startH _ _ = pure NoContent

deleteH :: LobbyState -> RoomId -> Handler NoContent
deleteH LobbyState {..} rid = do
  mrt <- liftIO . atomically $ do
    rms <- readTVar lsRooms
    writeTVar lsRooms (M.delete rid rms)
    pure (M.lookup rid rms)
  case mrt of
    Nothing -> pure NoContent
    Just rg -> liftIO (stopRoom rg) >> pure NoContent
