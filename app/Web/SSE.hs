{-# LANGUAGE OverloadedStrings #-}

module Web.SSE where

import Network.Wai (Application)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Room.Runtime

roomSSEApp :: LobbyState -> Application
roomSSEApp _st req send = do
  -- path already matched in Main; find room & stream from its event bus
  -- For brevity, this demo sends a trivial heartbeat stream.
  eventSourceAppIO (pure (ServerEvent (Just "room") Nothing ["\n"])) req send