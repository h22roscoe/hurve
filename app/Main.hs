{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Lobby.Server (mkApp)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Room.Runtime
import Web.SSE (roomSSEApp)
import Web.WS (roomWebSocket)

main :: IO ()
main = do
  st <- newLobbyState
  let httpApp = mkApp st -- Servant REST + static file
      wsOpts = WS.defaultConnectionOptions
      port = 9160
  putStrLn ("HTTP(SSE)+WS listening on :" <> show port)
  run port $ websocketsOr wsOpts (wsRouter st) (withSSE st httpApp)

-- Insert SSE under same Warp: delegate to SSE app when path matches, else REST
withSSE :: LobbyState -> Application -> Application
withSSE st rest req send = do
  let p = pathInfo req
  case p of
    ["rooms", _ridTxt, "sse"] -> roomSSEApp st req send
    _ -> rest req send

wsRouter :: LobbyState -> WS.ServerApp
wsRouter st pending = do
  let preq = WS.pendingRequest pending
      path = WS.requestPath preq -- e.g. "/ws/<roomId>/<token>"
  case parseWsTarget path of
    Left err -> WS.rejectRequest pending (TE.encodeUtf8 err)
    Right (rid, tok) -> do
      ok <- atomically $ do
        m <- readTVar (lsTokens st)
        pure (M.lookup tok m)
      case ok of
        Nothing -> WS.rejectRequest pending "invalid token"
        Just (rid', _pid, _name) | rid' /= rid -> WS.rejectRequest pending "token-room mismatch"
        Just (_, pid, name) -> do
          rms <- readTVarIO (lsRooms st)
          case M.lookup rid rms of
            Nothing -> WS.rejectRequest pending "room not running"
            Just rt -> roomWebSocket rt pid name pending

parseWsTarget :: ByteString -> Either Text (RoomId, JoinToken)
parseWsTarget rawPath =
  case T.splitOn "/" (TE.decodeUtf8 rawPath) of
    ["", "ws", ridTxt, tokTxt] -> do
      rid <- maybe (Left "bad room id") (Right . RoomId) (UUID.fromText ridTxt)
      tok <- maybe (Left "bad token") (Right . JoinToken) (UUID.fromText tokTxt)
      Right (rid, tok)
    _ -> Left "bad path (expected /ws/<roomId>/<token>)"
