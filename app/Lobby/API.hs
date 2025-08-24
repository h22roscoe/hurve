{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Lobby.API where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Room.Runtime (JoinToken (..), RoomId (..))
import Servant.API

data CreateRoomReq = CreateRoomReq {name :: Text, maxPlayers :: Int, isPrivate :: Bool} deriving (Show, Generic)

instance FromJSON CreateRoomReq

data CreateRoomRes = CreateRoomRes {roomId :: RoomId, wsUrl :: Text} deriving (Show, Generic)

instance ToJSON CreateRoomRes

data RoomMeta = RoomMeta {roomId :: RoomId, name :: Text, maxPlayers :: Int, isPrivate :: Bool, status :: Text, players :: [Text]} deriving (Show, Generic)

instance ToJSON RoomMeta

newtype JoinReq = JoinReq {displayName :: Text} deriving (Show, Generic)

instance FromJSON JoinReq

data JoinRes = JoinRes {token :: JoinToken, pid :: Int, wsUrl :: Text} deriving (Show, Generic)

instance ToJSON JoinRes

data ReadyReq = ReadyReq { token :: JoinToken, ready :: Bool } deriving (Generic, Show)

instance FromJSON ReadyReq

type LobbyAPI =
  "rooms" :> ReqBody '[JSON] CreateRoomReq :> Post '[JSON] CreateRoomRes
    :<|> "rooms" :> Get '[JSON] [RoomMeta]
    :<|> "rooms" :> Capture "roomId" RoomId :> "join" :> ReqBody '[JSON] JoinReq :> Post '[JSON] JoinRes
    :<|> "rooms" :> Capture "roomId" RoomId :> "ready" :> ReqBody '[JSON] ReadyReq :> PostNoContent
    :<|> "rooms" :> Capture "roomId" RoomId :> DeleteNoContent
    :<|> Raw
