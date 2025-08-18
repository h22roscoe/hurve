{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Room.Runtime where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Monad (void, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Game.Engine
import Game.Types
import Servant.API
import Unsafe.Coerce (unsafeCoerce)

-- Phases & events

data Phase = Waiting | Countdown Time | Playing | BetweenRounds Time | Finished deriving (Show, Eq)

data RoomEvent
  = PlayerJoined PlayerId Text
  | PlayerLeft PlayerId
  | PhaseChanged Phase
  | RoundStart Int
  | RoundOver Int [PlayerId]
  | ScoreUpdate [(PlayerId, Int)]
  deriving (Show)

-- Client and runtime

data Client = Client {cPid :: !PlayerId, cName :: !Text, cOut :: TBQueue BL, cConnAlive :: TVar Bool}

type BL = LBS.ByteString

-- RoomGame holds game state and plumbing

data RoomGame = RoomGame
  { rgWorld :: TVar World,
    rgPhase :: TVar Phase,
    rgRound :: TVar Int,
    rgScore :: TVar (M.Map PlayerId Int),
    rgInputs :: TChan (PlayerId, Turn),
    rgClients :: TVar (M.Map PlayerId Client),
    rgEvents :: TChan RoomEvent,
    rgTickThr :: Async ()
  }

newtype RoomId = RoomId UUID.UUID deriving (Eq, Ord, Show)

instance A.ToJSON RoomId where toJSON (RoomId u) = A.toJSON (UUID.toText u)

instance A.FromJSON RoomId where parseJSON = A.withText "RoomId" $ \t -> maybe (fail "bad room id") (pure . RoomId) (UUID.fromText t)

instance FromHttpApiData RoomId where
  parseUrlPiece t =
    maybe (Left "bad room id") (Right . RoomId) (UUID.fromText t)

instance ToHttpApiData RoomId where
  toUrlPiece (RoomId u) = UUID.toText u

newtype JoinToken = JoinToken UUID.UUID deriving (Eq, Ord, Show)

instance A.ToJSON JoinToken where toJSON (JoinToken u) = A.toJSON (UUID.toText u)

instance FromHttpApiData JoinToken where
  parseUrlPiece t =
    maybe (Left "bad token") (Right . JoinToken) (UUID.fromText t)

instance ToHttpApiData JoinToken where
  toUrlPiece (JoinToken u) = UUID.toText u

data LobbyState = LobbyState
  { lsRooms :: TVar (M.Map RoomId RoomGame),
    lsTokens :: TVar (M.Map JoinToken (RoomId, PlayerId, Text))
  }

newLobbyState :: IO LobbyState
newLobbyState = atomically $ LobbyState <$> newTVar M.empty <*> newTVar M.empty

startRoomGame :: IO RoomGame
startRoomGame = do
  w <- initialWorld
  atomically $ do
    rgWorld <- newTVar w
    rgPhase <- newTVar Waiting
    rgRound <- newTVar 0
    rgScore <- newTVar M.empty
    rgInputs <- newTChan
    rgClients <- newTVar M.empty
    rgEvents <- newTChan
    pure RoomGame {rgWorld, rgPhase, rgRound, rgScore, rgInputs, rgClients, rgEvents, rgTickThr = unsafeCoerce ()}

launchTick :: RoomGame -> Int -> IO RoomGame
launchTick rg hz = do
  thr <- async (tickLoop rg hz)
  pure rg {rgTickThr = thr}

stopRoom :: RoomGame -> IO ()
stopRoom RoomGame {..} = cancel rgTickThr

-- Attach/detach clients
addClient :: RoomGame -> Client -> STM ()
addClient RoomGame {..} c = modifyTVar' rgClients (M.insert (cPid c) c)

removeClient :: RoomGame -> PlayerId -> STM ()
removeClient RoomGame {..} p = modifyTVar' rgClients (M.delete p)

-- Tick loop with simple phase machine

tickLoop :: RoomGame -> Int -> IO ()
tickLoop rg@RoomGame {..} hz = go
  where
    us = 1000000 `div` hz
    go = do
      ph <- readTVarIO rgPhase
      case ph of
        Waiting -> pure ()
        Countdown endT -> do
          w <- readTVarIO rgWorld
          when (tick w >= endT) $ atomically $ do
            writeTVar rgPhase Playing
            writeTChan rgEvents (PhaseChanged Playing)
        Playing -> do
          ins <- atomically $ flush rgInputs
          w0 <- readTVarIO rgWorld
          let (w1, _dAdd, didReset, _dRem, survivors) = stepWorld ins w0
          atomically $ writeTVar rgWorld w1
          when didReset $ atomically $ do
            rn <- stateTVar rgRound (\n -> (n + 1, n + 1))
            -- award winner if exactly one survivor
            sc <- readTVar rgScore
            let sc' = case survivors of
                  [winner] -> M.insertWith (+) winner 1 sc
                  _ -> sc
            writeTVar rgScore sc'
            writeTVar rgPhase (BetweenRounds (tick w1 + 90)) -- ~1.5s at 60Hz
            writeTChan rgEvents (RoundOver rn survivors)
            writeTChan rgEvents (ScoreUpdate (M.toList sc'))
        BetweenRounds endT -> do
          w <- readTVarIO rgWorld
          when (tick w >= endT) $ atomically $ do
            modifyTVar' rgWorld resetWorld
            rn <- readTVar rgRound
            writeTVar rgPhase (Countdown (tick w + 180)) -- 3s
            writeTChan rgEvents (RoundStart (rn + 1))
            writeTChan rgEvents (PhaseChanged (Countdown (tick w + 180)))
        Finished -> pure ()

      -- broadcast state to WS clients
      broadcastState rg
      threadDelay us
      go

flush :: TChan a -> STM [a]
flush ch = do
  e <- isEmptyTChan ch
  if e then pure [] else (:) <$> readTChan ch <*> flush ch

-- Broadcast current state to all WS clients (now here to avoid cycles)
broadcastState :: RoomGame -> IO ()
broadcastState RoomGame {..} = do
  w <- readTVarIO rgWorld
  cs <- readTVarIO rgClients
  sc <- readTVarIO rgScore
  ph <- readTVarIO rgPhase
  rn <- readTVarIO rgRound
  let payload =
        A.encode
          [ A.String "state",
            A.toJSON (tick w),
            A.object
              [ "world" A..= worldObj w,
                "score" A..= [[A.toJSON (unPid p), A.toJSON s] | (p, s) <- M.toList sc],
                "phase" A..= show ph,
                "round" A..= rn
              ]
          ]
  atomically $ mapM_ (\c -> void $ writeTBQueue (cOut c) payload) (M.elems cs)

-- Local copy of worldObj to keep Runtime self-contained
worldObj :: World -> A.Value
worldObj w =
  A.object
    [ "tick" A..= tick w,
      "width" A..= width w,
      "height" A..= height w,
      "speed" A..= speed w,
      "turnRate" A..= turnRate w,
      "headRadiusPx" A..= headRadiusPx w,
      "tailRadiusPx" A..= tailRadiusPx w,
      "players" A..= players w,
      "trails" A..= map (\(x, y) -> A.toJSON [x, y]) (S.toList (trails w))
    ]