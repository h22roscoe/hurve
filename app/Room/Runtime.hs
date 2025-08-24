{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Room.Runtime where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Monad (void, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
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
  | ReadyChanged PlayerId Bool
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
  { rgId :: RoomId,
    rgWorld :: TVar World,
    rgPhase :: TVar Phase,
    rgRound :: TVar Int,
    rgRoundStart :: TVar Time,
    rgScore :: TVar (M.Map PlayerId Int),
    rgInputs :: TChan (PlayerId, Turn),
    rgClients :: TVar (M.Map PlayerId Client),
    rgReady :: TVar (M.Map PlayerId Bool),
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

instance A.FromJSON JoinToken where parseJSON = A.withText "JoinToken" $ \t -> maybe (fail "bad join token") (pure . JoinToken) (UUID.fromText t)

instance FromHttpApiData JoinToken where
  parseUrlPiece t =
    maybe (Left "bad token") (Right . JoinToken) (UUID.fromText t)

instance ToHttpApiData JoinToken where
  toUrlPiece (JoinToken u) = UUID.toText u

data LobbyEvent
  = LobbyRoomUpsert {leRoom :: RoomId, lePlayers :: Int, lePhase :: Phase}
  | LobbyRoomRemoved {leRoom :: RoomId}

data LobbyState = LobbyState
  { lsRooms :: TVar (M.Map RoomId RoomGame),
    lsTokens :: TVar (M.Map JoinToken (RoomId, PlayerId, Text)),
    lsLobbyEvents :: TChan LobbyEvent
  }

newLobbyState :: IO LobbyState
newLobbyState = atomically $ LobbyState <$> newTVar M.empty <*> newTVar M.empty <*> newTChan

startRoomGame :: RoomId -> IO RoomGame
startRoomGame rid = do
  w <- initialWorld
  atomically $ do
    rgWorld <- newTVar w
    rgPhase <- newTVar Waiting
    rgRound <- newTVar 0
    rgRoundStart <- newTVar 0
    rgScore <- newTVar M.empty
    rgInputs <- newTChan
    rgClients <- newTVar M.empty
    rgReady <- newTVar M.empty
    rgEvents <- newTChan
    pure RoomGame {rgId = rid, rgWorld, rgPhase, rgRound, rgRoundStart, rgScore, rgInputs, rgClients, rgReady, rgEvents, rgTickThr = unsafeCoerce ()}

launchTick :: RoomGame -> Int -> IO RoomGame
launchTick rg hz = do
  thr <- async (tickLoop rg hz)
  pure rg {rgTickThr = thr}

stopRoom :: RoomGame -> IO ()
stopRoom RoomGame {..} = cancel rgTickThr

-- Attach/detach clients: EMIT EVENTS
addClient :: LobbyState -> RoomGame -> Client -> STM ()
addClient st RoomGame {..} c = do
  modifyTVar' rgClients (M.insert (cPid c) c)
  modifyTVar' rgReady (M.insert (cPid c) False)
  writeTChan rgEvents (PlayerJoined (cPid c) (cName c))
  cs <- readTVar rgClients
  ph <- readTVar rgPhase
  writeTChan (lsLobbyEvents st) (LobbyRoomUpsert rgId (M.size cs) ph)

removeClient :: LobbyState -> RoomGame -> PlayerId -> STM ()
removeClient st RoomGame {..} p = do
  modifyTVar' rgClients (M.delete p)
  modifyTVar' rgReady (M.delete p)
  writeTChan rgEvents (PlayerLeft p)
  cs <- readTVar rgClients
  ph <- readTVar rgPhase
  writeTChan (lsLobbyEvents st) (LobbyRoomUpsert rgId (M.size cs) ph)

-- Utility

clientsSnapshot :: RoomGame -> STM [(PlayerId, Text)]
clientsSnapshot RoomGame {..} = do
  cs <- readTVar rgClients
  pure [(cPid c, cName c) | c <- M.elems cs]

readyCount :: RoomGame -> STM (Int, Int) -- (ready, totalConnected)
readyCount RoomGame {..} = do
  cs <- readTVar rgClients
  rmap <- readTVar rgReady
  let total = M.size cs
      ready = length [() | (pid, True) <- M.toList rmap, M.member pid cs]
  pure (ready, total)

hasQuorum :: RoomGame -> STM Bool
hasQuorum rg = do
  (r, t) <- readyCount rg
  pure (t >= 1 && r * 2 > t)

-- Tick loop with simple phase machine
tickLoop :: RoomGame -> Int -> IO ()
tickLoop rg@RoomGame {..} hz = go
  where
    us = 1000000 `div` hz
    go = do
      -- advance a wall-clock-ish tick every loop so Countdown/BetweenRounds can elapse
      atomically $ modifyTVar' rgWorld (\w -> w {tick = tick w + 1})

      ph <- readTVarIO rgPhase
      case ph of
        Waiting -> pure ()
        Countdown endT -> do
          w <- readTVarIO rgWorld
          when (tick w >= endT) $ atomically $ do
            writeTVar rgRoundStart (tick w) -- remember when the round starts
            writeTVar rgPhase Playing
            writeTChan rgEvents (PhaseChanged Playing)
        Playing -> do
          ins <- atomically $ flush rgInputs
          w0 <- readTVarIO rgWorld
          startAt <- readTVarIO rgRoundStart
          let now = tick w0
          let graceTicks = 10 -- Ticks before
          let noCollideUntil = startAt + graceTicks
          let (w1, _dAdd, didReset, _dRem, survivors) = stepWorld now noCollideUntil ins w0
          atomically $ writeTVar rgWorld w1
          when didReset $ atomically $ do
            rn <- stateTVar rgRound (\n -> (n + 1, n + 1))
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
          clients <- readTVarIO rgClients
          when (tick w >= endT) $ atomically $ do
            modifyTVar' rgWorld (resetWorld $ M.keys clients)
            rn <- readTVar rgRound
            let tNow = tick w
            writeTVar rgPhase (Countdown (tNow + 180)) -- 3s at 60Hz
            writeTChan rgEvents (RoundStart (rn + 1))
            writeTChan rgEvents (PhaseChanged (Countdown (tNow + 180)))
        Finished -> pure ()

      broadcastState rg
      threadDelay us
      go

flush :: TChan a -> STM [a]
flush ch = do
  e <- isEmptyTChan ch
  if e then pure [] else (:) <$> readTChan ch <*> flush ch

-- Broadcast current state to all WS clients (now here to avoid cycles)
broadcastState :: RoomGame -> IO ()
broadcastState rg@RoomGame {..} = do
  w <- readTVarIO rgWorld
  cs <- readTVarIO rgClients
  sc <- readTVarIO rgScore
  ph <- readTVarIO rgPhase
  rn <- readTVarIO rgRound
  cls <- atomically (clientsSnapshot rg)
  let payload =
        A.encode
          [ A.String "state",
            A.toJSON (tick w),
            A.object
              [ "world" .= worldObj w,
                "score" .= [[A.toJSON (unPid p), A.toJSON s] | (p, s) <- M.toList sc],
                "phase" .= show ph,
                "round" .= rn,
                "clients" .= [A.object ["pid" .= unPid p, "name" .= nm] | (p, nm) <- cls]
              ]
          ]
  atomically $ mapM_ (\c -> void $ writeTBQueue (cOut c) payload) (M.elems cs)

-- Local copy of worldObj to keep Runtime self-contained
worldObj :: World -> A.Value
worldObj w =
  A.object
    [ "tick" .= tick w,
      "width" .= width w,
      "height" .= height w,
      "speed" .= speed w,
      "turnRate" .= turnRate w,
      "headRadiusPx" .= headRadiusPx w,
      "tailRadiusPx" .= tailRadiusPx w,
      "players" .= players w,
      "trails" .= map (\(cell, (pid, _)) -> A.toJSON [fst cell, snd cell, unPid pid]) (M.toList (trails w))
    ]