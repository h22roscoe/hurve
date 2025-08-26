{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Types where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random (StdGen, mkStdGen)

-- Basic types

type Time = Int

type Coord = Double -- switch to Fixed for strict determinism cross-platform

newtype PlayerId = PlayerId {unPid :: Int}
  deriving (Eq, Ord, Show)

instance ToJSON PlayerId where toJSON = toJSON . unPid

instance FromJSON PlayerId where parseJSON = fmap PlayerId . parseJSON

-- Inputs

data Turn = TurnLeft | TurnRight | Straight deriving (Show, Eq, Generic)

instance ToJSON Turn where
  toJSON TurnLeft = A.String "Left"
  toJSON TurnRight = A.String "Right"
  toJSON Straight = A.String "Straight"

instance FromJSON Turn where
  parseJSON = A.withText "Turn" $ \case
    "Left" -> pure TurnLeft
    "Right" -> pure TurnRight
    "Straight" -> pure Straight
    _ -> fail "invalid Turn"

-- World & players

data Pos = Pos {px :: !Coord, py :: !Coord} deriving (Show, Eq, Generic)

instance ToJSON Pos

instance FromJSON Pos

data GapState
  = Solid {nextAt :: !Time}
  | Gapping {untilT :: !Time}
  deriving (Show, Eq, Generic)

instance ToJSON GapState

instance FromJSON GapState

data Player = Player
  { pid :: !PlayerId
  , pos :: !Pos
  , dir :: !Coord
  , alive :: !Bool
  , gap :: !GapState
  , rng :: !StdGen
  }
  deriving (Show, Generic)

instance ToJSON Player where
  toJSON Player{..} =
    A.object
      [ "pid" .= pid
      , "pos" .= pos
      , "dir" .= dir
      , "alive" .= alive
      , "gap" .= gap
      ]

instance FromJSON Player where
  parseJSON = A.withObject "Player" $ \o -> do
    pid <- o .: "pid"
    pos <- o .: "pos"
    dir <- o .: "dir"
    alive <- o .: "alive"
    gap <- o .: "gap"
    let rng = mkStdGen (unPid pid)
    pure Player{..}

-- Server-side world
type Cell = (Int, Int)

type Trail = M.Map Cell (PlayerId, Int) -- (owner, paintedTick)

data World = World
  { tick :: !Time
  , width :: !Coord
  , height :: !Coord
  , speed :: !Coord
  , turnRate :: !Coord
  , headRadiusPx :: !Int
  , tailRadiusPx :: !Int
  , gapCoolMin :: !Int
  , gapCoolMax :: !Int
  , gapDurMin :: !Int
  , gapDurMax :: !Int
  , seed :: !Word64
  , trails :: !Trail
  , players :: ![Player]
  }
  deriving (Show, Generic)

instance ToJSON World

instance FromJSON World
