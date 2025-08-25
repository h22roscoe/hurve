{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Engine where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Game.Types
import System.Random (mkStdGen, mkStdGen64, uniformR)
import System.Random.Shuffle (shuffle')

-- Geometry helpers
pix :: Pos -> (Int, Int)
pix (Pos x y) = (round x, round y)

pixPrev :: Pos -> Coord -> Coord -> (Int, Int)
pixPrev (Pos x y) th delta = (round (x + delta * cos th), round (y + delta * sin th))

vecDisc :: Int -> (Int, Int) -> [(Int, Int)]
vecDisc r (cx, cy) = [(cx + dx, cy + dy) | dx <- [-r .. r], let dx2 = dx * dx, dy <- [-r .. r], dx2 + dy * dy <= r * r]

outOfBoundsDisc :: Int -> Coord -> Coord -> (Int, Int) -> Bool
outOfBoundsDisc r w h (cx, cy) = let maxX = round w - 1; maxY = round h - 1 in cx - r < 0 || cy - r < 0 || cx + r > maxX || cy + r > maxY

sweptCenters :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
sweptCenters (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
      steps = max (abs dx) (abs dy)
   in if steps == 0 then [(x0, y0)] else [(x0 + (i * dx) `div` steps, y0 + (i * dy) `div` steps) | i <- [0 .. steps]]

paintPath :: Int -> [(Int, Int)] -> S.Set (Int, Int)
paintPath r = foldl' (\acc c -> S.union acc (S.fromList (vecDisc r c))) S.empty

advanceGap :: World -> Player -> Player
advanceGap World{..} p =
  let (durT, durR) = uniformR (gapDurMin, gapDurMax) (rng p)
      (coolT, coolR) = uniformR (gapCoolMin, gapCoolMax) (rng p)
   in case gap p of
        Solid tStart | tick >= tStart -> p{gap = Gapping (tick + durT), rng = durR}
        Gapping tEnd | tick >= tEnd -> p{gap = Solid (tick + coolT), rng = coolR}
        _ -> p

isGapping :: Player -> Bool
isGapping p = case gap p of Gapping _ -> True; _ -> False

startPositions :: Double -> Double -> [(Double, Double, Double)]
startPositions w h =
  [ (w / 4, h / 4, 0)
  , (3 * w / 4, h / 4, pi)
  , (w / 4, 3 * h / 4, pi / 2)
  , (3 * w / 4, 3 * h / 4, 3 * pi / 2)
  , (w / 2, h / 4, pi / 2)
  , (w / 2, 3 * h / 4, 3 * pi / 2)
  , (w / 4, h / 2, 0)
  , (3 * w / 4, h / 2, pi)
  ]

-- Step the world by one tick. Returns (newWorld, deltaAdd, didReset, deltaRemove, survivors)
stepWorld ::
  Int ->
  Int ->
  [(PlayerId, Turn)] ->
  World ->
  (World, S.Set (Int, Int), Bool, S.Set (Int, Int), [PlayerId])
stepWorld now noCollideUntil inputs w0@World{..} =
  let turnOf pid' = fromMaybe Straight (lookup pid' inputs)
      dtheta = \case TurnLeft -> negate turnRate; TurnRight -> turnRate; Straight -> 0

      advance p
        | not (alive p) = (p, pix (pos p))
        | otherwise =
            let th' = dir p + dtheta (turnOf (pid p))
                p' =
                  p
                    { dir = th'
                    , pos =
                        Pos
                          (px (pos p) + speed * cos th')
                          (py (pos p) + speed * sin th')
                    }
             in (p', pix (pos p'))

      moved = map advance players
      moved' = [if alive p then advanceGap w0 p else p | (p, _) <- moved]
      movedWithCell = zip moved' (map snd moved)

      -- fold over players, accumulating newly-painted cells (as Map) and updated players
      stepAcc (!paintedMap, !psAcc) (p', newCell) =
        if not (alive p')
          then (paintedMap, p' : psAcc)
          else
            let oldCell = pixPrev (pos p') (dir p') (-speed)
                path = sweptCenters oldCell newCell
                ownGrace = 10 -- 10 ticks so the head doesn't crash with tail being produced
                -- compute potential paint for THIS player THIS tick
                newPaintKeys =
                  if now >= noCollideUntil && not (isGapping p')
                    then paintPath tailRadiusPx path
                    else S.empty
                -- convert to Map with ownership & timestamp
                newPaintMap = M.fromSet (const (pid p', now)) newPaintKeys

                -- collision check (skip during global start grace)
                headCells = vecDisc headRadiusPx newCell
                out = outOfBoundsDisc headRadiusPx width height newCell

                -- Helper: does a cell hit solid trail considering ownership?
                hitsTrail cell =
                  case M.lookup cell trails of
                    Just (owner, tPaint)
                      | owner == pid p' && now - tPaint <= ownGrace -> False -- ignore own recent
                      | otherwise -> True -- collide
                    Nothing ->
                      -- Also check new paint laid down earlier this tick by OTHER players.
                      case M.lookup cell paintedMap of
                        Just (owner2, _t2) -> owner2 /= pid p'
                        Nothing -> False

                hit =
                  now >= noCollideUntil
                    && any hitsTrail headCells

                alive' = not (out || hit)
                painted' =
                  if alive'
                    then M.union paintedMap newPaintMap
                    else paintedMap
                pFinal = if alive' then p' else p'{alive = False}
             in (painted', pFinal : psAcc)

      (newlyPaintedMap, psRev) = foldl stepAcc (M.empty, []) movedWithCell
      players1 = reverse psRev
      trails1 = M.union trails newlyPaintedMap

      w1Base = w0{tick = tick + 1, players = players1, trails = trails1}
      aliveP = [pid p | p <- players1, alive p]
      doReset = length aliveP <= 1

      (w1, deltaAdd, deltaRem) =
        if doReset
          then (resetWorld (map pid players1) w1Base, S.empty, S.fromList (M.keys trails1))
          else (w1Base, S.fromList (M.keys newlyPaintedMap), S.empty)
   in (w1, deltaAdd, doReset, deltaRem, aliveP)

resetWorld :: [PlayerId] -> World -> World
resetWorld pids w@World{..} = do
  let basePositions = startPositions width height
  let shuffledPositions = shuffle' basePositions (length basePositions) (mkStdGen64 seed)
  let chosen = take (length pids) shuffledPositions
      ps = zipWith mkPlayer pids chosen
  w
    { tick = 0
    , trails = M.empty
    , players = ps
    }
 where
  mkPlayer pid (x, y, theta) =
    let (cool, rng') = uniformR (gapCoolMin, gapCoolMax) (mkStdGen (unPid pid))
     in Player
          { pid = pid
          , pos = Pos x y
          , dir = theta
          , gap = Solid (tick + cool)
          , alive = True
          , rng = rng'
          }

initialWorld :: IO World
initialWorld = do
  let w = 800 :: Integer
      h = 600 :: Integer
      speed' = 2.2
      turn' = 0.08
      headR = 2
      tailR = 2
      cmin = 150
      cmax = 300
      dmin = 18
      dmax = 45
      seed0 = 0xDEADBEEFCAFEBABE
  pure
    World
      { tick = 0
      , width = fromIntegral w
      , height = fromIntegral h
      , speed = speed'
      , turnRate = turn'
      , headRadiusPx = headR
      , tailRadiusPx = tailR
      , gapCoolMin = cmin
      , gapCoolMax = cmax
      , gapDurMin = dmin
      , gapDurMax = dmax
      , seed = seed0
      , trails = M.empty
      , players = []
      }
