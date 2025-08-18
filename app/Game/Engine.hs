{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Engine where

import Data.Bits
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Word
import Game.Types

-- RNG (SplitMix64)
next64 :: Word64 -> (Word64, Word64)
next64 s =
  let s' = s + 0x9E3779B97F4A7C15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
   in (z3, s')

uniformR :: (Int, Int) -> Word64 -> (Int, Word64)
uniformR (lo, hi) st =
  let (x, st') = next64 st
      span' = hi - lo + 1
      n64 = lo + (fromIntegral x `mod` span')
   in (fromIntegral n64 :: Int, st')

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
sweptCenters (x0, y0) (x1, y1) = let dx = x1 - x0; dy = y1 - y0; steps = max (abs dx) (abs dy) in if steps == 0 then [(x0, y0)] else [(x0 + (i * dx) `div` steps, y0 + (i * dy) `div` steps) | i <- [0 .. steps]]

paintPath :: Int -> [(Int, Int)] -> S.Set (Int, Int)
paintPath r = foldl' (\acc c -> S.union acc (S.fromList (vecDisc r c))) S.empty

advanceGap :: World -> Player -> Player
advanceGap World {..} p = case gap p of
  Solid tStart | tick >= tStart -> let (dur, r1) = uniformR (gapDurMin, gapDurMax) (rng p) in p {gap = Gapping (tick + dur), rng = r1}
  Gapping tEnd | tick >= tEnd -> let (cool, r1) = uniformR (gapCoolMin, gapCoolMax) (rng p) in p {gap = Solid (tick + cool), rng = r1}
  _ -> p

isGapping :: Player -> Bool
isGapping p = case gap p of Gapping _ -> True; _ -> False

-- Step the world by one tick. Returns (newWorld, deltaAdd, didReset, deltaRemove, survivors)
stepWorld :: [(PlayerId, Turn)] -> World -> (World, S.Set (Int, Int), Bool, S.Set (Int, Int), [PlayerId])
stepWorld inputs w0@World {..} =
  let turnOf pid' = fromMaybe Straight (lookup pid' inputs)
      dtheta = \case TurnLeft -> negate turnRate; TurnRight -> turnRate; Straight -> 0
      advance p
        | not (alive p) = (p, pix (pos p))
        | otherwise =
            let th' = dir p + dtheta (turnOf (pid p))
                p' = p {dir = th', pos = Pos (px (pos p) + speed * cos th') (py (pos p) + speed * sin th')}
             in (p', pix (pos p'))
      moved = map advance players
      moved' = [if alive p then advanceGap w0 p else p | (p, _) <- moved]
      movedWithCell = zip moved' (map snd moved)
      stepAcc (!painted, !psAcc) (p', newCell) =
        if not (alive p')
          then (painted, p' : psAcc)
          else
            let oldCell = pixPrev (pos p') (dir p') (-speed)
                path = sweptCenters oldCell newCell
                out = outOfBoundsDisc headRadiusPx width height newCell
                hit = any (`S.member` trails) (vecDisc headRadiusPx newCell) || any (`S.member` painted) (vecDisc headRadiusPx newCell)
                alive' = not (out || hit)
                shouldPaint = alive' && not (isGapping p')
                newPaint = if shouldPaint then paintPath tailRadiusPx path else S.empty
                painted' = S.union painted newPaint
                pFinal = if alive' then p' else p' {alive = False}
             in (painted', pFinal : psAcc)
      (newlyPainted, psRev) = foldl' stepAcc (S.empty, []) movedWithCell
      players1 = reverse psRev
      trails1 = S.union trails newlyPainted
      w1Base = w0 {tick = tick + 1, players = players1, trails = trails1}
      aliveP = [pid p | p <- players1, alive p]
      doReset = length aliveP <= 1
      (w1, deltaAdd, deltaRem) =
        if doReset
          then (resetWorld w1Base, S.empty, trails1)
          else (w1Base, newlyPainted, S.empty)
   in (w1, deltaAdd, doReset, deltaRem, aliveP)

resetWorld :: World -> World
resetWorld w@World {} = w {trails = S.empty, players = respawnPlayers w}

respawnPlayers :: World -> [Player]
respawnPlayers World {..} =
  let spots =
        cycle
          [ (width * 0.25, height * 0.5, 0),
            (width * 0.75, height * 0.5, pi),
            (width * 0.5, height * 0.25, pi / 2),
            (width * 0.5, height * 0.75, -(pi / 2))
          ]
      reinit p (x, y, th) =
        let (cool, r1) = uniformR (gapCoolMin, gapCoolMax) (seed `xor` fromIntegral (unPid (pid p)))
         in p {pos = Pos x y, dir = th, alive = True, gap = Solid (tick + cool), rng = r1}
   in zipWith reinit players spots

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
      p1 = Player (PlayerId 1) (Pos (fromIntegral (w `div` 4)) (fromIntegral (h `div` 2))) 0 True (Solid 120) (seed0 `xor` 0xA5A5)
      p2 = Player (PlayerId 2) (Pos (fromIntegral (w * 3 `div` 4)) (fromIntegral (h `div` 2))) pi True (Solid 200) (seed0 `xor` 0x5A5A)
  pure
    World
      { tick = 0,
        width = fromIntegral w,
        height = fromIntegral h,
        speed = speed',
        turnRate = turn',
        headRadiusPx = headR,
        tailRadiusPx = tailR,
        gapCoolMin = cmin,
        gapCoolMax = cmax,
        gapDurMin = dmin,
        gapDurMax = dmax,
        seed = seed0,
        trails = S.empty,
        players = [p1, p2]
      }
