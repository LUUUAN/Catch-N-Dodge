{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module CnD
  ( initGame,
    step,
    Game (..),
    Direction (..),
    dead,
    score,
    player,
    height,
    width,
    movePlayer,
    counter,
    curProgress,
    goodBlocks,
    badBlocks,
    level,
    highestScore,
    translateCoord,
    isInBounds,
    shift,
    consumeGoodBlocks,
    consumeBadBlocks,
    moveBlocks,
    updateDelay,
    goodBlockDelay,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.List
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.IO
import System.Random (Random (..), RandomGen, mkStdGen, newStdGen)

-- Types

type Coord = V2 Int

type Player = Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data Game = Game
  { -- | Player as a sequence of points in N2
    _player :: Player,
    -- | direction
    _dir :: Direction,
    -- | game over flag
    _dead :: Bool,
    -- | score
    _score :: Int,
    -- | lock to disallow duplicate turns between time steps
    _locked :: Bool,
    -- | designated number of good block each turn
    _goodPerTurn :: Int,
    -- | designated number of bad block each turn
    _badPerTurn :: Int,
    -- | good block counter in current iteration
    _goodCntPerTurn :: Int,
    -- | bad block counter in current iteration
    _badCntPerTurn :: Int,
    -- | store all good blocks currently appeared in the game
    _goodBlocks :: Seq Coord,
    _goodBlockDelay :: Int,
    -- | store all bad blocks currently appeared in the game
    _badBlocks :: Seq Coord,
    -- | store all blocks will appear in the game later
    _blockStore :: Stream Coord,
    -- | predefined vertical gap between all blocks (in terms of tick)
    _blockGap :: Int,
    --- Counter represents the count of the end game
    --- if you want to player a longer game, set counter to some large values
    _counter :: Float,
    --- curProgress is incremented by one / tick
    _curProgress :: Float,
    -- | difficulty level from 0 to 9
    _level :: Int,
    _highestScore :: [Int]
  }
  deriving (Show)

makeLenses ''Game

-- Constants

height, width, targetBlockGap, targetGoodDelay :: Int
height = 17
width = 20
targetGoodDelay = 2
targetBlockGap = 2 * targetGoodDelay

-- Functions

-- | Step forward in time
-- step :: Game -> Game
-- step s = flip execState s . runMaybeT $ do
--   -- Make sure the game is not paused or game over
--   MaybeT $ guard . not <$> orM [use dead]
--   -- Update the score if the player consume a good block
--   MaybeT (Just <$> modify consumeGoodBlocks)
--   -- Update the score if the player consume a bad block
--   MaybeT (Just <$> modify consumeBadBlocks)
--   -- Increment the curProgress
--   MaybeT (Just <$> modify advanceTime)
--   -- if the counter is equal to curProgress, then the game is over
--   MaybeT (Just <$> modify setGameOver)
--   --- Drop the blocks for each tick
--   MaybeT (Just <$> modify moveBlocks)
--   --- Randomly Generate nextBlock
--   MaybeT (Just <$> modify nextBlock)
--   MaybeT (Just <$> modify updateBlockCnt)
--   MaybeT (Just <$> modify updateBlockGap)
--   MaybeT (Just <$> modify updateDelay)

-- step g = fromMaybe g $ do
--   guard ( not (g^.dead) )
--   return (stepHelper g)

step g = if g^.dead then g else stepHelper g

stepHelper:: Game -> Game
stepHelper = updateDelay . updateBlockGap . updateBlockCnt . nextBlock . moveBlocks . setGameOver . advanceTime . consumeBadBlocks . consumeGoodBlocks

consumeGoodBlocks :: Game -> Game
consumeGoodBlocks g =
  if null (g ^. goodBlocks)
    then g
    else (g & score .~ newScore) & goodBlocks .~ newGoodBlocks
  where
    bs :|> b = g ^. goodBlocks
    newScore
      | g ^. player == b = g ^. score + 10
      | otherwise = g ^. score
    newGoodBlocks =
      if g ^. player == b
        then S.deleteAt (S.length (g ^. goodBlocks) - 1) (g ^. goodBlocks)
        else g ^. goodBlocks

consumeBadBlocks :: Game -> Game
consumeBadBlocks g =
  if null (g ^. badBlocks)
    then g
    else (g & score .~ newScore) & badBlocks .~ newBadBlocks
  where
    bs :|> b = g ^. badBlocks
    newScore
      | g ^. player == b = g ^. score - 20
      | otherwise = g ^. score
    newBadBlocks =
      if g ^. player == b
        then S.deleteAt (S.length (g ^. badBlocks) - 1) (g ^. badBlocks)
        else g ^. badBlocks

advanceTime :: Game -> Game
advanceTime g = g & curProgress .~ ((g ^. curProgress) + 1)

setGameOver :: Game -> Game
setGameOver g = if (g ^. counter) <= (g ^. curProgress) then g & dead .~ True else g

-- | Insert a new block at the top of the game if blockGap satisfied
nextBlock :: Game -> Game
nextBlock g = (g' & goodBlocks .~ newGoodBlocks) & badBlocks .~ newBadBlocks
  where
    (b :| bs) = g ^. blockStore
    g' = g & blockStore .~ bs
    isBad = do
      if g ^. badCntPerTurn < g ^. badPerTurn && g ^. goodCntPerTurn < g ^. goodPerTurn
        then g ^. badCntPerTurn < g ^. goodCntPerTurn
        else g ^. badCntPerTurn < g ^. badPerTurn
    newBadBlocks = do
      if isBad && g ^. blockGap == targetBlockGap
        then S.insertAt 0 b (g' ^. badBlocks)
        else g ^. badBlocks
    newGoodBlocks = do
      if not isBad && g ^. blockGap == targetBlockGap
        then S.insertAt 0 b (g ^. goodBlocks)
        else g ^. goodBlocks

isInBounds :: Coord -> Bool
isInBounds (V2 x y) = 0 <= x && x < width && 0 <= y

shift :: Coord -> Coord
shift = translateCoord 1 South

translateCoord :: Int -> Direction -> Coord -> Coord
translateCoord n West (V2 x y) = V2 (x - n) y
translateCoord n East (V2 x y) = V2 (x + n) y
translateCoord n North (V2 x y) = V2 x (y + n)
translateCoord n South (V2 x y) = V2 x (y - n)

movePlayer :: Direction -> Game -> Game
movePlayer dir g = do
  let newCoord = translateCoord 1 dir (g ^. player)
  if isInBounds newCoord
    then g & player .~ newCoord
    else g

-- | Gravitate the block
moveBlocks :: Game -> Game
moveBlocks g = (g & goodBlocks .~ newGoodBlocks) & badBlocks .~ newBadBlocks
  where
    f _ blk = translateCoord 1 South blk
    -- newBlocks = S.mapWithIndex f (g ^. blocks)
    newBadBlocks = S.filter isInBounds (S.mapWithIndex f (g ^. badBlocks))
    newGoodBlocks =
      if g ^. goodBlockDelay == targetGoodDelay
        then S.filter isInBounds (S.mapWithIndex f (g ^. goodBlocks))
        else g ^. goodBlocks

updateBlockGap :: Game -> Game
updateBlockGap g = g & blockGap .~ newGap
  where
    newGap = do
      if g ^. blockGap == targetBlockGap
        then 1
        else g ^. blockGap + 1

updateBlockCnt :: Game -> Game
updateBlockCnt g = (g & goodCntPerTurn .~ gCnt) & badCntPerTurn .~ bCnt
  where
    isBad = do
      if g ^. badCntPerTurn < g ^. badPerTurn && g ^. goodCntPerTurn < g ^. goodPerTurn
        then g ^. badCntPerTurn < g ^. goodCntPerTurn
        else g ^. badCntPerTurn < g ^. badPerTurn
    gCnt = do
      if g ^. blockGap == targetBlockGap
        then
          if g ^. goodCntPerTurn + g ^. badCntPerTurn + 1 == g ^. goodPerTurn + g ^. badPerTurn
            then 0
            else
              if not isBad
                then g ^. goodCntPerTurn + 1
                else g ^. goodCntPerTurn
        else g ^. goodCntPerTurn
    bCnt = do
      if g ^. blockGap == targetBlockGap
        then
          if g ^. goodCntPerTurn + g ^. badCntPerTurn + 1 == g ^. goodPerTurn + g ^. badPerTurn
            then 0
            else
              if isBad
                then g ^. badCntPerTurn + 1
                else g ^. badCntPerTurn
        else g ^. badCntPerTurn

updateDelay :: Game -> Game
updateDelay g = g & goodBlockDelay .~ newDelay
  where
    newDelay = do
      if g ^. goodBlockDelay == targetGoodDelay
        then 1
        else g ^. goodBlockDelay + 1

-- change the ratio of good block and bad block to facilitate change of difficulty
levelToBadBlock :: Int -> Int
levelToBadBlock n
  | n == 0 = 1
  | n == 1 = 1
  | n == 2 = 1
  | n == 3 = 1
  | n == 4 = 1
  | n == 5 = 2
  | n == 6 = 3
  | n == 7 = 4
  | n == 8 = 5
  | n == 9 = 6
  | otherwise = 6

levelToGoodBlock :: Int -> Int
levelToGoodBlock n
  | n == 0 = 5
  | n == 1 = 4
  | n == 2 = 3
  | n == 3 = 2
  | n == 4 = 1
  | n == 5 = 1
  | n == 6 = 1
  | n == 7 = 1
  | n == 8 = 1
  | n == 9 = 1
  | otherwise = 1

-- change counter to maintain same playing time for each level
levelToCounter :: Int -> Float
levelToCounter n
  | n == 0 = 100
  | n == 1 = 100 * 1.55
  | n == 2 = 100 * 1.68
  | n == 3 = 100 * 1.79
  | n == 4 = 100 * 1.92
  | n == 5 = 100 * 2.10
  | n == 6 = 100 * 2.39
  | n == 7 = 100 * 2.76
  | n == 8 = 100 * 3.19
  | n == 9 = 100 * 3.70
  | otherwise = 100 * 1.15

-- | Initialize a paused game with random block location
initGame :: Int -> [Int] -> IO Game
initGame lvl scores = do
  (b :| bs) <-
    fromList . randomRs (V2 0 height, V2 width height) <$> newStdGen
  let g =
        Game
          { _player = V2 (width `div` 2) 0,
            _score = 0,
            _dir = East,
            _dead = False,
            _locked = False,
            _goodBlocks = S.fromList [b],
            _badBlocks = S.fromList [],
            _blockGap = 0,
            _goodPerTurn = levelToGoodBlock lvl,
            _badPerTurn = levelToBadBlock lvl,
            _badCntPerTurn = 0,
            _goodCntPerTurn = 0,
            _blockStore = bs,
            _counter = levelToCounter lvl,
            _curProgress = 0,
            _goodBlockDelay = 0,
            _level = lvl,
            _highestScore = scores
          }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

-- updateHighestScore :: Game -> Game
-- updateHighestScore  g = g & highestScore .~ newHighestScore
--                           where
--                             (x,prevScore:y) = splitAt (g ^. level) (g ^. highestScore)
--                             newHighestScore = if (g ^. score) <=  prevScore then g ^. highestScore else x ++ [g ^. score] ++ y
