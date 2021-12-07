{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Player
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
    blocks,
    counter,
    curProgress,
    flatBlocks,
    BlockType (..),
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
import System.Random (Random (..), RandomGen, mkStdGen, newStdGen)

-- Types

type Coord = V2 Int

type Player = Coord

data Stream a = a :| Stream a
  deriving (Show)

type Blocks = Seq Block

data BlockType = GOOD | BAD
  deriving (Eq, Show, Enum)

data Block = Block
  { -- | block type
    _category :: BlockType,
    -- | position
    _pos :: Coord,
  }
  deriving (Eq, Show)

makeLenses ''Block

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
    -- | paused flag
    _paused :: Bool,
    -- | score
    _score :: Int,
    -- | lock to disallow duplicate turns between time steps
    _locked :: Bool,
    -- | store all blocks currently appeared in the game
    _blocks :: Blocks,
    -- | store all blocks will appear in the game later
    _blockStore :: Stream Coord,
    -- | predefined vertical gap between blocks
    _blockGap :: Int,
    -- | Output a bad block every badBlockGap blocks
    _badBlockGap :: Int,
    --- Counter represents the count of the end game
    --- if you want to player a longer game, set couter to some large values
    _counter :: Float,
    --- curProgress is incremented by one / tick
    _curProgress :: Float
  }
  deriving (Show)

makeLenses ''Game

-- Constants

height, width, initialBlocksCount, targetBlockGap, targetBadBlockGap :: Int
height = 17
width = 20
initialBlocksCount = 5
targetBlockGap = 3
targetBadBlockGap = 3

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game is not paused or game over
  MaybeT $ guard . not <$> orM [use paused, use dead]
  -- Update the score if the player consume a good block
  MaybeT (Just <$> modify consumeGoodBlocks)
  -- Increment the curProgress
  MaybeT (Just <$> modify advanceTime)
  -- if the counter is equal to curProgress, then the game is over
  MaybeT (Just <$> modify setGameOver)
  --- Drop the blocks for each tick
  MaybeT (Just <$> modify moveBlocks)
  --- Randomly Generate nextBlock
  MaybeT (Just <$> modify nextBlock)
  MaybeT (Just <$> modify updateBlockGap)

-- die (moved into boundary), eat (moved into food), or move (move into space)
-- die <|> MaybeT (Just <$> modify move)

-- -- | Possibly die if next head position is in snake
-- die :: MaybeT (State Game) ()
-- die = do
--   MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> (use snake)
--   MaybeT . fmap Just $ dead .= True

consumeGoodBlocks :: Game -> Game
consumeGoodBlocks g = (g & score .~ newScore) & blocks .~ newBlocks
  where
    bs :|> b = g ^. blocks
    newScore
      | g ^. player == b ^. pos && b ^. category == GOOD = g ^. score + 10
      | g ^. player == b ^. pos && b ^. category == BAD = g ^. score - 20
      | otherwise = g ^. score
    newBlocks =
      if g ^. player == b ^. pos
        then S.deleteAt (S.length (g ^. blocks) - 1) (g ^. blocks)
        else g ^. blocks

advanceTime :: Game -> Game
advanceTime g = g & curProgress .~ ((g ^. curProgress) + 1)

setGameOver :: Game -> Game
setGameOver g = if (g ^. counter) == (g ^. curProgress) then g & dead .~ True else g

-- | Insert a new block at the top of the game if blockGap satisfied
nextBlock :: Game -> Game
nextBlock g = g' & blocks .~ newBlockSeq
  where
    (b :| bs) = g ^. blockStore
    g' = g & blockStore .~ bs
    newBlockSeq = do
      if g ^. blockGap == targetBlockGap && g ^. badBlockGap == targetBadBlockGap
        then S.insertAt 0 (Block BAD b) (g' ^. blocks)
        else
          if g ^. blockGap == targetBlockGap
            then S.insertAt 0 (Block GOOD b) (g' ^. blocks)
            else g ^. blocks

isInBounds :: Coord -> Bool
isInBounds (V2 x y) = 0 <= x && x < width && 0 <= y

blockInBounds :: Block -> Bool
blockInBounds b = isInBounds (b ^. pos)

shift :: Coord -> Coord
shift = translateCoord 1 South

translateCoord :: Int -> Direction -> Coord -> Coord
translateCoord n West (V2 x y) = V2 (x - n) y
translateCoord n East (V2 x y) = V2 (x + n) y
translateCoord n North (V2 x y) = V2 x (y + n)
translateCoord n South (V2 x y) = V2 x (y - n)

movePlayer :: Direction -> Game -> Game
movePlayer dir g@Game {_player = t} = do
  let newCoord = translateCoord 1 dir t
  if isInBounds newCoord
    then g & player .~ newCoord
    else g
movePlayer _ _ = error "Players can't be empty!"

-- | Gravitate the block
moveBlocks :: Game -> Game
moveBlocks g = g & blocks .~ newBlocks
  where
    f _ blk = blk & pos .~ translateCoord 1 South (blk ^. pos)
    -- newBlocks = S.mapWithIndex f (g ^. blocks)
    newBlocks = S.filter blockInBounds (S.mapWithIndex f (g ^. blocks))

updateBlockGap :: Game -> Game
updateBlockGap g = (g & blockGap .~ newGap) & badBlockGap .~ newBBGap
  where
    newBBGap = do
      if g ^. blockGap == targetBlockGap && g ^. badBlockGap == targetBadBlockGap
        then 0
        else
          if g ^. blockGap == targetBlockGap
            then g ^. badBlockGap + 1
            else g ^. badBlockGap
    newGap = do
      if g ^. blockGap == targetBlockGap
        then 0
        else g ^. blockGap + 1

flatBlocks :: Blocks -> (Seq Coord, Seq BlockType)
flatBlocks blks = S.unzipWith getBlockPos blks

getBlockPos :: Block -> (Coord, BlockType)
getBlockPos blk = (blk ^. pos, blk ^. category)

-- | Initialize a paused game with random block location
initGame :: IO Game
initGame = do
  (b :| bs) <-
    fromList . randomRs (V2 0 height, V2 width height) <$> newStdGen
  let g =
        Game
          { _player = V2 (width `div` 2) 0,
            _score = 0,
            _dir = East,
            _dead = False,
            _paused = False,
            _locked = False,
            _blocks = S.fromList [Block GOOD b],
            _blockStore = bs,
            _blockGap = 0,
            _badBlockGap = 0,
            _counter = 100,
            _curProgress = 0
          }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")