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
    _blocks :: Blocks,
    _blockStore :: Stream Coord,
    _blockGap :: Int,
    --- Counter represents the count of the end game
    --- if you want to player a longer game, set couter to some large values
    _counter :: Float,
    --- curProgress is incrmented by one / tick
    _curProgress :: Float
  }
  deriving (Show)

type Coord = V2 Int

type Player = Coord

data Stream a = a :| Stream a
  deriving (Show)

type Blocks = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width, initialBlocksCount, targetBlockGap :: Int
height = 20
width = 20
initialBlocksCount = 5
targetBlockGap = 3

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game is not paused or game over
  MaybeT $ guard . not <$> orM [use paused, use dead]
  -- Update the score if the player cosume a good block
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

-- | If consume a good block, we add 10 points to the total score
consumeGoodBlocks :: Game -> Game 
consumeGoodBlocks g = g & score .~ newScore
  where 
    newScore = case S.elemIndexL (g^.player) (g^.blocks) of
      Nothing -> g^.score 
      Just _ -> g^.score + 10

advanceTime :: Game -> Game 
advanceTime g = g & curProgress .~ ( (g ^. curProgress) + 1)

setGameOver :: Game -> Game
setGameOver g = if (g ^. counter) == (g ^. curProgress) then g & dead .~ True  else g

-- | Set a valid next food coordinate
nextBlock :: Game -> Game
nextBlock g = g' & blocks .~ newCoordSeq
  where
    (b :| bs) = g ^. blockStore
    g' = g & blockStore .~ bs
    newCoordSeq = do
      if g ^. blockGap == targetBlockGap
        then S.insertAt 0 b (g' ^. blocks)
        else g ^. blocks

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
movePlayer dir g@Game {_player = t} = do
  let newCoord = translateCoord 1 dir t
  if isInBounds newCoord
    then g & player .~ newCoord
    else g
movePlayer _ _ = error "Players can't be empty!"

moveBlocks :: Game -> Game
moveBlocks g = g & blocks .~ newCoordSeq
  where
    f _ a = translateCoord 1 South a
    newCoordSeq = S.filter isInBounds (S.mapWithIndex f (g ^. blocks))

updateBlockGap :: Game -> Game
updateBlockGap g = g & blockGap .~ newGap
  where
    newGap = do
      if g ^. blockGap == targetBlockGap
        then 0
        else g ^. blockGap + 1

-- | Initialize a paused game with random food location
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
            _paused = False ,
            _locked = False,
            _blocks = S.fromList [b],
            _blockStore = bs,
            _blockGap = 0,
            _counter = 100,
            _curProgress = 0
          }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")