{-# LANGUAGE OverloadedStrings #-}

module UI.Game (playGame) where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    continue,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Brick.Widgets.ProgressBar
import qualified Brick.Widgets.ProgressBar as P
import CnD
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))
import Text.Printf (printf)

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Player | Empty | BadBlock | GoodBlock

-- App definition

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

playGame ::
  -- | Starting level
  Int -> [Int] ->
  IO Game
playGame lvl scores = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 200000 -- decides how fast your game moves
  initG <- initGame lvl scores
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  customMain initialVty builder (Just chan) app initG


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ movePlayer East g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ movePlayer West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame (g ^. level) (g ^. highestScore )) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [((C.center $ (padRight (Pad 2) (drawStats g) <+> drawGrid g)) <=> drawGameProgressBar g) <+> padLeft (Pad 2) drawHelp]

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 11 $
    vBox
      [ 
        drawHighestScore g,
        padTop (Pad 2) $ drawScore (g ^. score),
        padTop (Pad 2) $ drawGameOver (g ^. dead)
      ]

drawScore :: Int -> Widget Name
drawScore n =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Score") $
      C.hCenter $
        padAll 1 $
          str $ show n

drawHighestScore :: Game -> Widget Name
drawHighestScore g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Highest") $
      C.hCenter $
        padAll 1 $
          str $ show s
          where 
            (_,s:_) = splitAt (g ^. level) (g ^. highestScore)

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str " Game ") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c == g ^. player = Player
      | c `elem` (g ^. badBlocks) = BadBlock
      | c `elem` g ^. goodBlocks = GoodBlock
      | otherwise = Empty

drawGameProgressBar :: Game -> Widget Name
drawGameProgressBar g =
  withBorderStyle BS.unicodeBold
    . overrideAttr progressCompleteAttr gameProgressAttr
    $ C.vCenter $
      vLimit 3 $
        C.hCenter $
          hLimit 45 $
            progressBar (Just $ displayProgress "Time Tick" percent) percent
  where
    percent = (g ^. curProgress) / (g ^. counter)

drawCell :: Cell -> Widget Name
drawCell Player = withAttr playerAttr cw
drawCell Empty = withAttr emptyAttr cw
drawCell BadBlock = withAttr badBlocksAttr cw
drawCell GoodBlock = withAttr goodBlocksAttr cw


drawHelp :: Widget Name
drawHelp =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Help")
    $ padTopBottom 1
    --  $ C.hCenter 
      -- $ padAll 1 
        $ vLimit 30 
          $ hLimit 30 
             $ vBox
              $ map (uncurry drawKeyInfo)
              [ 
                ("Move Left"   , "←")
              , ("Move Right"  , "→")
              , ("Restart Game", "r")
              , ("Quit Game"   , "q")
              , (" " , " ")
              , ("Yellow Blocks",  "+10pt")
              , ("Red Blocks",     "-20pt")
              ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)


cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, V.blue `on` V.blue),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (goodBlocksAttr, V.yellow `on` V.yellow),
      (badBlocksAttr, V.red `on` V.red),
      (gameProgressAttr, V.white `on` V.red)
    ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

playerAttr, emptyAttr :: AttrName
playerAttr = "playerAttr"

-- blocksAttr = "blocksAttr"
badBlocksAttr = "badBlocksAttr"

goodBlocksAttr = "goodBlocksAttr"

emptyAttr = "emptyAttr"

gameProgressAttr :: AttrName
gameProgressAttr = "progress"

displayProgress :: String -> Float -> String
displayProgress w amt = printf "%s %.0f%%" w (amt * 100)