module Main where

import CnD (Game (..))
import Control.Monad (when)
import Options.Applicative
import qualified System.Directory as D
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import UI.Game (playGame)
import UI.PickLevel (pickLevel)

data Opts = Opts
  { level :: Maybe Int,
    score :: Bool
  }

opts :: Parser Opts
opts =
  Opts
    <$> optional
      ( option
          auto
          ( long "level"
              <> short 'l'
              <> metavar "LEVEL"
              <> help "Specify level (unspecified results in prompt)"
          )
      )
    <*> switch
      ( long "high-score"
          <> help "Print high score and exit"
      )

fullopts :: ParserInfo Opts
fullopts =
  info
    (helper <*> opts)
    ( fullDesc
        <> header "NSShaft - the iconic game right in your terminal"
    )

main :: IO ()
main = do
  (Opts ml hs) <- execParser fullopts -- get CLI opts/args
  when hs (getHighScore >>= printM >> exitSuccess) -- show high score and exit
  l <- maybe pickLevel return ml -- pick level prompt if necessary
  g <- playGame l -- play game
  handleEndGame (_score g) -- save & print score

handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
  where
    justShowScore = putStrLn ("Your final score: " ++ show s)
    newHighScore = do
      putStrLn $ "Congrats! You just got the new highest score: " ++ show s
      setHighScore s

printM :: Show a => Maybe a -> IO ()
printM Nothing = putStrLn "None"
printM (Just s) = print s

getHighScore :: IO (Maybe Int)
getHighScore = do
  lb <- getLeaderboardFile
  exists <- D.doesFileExist lb
  if exists
    then readMaybe <$> readFile lb
    else return Nothing

setHighScore :: Int -> IO ()
setHighScore s = do
  lb <- getLeaderboardFile
  writeFile lb (show s)

getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- D.getXdgDirectory D.XdgData "GameN"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")
