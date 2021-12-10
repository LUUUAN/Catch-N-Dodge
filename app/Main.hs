module Main where

import CnD (Game (..))
import Options.Applicative
import System.IO
import UI.Game (playGame)
import UI.PickLevel (pickLevel)

-- import Control.Monad (when)
-- import qualified System.Directory as D
-- import System.Exit (exitSuccess)
-- import System.FilePath ((</>))
-- import Text.Read (readMaybe)

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
        <> header "CnD - the iconic game right in your terminal"
    )

main :: IO ()
main = do
  (Opts ml hs) <- execParser fullopts -- get CLI opts/args
  -- when hs (getHighScore >>= printM >> exitSuccess) -- show high score and exit
  l <- maybe pickLevel return ml -- pick level prompt if necessary
  prevScores <- readScores
  g <- playGame l prevScores -- play game
  handleEndGame (_score g) (_level g) (_highestScore g) -- save & print score

handleEndGame :: Int -> Int -> [Int] -> IO ()
handleEndGame s l scores = do
  let mhs = getHighScore l scores
  if s <= mhs then justShowScore else newHighScore
  where
    justShowScore = putStrLn ("Your final score: " ++ show s)
    newHighScore = do
      putStrLn $ "Congrats! You just got the new highest score: " ++ show s
      let newScores = setHighScore s l scores
      writeScores (scoresToString newScores)

printM :: Show a => Maybe a -> IO ()
printM Nothing = putStrLn "None"
printM (Just s) = print s

writeScores :: String -> IO ()
writeScores s = do
  scoreFile <- openFile "src/numbers.txt" WriteMode
  hPutStrLn scoreFile s
  hClose scoreFile

scoresToString :: [Int] -> String
scoresToString [] = ""
scoresToString (h : t) = show h ++ " " ++ scoresToString t

getHighScore :: Int -> [Int] -> Int
getHighScore l scores = s
  where
    (_, s : _) = splitAt l scores

setHighScore :: Int -> Int -> [Int] -> [Int]
setHighScore newHighestScore l scores = s
  where
    (x, _ : y) = splitAt l scores
    s = x ++ [newHighestScore] ++ y

getNumbers :: String -> [Int]
getNumbers str = map (read :: String -> Int) (words str)

readScores :: IO [Int]
readScores = do
  contents <- readFile "src/numbers.txt"
  putStrLn contents
  let numbers = getNumbers contents
  return numbers

-- getHighScore :: IO (Maybe Int)
-- getHighScore = do
--   lb <- getLeaderboardFile
--   exists <- D.doesFileExist lb
--   if exists
--     then readMaybe <$> readFile lb
--     else return Nothing

-- setHighScore :: Int -> IO ()
-- setHighScore s = do
--   lb <- getLeaderboardFile
--   writeFile lb (show s)

-- getLeaderboardFile :: IO FilePath
-- getLeaderboardFile = do
--   xdg <- D.getXdgDirectory D.XdgData "GameN"
--   D.createDirectoryIfMissing True xdg
--   return (xdg </> "leaderboard")