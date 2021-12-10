module Main where

import CnD
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.Exit
import Test.HUnit
import UI.Game

testMovement = TestCase (assertEqual "Movement" (translateCoord 2 East (V2 1 5)) (V2 3 5))

testShift = TestCase (assertEqual "Shift" (shift (V2 10 5)) (V2 10 4))

testInBound = TestCase (assertEqual "In Bound" (isInBounds (V2 100 5)) False)

main :: IO ()
main = do
  putStrLn "\nRunning CnD Tests... "
  game <- initGame 1 [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  counts <-
    runTestTT
      ( test
          [ testMovement,
            testInBound,
            testShift,
            TestCase (assertEqual "Consume Good Block Success" ((consumeGoodBlocks ((game & goodBlocks .~ (S.fromList ([(V2 4 0)]))) & player .~ (V2 4 0))) ^. goodBlocks) (S.fromList ([]))),
            TestCase
              (assertEqual "Consume Good Block Fail" ((consumeGoodBlocks ((game & goodBlocks .~ (S.fromList ([(V2 4 8)]))) & player .~ (V2 4 0))) ^. goodBlocks) (S.fromList ([(V2 4 8)]))),
            TestCase
              (assertEqual "Consume Bad Block Success" ((consumeBadBlocks ((game & badBlocks .~ (S.fromList ([(V2 4 0)]))) & player .~ (V2 4 0))) ^. badBlocks) (S.fromList ([]))),
            TestCase (assertEqual "Consume Bad Block Fail" ((consumeBadBlocks ((game & badBlocks .~ (S.fromList ([(V2 4 8)]))) & player .~ (V2 4 0))) ^. badBlocks) (S.fromList ([(V2 4 8)]))),
            TestCase
              (assertEqual "Move Block" ((moveBlocks (game & badBlocks .~ (S.fromList ([(V2 4 8)])))) ^. badBlocks) (S.fromList ([(V2 4 7)]))),
            TestCase
              (assertEqual "Update Delay" ((updateDelay game) ^. goodBlockDelay) 1)
          ]
      )
  putStrLn ("------ Tests Finished! ------")
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure