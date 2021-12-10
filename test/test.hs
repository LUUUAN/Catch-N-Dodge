module Main where

import CnD
import Control.Lens hiding ((:<), (:>), (<|), (|>))
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
  counts <-
    runTestTT
      ( test
          [ testMovement,
            testInBound,
            testShift
          ]
      )
  putStrLn ("------ Tests Finished! ------")
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure