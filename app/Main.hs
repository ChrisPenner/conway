module Main where

import Conway
import Control.Concurrent

tickTime :: Int
tickTime = 200000

start :: Grid Bool
start = mkGrid $
     glider `at` (0, 0)
  ++ beacon `at` (15, 5)

main :: IO ()
main = loop (step basicRule) start

loop :: (Grid Bool -> Grid Bool) -> Grid Bool -> IO ()
loop stepper g = do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render g)
  threadDelay tickTime
  loop stepper (stepper g)
