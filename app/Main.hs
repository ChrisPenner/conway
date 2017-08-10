module Main where

import Conway
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

tickTime :: Int
tickTime = 200000

start :: Grid
start = mkGrid $
     glider `at` (0, 0)
  ++ beacon `at` (15, 5)

main :: IO ()
main = forM_ (iterate (step basicRule) start) $ \grid -> do
  putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render grid)
  threadDelay tickTime
