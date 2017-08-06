module Main where

import Conway
import Control.Concurrent

main :: IO ()
main = loop basicGrid

loop :: Grid Bool -> IO ()
loop g = do
  putStr "\ESC[2J"
  putStrLn (render g)
  threadDelay 200000
  loop (next g)
