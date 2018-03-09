module Main where

import ProcessKiller (killProcess)
import Control.Concurrent (threadDelay)
import System.Process (runCommand)


main :: IO ()
main = do
  pr <- runCommand $ "./Main"
  threadDelay 1000000
  killProcess pr
