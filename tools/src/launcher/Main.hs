module Main where

import ProcessKiller (stopProcess)
import Control.Concurrent (threadDelay)
import System.Process (runCommand)


main :: IO ()
main = do
  pr <- runCommand $ "./Main"
  threadDelay 1000000
  stopProcess pr
