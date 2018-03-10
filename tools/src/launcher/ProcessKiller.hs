{-# OPTIONS_GHC -cpp #-}

module ProcessKiller
        ( stopProcess,
          killProcess
        ) where

import Control.Concurrent.MVar (readMVar)

#ifdef mingw32_HOST_OS
import System.Win32.Console (generateConsoleCtrlEvent, cTRL_C_EVENT)
import System.Win32.Process (getProcessId)
import qualified System.Process as P
import qualified System.Process.Internals as Pi
#else
import System.Posix.Signals hiding (killProcess)
import System.Process (terminateProcess)
import System.Process.Internals (ProcessHandle__(..),
                                 ProcessHandle(..),
                                 withProcessHandle)
#endif

#ifdef mingw32_HOST_OS
stopProcess :: P.ProcessHandle -> IO ()
stopProcess ph = do
  pid <- getPid ph
  case pid of
    Nothing -> putStrLn "wtf"
    Just pD -> do
      putStrLn "Stop me, oh, stop me"
      generateConsoleCtrlEvent cTRL_C_EVENT pD
  return ()
  where
    getPid (Pi.ProcessHandle mh _) = do
      p_ <- readMVar mh
      case p_ of
        Pi.OpenHandle h -> do
          pid <- getProcessId h
          return $ Just pid
        _ -> return Nothing

killProcess :: P.ProcessHandle -> IO ()
killProcess ph = do
  putStrLn "Murder most foul, as in the best it is, but this most foul, strange, and unnatural"
  P.terminateProcess ph

#else
stopProcess :: ProcessHandle -> IO ()
stopProcess ph = do
  putStrLn "Stop me, oh, stop me"
  terminateProcess ph

killProcess :: ProcessHandle -> IO ()
killProcess ph = do
  withProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle _ -> return ()
      OpenHandle h -> do
        putStrLn "Murder most foul, as in the best it is, But this most foul, strange, and unnatural."
        signalProcess sigKILL h
#endif
