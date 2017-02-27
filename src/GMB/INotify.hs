module GMB.INotify(
    watchDirectoryRecursive
  , recursiveSubdirs
  , stopWatch
  , Monitor
  , watchRecursiveBuffering
  , unbuffer
  , NotifyEvent(..)
  , Event(..)) where

import Data.Maybe(Maybe(..))
import           Control.Concurrent           (threadDelay)
import Control.Monad.Loops(unfoldM)
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Chan      (Chan, newChan, readChan,
                                               writeChan)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan,tryReadTChan)
import           Control.Monad                (filterM, forM, forM_, forever,
                                               liftM, return, void, (>>), (>>=))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.STM            (atomically,STM)
import           Data.Bool                    (Bool (..))
import           Data.Function                (($), (.))
import           Data.Functor                 ((<$>))
import           Data.List                    (concat, filter)
import           Data.Monoid                  ((<>))
import           Prelude                      (undefined, (*))
import           System.Directory             (doesDirectoryExist,
                                               listDirectory)
import           System.FilePath              (FilePath, (</>))
import           System.INotify               (Event (..),
                                               EventVariety (AllEvents),
                                               INotify, addWatch, initINotify,
                                               killINotify)
import           System.IO                    (IO, print, putStrLn)
import           Text.Show                    (show)

data Monitor = Monitor {
    monitorStop    :: Chan ()
  , monitorStopped :: Chan ()
  }

listDirectoryFull dir = do
  list <- liftIO (listDirectory dir)
  return $ (dir </>) <$> list

recursiveSubdirs :: MonadIO m => FilePath -> m [FilePath]
recursiveSubdirs dir = do
  files <- listDirectoryFull dir
  dirs <- filterM (liftIO . doesDirectoryExist) files
  liftM concat $ forM dirs $ \innerDir -> do
    subdirs <- recursiveSubdirs innerDir
    return (innerDir : subdirs)

data NotifyEvent = NotifyEvent FilePath Event

watchDirectoryRecursive' :: MonadIO m => INotify -> FilePath -> (NotifyEvent -> IO ()) -> m ()
watchDirectoryRecursive' inotify fp cb = do
  subDirs <- recursiveSubdirs fp
  forM_ (fp : subDirs) $ \dir -> do
    liftIO $ putStrLn $ "Watching directory: " <> dir
    liftIO $ void $ addWatch inotify [AllEvents] dir (\event -> cb (NotifyEvent dir event))

watchDirectoryRecursive :: MonadIO m => FilePath -> (NotifyEvent -> IO ()) -> m Monitor
watchDirectoryRecursive fp cb = do
  inotify <- liftIO $ initINotify
  stopChan <- liftIO newChan
  stoppedChan <- liftIO newChan
  let innerCb (NotifyEvent filePath event) =
        case event of
          Created True path -> do
            liftIO $ putStrLn $ "directory was created:" <> (filePath </> path)
            watchDirectoryRecursive' inotify (filePath </> path) innerCb
            cb $ NotifyEvent (filePath </> path) event
          _ -> cb $ NotifyEvent filePath event
  liftIO $ void $ forkIO $ do
    watchDirectoryRecursive' inotify fp innerCb
    liftIO $ putStrLn "waiting"
    liftIO $ void $ readChan stopChan
    liftIO $ killINotify inotify
    liftIO $ writeChan stoppedChan ()
  return (Monitor stopChan stoppedChan)

stopWatch :: MonadIO m => Monitor -> m ()
stopWatch m = do
  liftIO $ writeChan (monitorStop m) ()
  liftIO $ void $ readChan (monitorStopped m)


data RecursiveWatcher = RecursiveWatcher (TChan NotifyEvent)

watchRecursiveBuffering :: MonadIO m => FilePath -> m RecursiveWatcher
watchRecursiveBuffering fb = do
  chan <- liftIO newTChanIO
  watchDirectoryRecursive fb (\e -> atomically (writeTChan chan e))
  return (RecursiveWatcher chan)

unbuffer :: MonadIO m => RecursiveWatcher -> ([NotifyEvent] -> IO ()) -> m ()
unbuffer (RecursiveWatcher tchan) cb = forever $ do
  liftIO $ threadDelay (5000 * 1000)
  packet <- liftIO $ atomically (unfoldM (tryReadTChan tchan))
  liftIO $ cb packet
