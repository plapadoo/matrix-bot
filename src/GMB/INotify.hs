module GMB.INotify(
    watchDirectoryRecursive
  , recursiveSubdirs
  , stopWatch
  , Monitor
  , Event(..)) where

import System.INotify(initINotify,killINotify,INotify,addWatch,EventVariety(AllEvents),Event(..))
import Data.Monoid((<>))
import Data.Function(($),(.))
import Prelude(undefined)
import Text.Show(show)
import System.FilePath(FilePath,(</>))
import Data.Bool(Bool(..))
import Control.Concurrent.Chan(Chan,newChan,writeChan,readChan)
import Control.Concurrent(forkIO)
import Control.Monad(void,return,(>>=),forM,forM_,filterM,liftM,(>>))
import Control.Monad.IO.Class(MonadIO,liftIO)
import System.IO(IO,print,putStrLn)
import System.Directory(listDirectory,doesDirectoryExist)
import Data.List(filter,concat)
import Data.Functor((<$>))

data Monitor = Monitor {
    monitorStop :: Chan ()
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

watchDirectoryRecursive' :: MonadIO m => INotify -> FilePath -> (FilePath -> Event -> IO ()) -> m ()
watchDirectoryRecursive' inotify fp cb = do
  subDirs <- recursiveSubdirs fp
  forM_ (fp : subDirs) $ \dir -> do
    liftIO $ putStrLn $ "Watching directory: " <> dir
    liftIO $ void $ addWatch inotify [AllEvents] dir (cb dir)

watchDirectoryRecursive :: MonadIO m => FilePath -> (FilePath -> Event -> IO ()) -> m Monitor
watchDirectoryRecursive fp cb = do
  inotify <- liftIO $ initINotify
  stopChan <- liftIO newChan
  stoppedChan <- liftIO newChan
  let innerCb filePath event =
        case event of
          Created True path -> do
            liftIO $ putStrLn $ "directory was created:" <> (filePath </> path)
            watchDirectoryRecursive' inotify (filePath </> path) innerCb
            cb (filePath </> path) event
          _ -> cb filePath event
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

