{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GMB.WebServer(webServer,ServerData(..)) where

import           Control.Lens           (makeLenses, view, (^.))
import           Control.Monad          (return)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, asks, runReaderT)
import           Data.Function          (($))
import           Data.Text              (Text)
import           GMB.Gitlab             (GitlabEvent)
import           GMB.Matrix             (MatrixContext)
import           GMB.RepoMapping        (RepoMapping)
import           Prelude                ()
import           System.IO              (IO)
import           Web.Scotty             (Parsable (..), addHeader, body, delete,
                                         file, header, json, jsonData, matchAny,
                                         middleware, next, param, params, post,
                                         scotty, setHeader, status, text)

data ServerData = ServerData {
    _sdCallback      :: GitlabEvent -> IO ()
  }

makeLenses ''ServerData

webServer port serverData = scotty port $ do
  post "/" $ do
    content <- jsonData
    liftIO ((serverData ^. sdCallback) content)
    return ()
