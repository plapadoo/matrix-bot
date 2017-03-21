{-# LANGUAGE OverloadedStrings #-}

module GMB.MonadLog where

import Data.Text (Text, breakOn, pack, unpack)
import Prelude ()

class MonadLog m  where
    putLog :: Text -> m ()
