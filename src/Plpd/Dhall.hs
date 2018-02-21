module Plpd.Dhall(
    toText
  , toString
  ) where

import           Data.Function          ((.))
import           Data.String            (String)
import qualified Data.Text              as Text
import           Data.Text.Buildable    (build)
import qualified Data.Text.Lazy         as TextLazy
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Dhall                  as Dhall
import           Prelude                ()

toText :: Dhall.Text -> Text.Text
toText = TextLazy.toStrict . toLazyText . build

toString :: Dhall.Text -> String
toString = Text.unpack . toText
