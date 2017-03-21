{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GMB.IncomingMessage
  (parseIncomingMessage
  ,coparseIncomingMessage
  ,constructIncomingMessage
  ,bodyStart
  ,bodyEnd
  ,bodySurround
  ,IncomingMessage
  ,plainBody
  ,markupBody)
  where

import Control.Lens (Lens', Getter, Prism', prism', to, _1, _2)
import Data.Eq (Eq)
import Data.Foldable (foldMap)
import Data.Function ((.))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import GMB.Util (breakOnMaybe)
import Prelude ()
import Text.Show (Show)
import Data.Functor ((<$>))

data IncomingMessage =
    IncomingMessage (Text.Text, Maybe Text.Text)
    deriving (Eq,Show)

plainBody :: Lens' IncomingMessage Text.Text
plainBody f (IncomingMessage (t,u)) = (IncomingMessage . (, u)) <$> f t

markupBody :: Lens' IncomingMessage (Maybe Text.Text)
markupBody f (IncomingMessage (u,t)) = (IncomingMessage . (u, )) <$> f t

bodyStart = "<body>"

bodyEnd = "</body>"

bodySurround t = bodyStart <> t <> bodyEnd

parseIncomingMessage :: Text.Text -> IncomingMessage
parseIncomingMessage mb =
    case breakOnMaybe bodyStart mb of
        Nothing -> IncomingMessage (mb, Nothing)
        Just (_,bodyText) ->
            case breakOnMaybe bodyEnd bodyText of
                Nothing -> IncomingMessage (mb, Nothing)
                Just (bodyTextTillEnd,bodyEnd) ->
                    IncomingMessage
                        ( Text.drop 7 bodyEnd
                        , Just (Text.drop 6 bodyTextTillEnd))

constructIncomingMessage :: Text.Text -> Maybe Text.Text -> IncomingMessage
constructIncomingMessage plain markup = IncomingMessage (plain, markup)

coparseIncomingMessage :: IncomingMessage -> Text.Text
coparseIncomingMessage (IncomingMessage (plain,markup)) =
    (foldMap bodySurround markup) <> plain
