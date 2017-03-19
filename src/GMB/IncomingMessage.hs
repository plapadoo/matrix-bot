{-# LANGUAGE OverloadedStrings #-}
module GMB.IncomingMessage(
    parseIncomingMessage
  , coparseIncomingMessage
  , constructIncomingMessage
  , bodyStart
  , bodyEnd
  , bodySurround
  , IncomingMessage
  , plainBody
  , markupBody
  ) where

import           Control.Lens (Getter, to,Prism',prism')
import           Data.Eq      (Eq)
import           Data.Maybe   (Maybe (..), maybe)
import           Data.Monoid  ((<>))
import qualified Data.Text    as Text
import           GMB.Util     (breakOnMaybe)
import           Prelude      ()
import           Text.Show    (Show)

data IncomingMessage = MessageWithMarkup Text.Text Text.Text
                     | MessageWithoutMarkup Text.Text deriving(Eq,Show)

plainBody :: Getter IncomingMessage Text.Text
plainBody = to f
  where f (MessageWithMarkup _ plain)  = plain
        f (MessageWithoutMarkup plain) = plain

markupBody :: Getter IncomingMessage (Maybe Text.Text)
markupBody = to f
  where f (MessageWithMarkup markup _) = Just markup
        f (MessageWithoutMarkup _)     = Nothing

bodyStart = "<body>"
bodyEnd = "</body>"

bodySurround t = bodyStart <> t <> bodyEnd

parseIncomingMessage :: Text.Text -> IncomingMessage
parseIncomingMessage mb =
  case breakOnMaybe bodyStart mb of
    Nothing -> MessageWithoutMarkup mb
    Just (_,bodyText) ->
      case breakOnMaybe bodyEnd bodyText of
        Nothing -> MessageWithoutMarkup mb
        Just (bodyTextTillEnd,bodyEnd) -> MessageWithMarkup (Text.drop 6 bodyTextTillEnd) (Text.drop 7 bodyEnd)

constructIncomingMessage :: Text.Text -> Maybe Text.Text -> IncomingMessage
constructIncomingMessage plain markup = maybe (MessageWithoutMarkup plain) (\mu -> MessageWithMarkup mu plain) markup

coparseIncomingMessage :: IncomingMessage -> Text.Text
coparseIncomingMessage (MessageWithMarkup markup plain) = bodySurround markup <> plain
coparseIncomingMessage (MessageWithoutMarkup plain) = plain
