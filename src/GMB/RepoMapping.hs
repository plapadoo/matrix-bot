{-# LANGUAGE OverloadedStrings #-}
module GMB.RepoMapping(RepoMapping,readRepoMapping) where

import           Control.Applicative    ((*>), (<*), (<*>))
import           Control.Monad          (return)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.Text   as Atto
import           Data.Either            (Either)
import           Data.Eq                ((/=), (==))
import           Data.Foldable          (foldMap)
import           Data.Functor           ((<$>))
import           Data.Map               (Map, singleton)
import           Data.String            (String)
import           Data.Text              (Text)
import           Data.Text.IO           (readFile)
import           Prelude                (undefined)
import           System.FilePath

type RepoMapping = Map Text [Text]

data RepoMappingLine = RepoMappingLine Text [Text]

linesToMapping :: [RepoMappingLine] -> RepoMapping
linesToMapping = foldMap (\(RepoMappingLine key values) -> singleton key values)

lineParser :: Atto.Parser RepoMappingLine
lineParser =
  RepoMappingLine <$> (Atto.skipSpace *> (Atto.takeWhile (/= '=')) )
                  <*> ((Atto.skipSpace *> Atto.takeWhile (/= ',') <* Atto.skipSpace) `Atto.sepBy` (Atto.char ','))

fileParser :: Atto.Parser [RepoMappingLine]
fileParser = lineParser `Atto.sepBy` Atto.char '\n' <* (Atto.char '\n' Atto.<?> "final newline")

readRepoMapping :: MonadIO m => FilePath -> m (Either String RepoMapping)
readRepoMapping fileName = do
  file <- liftIO (readFile fileName)
  return (linesToMapping <$> (Atto.parseOnly fileParser file))
