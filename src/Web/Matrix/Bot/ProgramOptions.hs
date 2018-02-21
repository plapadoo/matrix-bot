{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.Bot.ProgramOptions
  (ProgramOptions(..)
  ,readProgramOptions
  ,poConfigFile)
  where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid            ((<>))
import qualified Options.Applicative    as OptAppl
import           System.FilePath        (FilePath)

data ProgramOptions = ProgramOptions
    { _poConfigFile :: FilePath
    }

makeLenses ''ProgramOptions

programOptionsParser :: OptAppl.Parser ProgramOptions
programOptionsParser =
    ProgramOptions <$>
    OptAppl.strOption
        (OptAppl.long "config-file" <>
         OptAppl.help "Where to put the config file" <>
         OptAppl.value "/etc/matrix-bot/matrix-bot.dhall")

readProgramOptions
    :: MonadIO m
    => m ProgramOptions
readProgramOptions = liftIO (OptAppl.execParser opts)
  where
    opts =
        OptAppl.info
            (OptAppl.helper <*> programOptionsParser)
            (OptAppl.fullDesc <>
             OptAppl.progDesc
                 "Listen for messages via HTTP POST, send them to a matrix channel" <>
             OptAppl.header "matrix-bot - send stuff to matrix")
