{-# LANGUAGE DeriveGeneric #-}
module GMB.Gitlab(
  GitlabEvent,
  GitlabCommit,
  GitlabRepository,
  eventObjectAttributes,
  eventRepository,
  commitMessage,
  eventUserName,
  objectTitle,
  objectNote,
  objectUrl,
  eventObjectKind,
  eventCommits,
  repositoryName) where

import Prelude()
import GHC.Generics(Generic)
import Data.Maybe(Maybe)
import Data.Text(Text)
import Data.Aeson(FromJSON)

data GitlabRepository = GitlabRepository {
    name :: Text
  } deriving(Generic)

data GitlabCommit = GitlabCommit {
    message :: Text
  } deriving(Generic)

commitMessage :: GitlabCommit -> Text
commitMessage = message

data GitlabObjectAttributes = GitlabObjectAttributes {
    title :: Text
  , url :: Text
  , note :: Text
  } deriving(Generic)

objectTitle :: GitlabObjectAttributes -> Text
objectTitle = title

objectNote :: GitlabObjectAttributes -> Text
objectNote = note

objectUrl :: GitlabObjectAttributes -> Text
objectUrl = url

data GitlabEvent = GitlabEvent {
    object_kind :: Text
  , user_name :: Maybe Text
  , repository :: Maybe GitlabRepository
  , commits :: Maybe [GitlabCommit]
  , object_attributes :: Maybe GitlabObjectAttributes
  } deriving(Generic)

eventObjectAttributes :: GitlabEvent -> Maybe GitlabObjectAttributes
eventObjectAttributes = object_attributes

eventObjectKind :: GitlabEvent -> Text
eventObjectKind = object_kind

eventCommits :: GitlabEvent -> Maybe [GitlabCommit]
eventCommits = commits

eventRepository :: GitlabEvent -> Maybe GitlabRepository
eventRepository = repository

eventUserName :: GitlabEvent -> Maybe Text
eventUserName = user_name

repositoryName :: GitlabRepository -> Text
repositoryName = name

instance FromJSON GitlabCommit
instance FromJSON GitlabRepository
instance FromJSON GitlabEvent
instance FromJSON GitlabObjectAttributes
