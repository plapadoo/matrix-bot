{-# LANGUAGE DeriveGeneric #-}
module GMB.Gitlab(
  GitlabEvent,
  GitlabCommit,
  GitlabRepository,
  eventObjectAttributes,
  eventRepository,
  commitMessage,
  eventUserName,
  eventUserUserName,
  objectTitle,
  objectState,
  objectNote,
  objectUrl,
  eventObjectKind,
  eventCommits,
  repositoryName) where

import           Data.Aeson   (FromJSON)
import           Data.Functor ((<$>))
import           Data.Maybe   (Maybe)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Prelude      ()

data GitlabRepository = GitlabRepository {
    name :: Text
  } deriving(Generic)

data GitlabCommit = GitlabCommit {
    message :: Text
  } deriving(Generic)

commitMessage :: GitlabCommit -> Text
commitMessage = message

data GitlabObjectAttributes = GitlabObjectAttributes {
    title :: Maybe Text
  , url   :: Text
  , note  :: Maybe Text
  , state :: Maybe Text
  } deriving(Generic)

objectTitle :: GitlabObjectAttributes -> Maybe Text
objectTitle = title

objectState :: GitlabObjectAttributes -> Maybe Text
objectState = state

objectNote :: GitlabObjectAttributes -> Maybe Text
objectNote = note

objectUrl :: GitlabObjectAttributes -> Text
objectUrl = url

data GitlabUser = GitlabUser {
    username :: Text
  } deriving(Generic)

data GitlabEvent = GitlabEvent {
    object_kind       :: Text
  , user              :: Maybe GitlabUser
  , user_name         :: Maybe Text
  , repository        :: Maybe GitlabRepository
  , commits           :: Maybe [GitlabCommit]
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

eventUserUserName :: GitlabEvent -> Maybe Text
eventUserUserName event = username <$> user event

repositoryName :: GitlabRepository -> Text
repositoryName = name

instance FromJSON GitlabCommit
instance FromJSON GitlabRepository
instance FromJSON GitlabEvent
instance FromJSON GitlabObjectAttributes
instance FromJSON GitlabUser
