{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module API.Comment where

import Config
import Data.Time (getCurrentTime)
import Database.Persist qualified as DB
import Domain.Comment
import Domain.Tripcode
import Env
import Lib.Prelude
import Lib.DB qualified as DB 
import Lib.Scotty
import Lib.Types
import Web.Scotty

data QueryCommentReq = QueryCommentReq
  { wait :: Bool
  , since :: Maybe UTCTime
  } deriving Generic
  deriving anyclass (FromJSON, ToJSON)

data PostCommentReq = PostCommentReq
  { name :: Text
  , content :: Text
  } deriving Generic
  deriving anyclass (FromJSON, ToJSON)

rest :: Env -> REST
rest Env{..} = REST
  { scope = requireAuth
  , query = do
    QueryCommentReq{..} <- jsonData
    when wait $ liftIO comet.wait

    comments <- DB.run conn $ selectComments since
    json comments
  , post = do
    PostCommentReq{..} <- jsonData
    now <- liftIO getCurrentTime
    _ <- DB.run conn $ DB.insert Comment
        { name = tripcode config.salt name
        , date = now
        , content = content
        }
    liftIO comet.notify
    json ()
  }