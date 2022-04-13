{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module API.Comment where

import Config
import Control.Monad (join)
import Data.Time (getCurrentTime)
import Database.Persist qualified as DB
import Domain.Comment
import Domain.Thread
import Domain.Tripcode
import Env
import Lib.Prelude
import Lib.DB qualified as DB 
import Lib.Scotty
import Lib.Types
import Network.HTTP.Types
import Web.Scotty

data QueryCommentReq = QueryCommentReq
  { since :: Maybe UTCTime
  } deriving Generic
  deriving anyclass (FromJSON, ToJSON)

data PostCommentReq = PostCommentReq
  { name :: Text
  , content :: Text
  } deriving Generic
  deriving anyclass (FromJSON, ToJSON)

route :: Env -> ScottyM ()
route Env{..} = rest "/threads/:thread/comments" REST
  { scope = pure ()
  , query = do
    tid <- param "thread"
    QueryCommentReq{..} <- jsonData

    join $ DB.run conn $ DB.getBy (UniqueIdent tid) >>= \case
      Just thread -> do
        let flt = (CommentThread DB.==. DB.entityKey thread)
              : [CommentDate DB.>. t | t <- maybe [] pure since]
        comments <- DB.selectList flt []
        pure $ json $ map (\(DB.Entity _ val) -> val) comments
      Nothing -> pure $ raiseStatus status404 "thread not found"
  , post = do
    tid <- param "thread"
    PostCommentReq{..} <- jsonData
    now <- liftIO getCurrentTime
    join $ DB.run conn $ DB.getBy (UniqueIdent tid) >>= \case
      Just thread -> do
        count <- DB.count [ CommentThread DB.==. DB.entityKey thread ]
        _ <- DB.insert Comment
          { thread = DB.entityKey thread
          , seqNo = count + 1
          , name = tripcode config.salt name
          , date = now
          , content = content
          }
        pure $ json ()
      Nothing -> pure $ raiseStatus status404 "thread not found"
  }