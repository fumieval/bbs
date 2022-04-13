{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module API.Thread (route) where

import Data.Time (getCurrentTime)
import Database.Persist qualified as DB
import Domain.Ident
import Domain.Thread
import Env
import Lib.Prelude
import Lib.DB qualified as DB 
import Lib.Scotty
import Lib.Types
import Web.Scotty

data PostThreadReq = PostThreadReq
  { name :: Text
  } deriving Generic
  deriving anyclass (FromJSON, ToJSON)

route :: Env -> ScottyM ()
route Env{..} = rest "/threads" REST
  { scope = requireAuth
  , query = do
    threads <- DB.run conn $ DB.selectList [] []
    json (map DB.entityVal threads :: [Thread])
  , post = do
    PostThreadReq{..} <- jsonData
    now <- liftIO getCurrentTime
    ident <- liftIO Domain.Ident.generate
    _ <- DB.run conn $ DB.insert Thread
        { ident = ident
        , name = name
        , createdAt = now
        , updatedAt = now
        }
    json ident
  }