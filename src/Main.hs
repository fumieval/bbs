{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (getCurrentTime)
import Database.Persist qualified as DB
import Database.Persist.Sqlite qualified as DB
import Network.HTTP.Types
import Web.Scotty

import Auth
import Config
import Model
import Types
import Util
import Panfiguration (Secret(..))

data CommentReq = CommentReq
    { name :: Text
    , content :: Text
    } deriving Generic
instance FromJSON CommentReq
instance ToJSON CommentReq

data Env = Env
    { config :: Config
    , conn :: DB.SqlBackend
    , vCounter :: TVar Int
    }

waitForChange :: Env -> IO ()
waitForChange Env{..} = do
    current <- readTVarIO vCounter
    atomically $ do
        i <- readTVar vCounter
        when (i <= current) retry

notifyChange :: Env -> IO ()
notifyChange Env{..} = atomically $ modifyTVar' vCounter (+1)

routes :: Env -> ScottyM ()
routes Env{..} = do
    get "/" $ file "index.html"
    options "/comments" $ text ""
    get "/comments" $ do
        requireAuth

        wait <- optional $ param "wait"
        when (wait == Just (1 :: Int)) $ liftIO $ waitForChange Env{..}
        since <- optional $ param "since"
        comments <- runDB conn $ selectComments since
        json comments 
    post "/comments" $ do
        requireAuth

        CommentReq{..} <- jsonData
        now <- liftIO getCurrentTime
        _ <- runDB conn $ DB.insert Comment
            { name = tripcode config.salt name
            , date = now
            , content = content
            }
        liftIO $ notifyChange Env{..}
        json ()

requireAuth :: ActionM ()
requireAuth = do
    token <- header "Authorization"
    when (token == Nothing) $ do
        raiseStatus status403 "unauthorised"

main :: IO ()
main = do
    config@Config{..} <- getConfig
    auth <- volatileTokenAuthorisation $ unSecret config.password
    runStdoutLoggingT $ DB.withSqliteConn sqlite $ \conn -> do
        runDB conn $ DB.runMigration migrateAll
        vCounter <- liftIO $ newTVarIO (0 :: Int)
        liftIO $ scotty port $ do
            middleware auth
            routes Env{..}
