{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
import           Control.Monad.IO.Class
import qualified Database.Persist as DB
import qualified Database.Persist.Sqlite as DB
import qualified Database.Persist.TH as DB
import           Web.Scotty
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format.ISO8601
import           Deriving.Aeson.Stock
import           Control.Monad.Trans.Reader
import           Control.Monad.Logger
import           GHC.Generics (Generic)
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Foldable
import           Control.Applicative
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base64 as Base64

DB.share [DB.mkPersist DB.sqlSettings, DB.mkMigrate "migrateAll"] [DB.persistLowerCase|
Comment
    name Text
    date UTCTime
    content Text
    deriving Show
|]

newtype Result e a = Result { unResult :: Either e a }
    deriving (Functor, Applicative, Monad)
instance IsString e => MonadFail (Result e) where
    fail = Result . Left . fromString

newtype ISO8601UTCTime = ISO8601UTCTime UTCTime

instance Parsable ISO8601UTCTime where
  parseParam = unResult . fmap ISO8601UTCTime . iso8601ParseM . TL.unpack

data CommentReq = CommentReq
    { crName :: Text
    , crContent :: Text
    } deriving Generic
    deriving (FromJSON, ToJSON) via PrefixedSnake "cr" CommentReq

deriving instance Generic Comment
deriving via PrefixedSnake "comment" Comment instance FromJSON Comment
deriving via PrefixedSnake "comment" Comment instance ToJSON Comment

tripcode :: Text -> Text -> Text
tripcode salt str
    | (name, input) <- T.break (=='#') str, not $ T.null input
    = mconcat [name, "◆", T.take 10 $ T.decodeUtf8 $ Base64.encode $ SHA256.hash $ T.encodeUtf8 $ salt <> input]
    | otherwise = str
    
main :: IO ()
main = runStdoutLoggingT $ DB.withSqliteConn "production.sqlite" $ \conn -> do
    let runDB :: MonadIO m => ReaderT DB.SqlBackend IO a -> m a
        runDB m = liftIO $ runReaderT m conn
    runDB $ DB.runMigration migrateAll
    vCounter <- liftIO $ newTVarIO (0 :: Int)
    liftIO $ scotty 8812 $ do
        get "/" $ file "index.html"
        options "/comments" $ text ""
        get "/comments" $ do
            wait <- optional $ param "wait"
            when (wait == Just (1 :: Int)) $ do
                current <- liftIO $ readTVarIO vCounter
                liftIO $ atomically $ do
                    i <- readTVar vCounter
                    when (i <= current) retry

            since <- optional $ param "since"
            comments <- runDB $ DB.selectList [CommentDate DB.>=. t | ISO8601UTCTime t <- toList since] []
            json $ map (\(DB.Entity i val) -> (i, val)) (comments :: [DB.Entity Comment])
        post "/comments" $ do
            body <- jsonData
            now <- liftIO getCurrentTime
            runDB $ DB.insert Comment
                { commentName = tripcode "re1z0uk0" $ crName body
                , commentDate = now
                , commentContent = crContent body
                }
            liftIO $ atomically $ modifyTVar' vCounter (+1)
            json ()