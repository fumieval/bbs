{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Database.Persist as DB
import Database.Persist.Sqlite as DB
import Database.Persist.TH as DB
import Types

type DBM = ReaderT SqlBackend IO

runDB :: MonadIO m => SqlBackend -> DBM a -> m a
runDB conn m = liftIO $ runReaderT m conn

share [mkPersist sqlSettings {DB.mpsFieldLabelModifier = const id}, mkMigrate "migrateAll"] [persistLowerCase|
Comment
    name Text
    date UTCTime
    content Text
    deriving Show Generic FromJSON ToJSON
|]

selectComments :: Maybe ISO8601UTCTime -> DBM [(DB.Key Comment, Comment)]
selectComments since = map (\(DB.Entity i val) -> (i, val))
    <$> selectList [CommentDate >=. t | ISO8601UTCTime t <- maybe [] pure since] []