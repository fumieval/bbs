module Lib.DB
    ( DBM
    , SqlBackend
    , run
    , with
    , declare
    , persistLowerCase
    , runMigration
    ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Database.Persist.Quasi.Internal
import Database.Persist.Sqlite
import Database.Persist.TH
import Language.Haskell.TH (DecsQ)
import Lib.Types
import UnliftIO

type DBM = ReaderT SqlBackend IO

run :: MonadIO m => SqlBackend -> DBM a -> m a
run conn m = liftIO $ runReaderT m conn

with :: (MonadLoggerIO m, MonadUnliftIO m) => Text -> (SqlBackend -> m a) -> m a
with = withSqliteConn

declare :: [UnboundEntityDef] -> DecsQ
declare = share [mkPersist sqlSettings {mpsFieldLabelModifier = const id}, mkMigrate "migrateAll"]