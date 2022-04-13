{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import API.Comment qualified
import API.Thread qualified
import Config
import Control.Monad.Logger
import Env
import Lib.Authentication qualified
import Lib.DB qualified as DB
import Lib.Prelude
import Lib.Scotty (bearer)
import Domain.Comment qualified
import Domain.Thread qualified
import Network.Wai.Middleware.RequestLogger
import Wai.Middleware.Comet qualified
import Web.Scotty

routes :: Env -> ScottyM ()
routes env = do
  get "/" $ file "frontend/index.html"
  get "/login" $ file "frontend/login.html"
  get "/style.css" $ file "frontend/style.css"
  get "/main.js" $ file "frontend/main.js"
  get "/threads/:thread" $ file "frontend/index.html"
  API.Thread.route env
  API.Comment.route env


main :: IO ()
main = do
  config@Config{..} <- getConfig
  (initBearer, requireAuth) <- bearer $ Lib.Authentication.check config.password
  comet <- Wai.Middleware.Comet.create
  runStdoutLoggingT
    $ DB.with config.database
    $ \conn -> do
      DB.run conn $ DB.runMigration $ do
        Domain.Comment.migrateAll
        Domain.Thread.migrateAll
      liftIO $ scotty port $ do
        initBearer
        middleware logStdoutDev
        middleware comet
        routes Env{..}
