{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import API.Comment qualified
import API.Thread qualified
import Config
import Control.Monad.Logger
import Env
import Lib.DB qualified as DB
import Lib.Prelude
import Domain.Comment qualified
import Domain.Thread qualified
import Data.HashMap.Strict qualified as HM
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Auth
import Network.Wai.Middleware.Auth.Provider
import Network.Wai.Middleware.Auth.OAuth2.Google
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
  comet <- Wai.Middleware.Comet.create
  let google = mkGoogleProvider
        googleOauthClientId
        googleOauthClientSecret
        [googleOauthWhitelist]
        Nothing
  auth <- mkAuthMiddleware
    $ setAuthProviders (HM.singleton "google" $ Provider google)
    $ defaultAuthSettings
  runStdoutLoggingT
    $ DB.with config.database
    $ \conn -> do
      DB.run conn $ DB.runMigration $ do
        Domain.Comment.migrateAll
        Domain.Thread.migrateAll
      liftIO $ scotty port $ do
        middleware logStdoutDev
        middleware auth
        middleware comet
        routes Env{..}
