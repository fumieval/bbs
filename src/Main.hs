{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import API.Comment qualified
import Config
import Control.Monad.Logger
import Env
import Lib.Authentication qualified
import Lib.Comet qualified
import Lib.DB qualified
import Lib.Prelude
import Lib.Scotty (rest, bearer)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

routes :: Env -> ScottyM ()
routes env = do
  get "/" $ file "frontend/index.html"
  get "/login" $ file "frontend/login.html"
  get "/style.css" $ file "frontend/style.css"
  get "/main.js" $ file "frontend/main.js"
  rest "/comments" $ API.Comment.rest env

main :: IO ()
main = do
  config@Config{..} <- getConfig
  (initBearer, requireAuth) <- bearer $ Lib.Authentication.check config.password
  comet <- Lib.Comet.new
  runStdoutLoggingT
    $ Lib.DB.with config.database
    $ \conn -> liftIO $ scotty port $ do
      initBearer
      middleware logStdoutDev
      routes Env{..}
