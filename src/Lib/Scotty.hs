{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Scotty where

import Control.Monad.State.Strict
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Types
import Network.Wai
import Web.Scotty hiding (post)
import Web.Scotty.Internal.Types

addHeader' :: Header -> ActionM ()
addHeader' kv = ActionT
  $ modify
  $ \sr -> sr { srHeaders = kv : srHeaders sr }

data REST = REST
  { scope :: ActionM ()
  , query :: ActionM ()
  , post :: ActionM ()
  }

rest :: RoutePattern -> REST -> ScottyM ()
rest route REST{..} = do
  addroute OPTIONS route $ text ""
  addroute QUERY route $ scope >> query
  addroute POST route $ scope >> post
