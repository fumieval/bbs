module Env (Env(..)) where

import Config (Config)
import Lib.DB (SqlBackend)
import Lib.Comet (Comet)
import Lib.Authentication (Credential)
import Web.Scotty (ActionM)

data Env = Env
  { config :: Config
  , conn :: SqlBackend
  , comet :: Comet
  , requireAuth :: ActionM Credential
  }