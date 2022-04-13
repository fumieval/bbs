module Env (Env(..)) where

import Config (Config)
import Lib.DB (SqlBackend)

data Env = Env
  { config :: Config
  , conn :: SqlBackend
  }
