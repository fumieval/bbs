
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Config (Config, ConfigH, ConfigB(..), getConfig) where
import Barbies.TH
import Panfiguration
import Lib.Types

passthroughBareB [d|
  data Config = Config
    { port :: Int
    , database :: Text
    , salt :: Text
    , password :: Secret ByteString
    }
  |]

getConfig :: IO Config
getConfig = run $ envs <> opts <> fullDefaults
  Config { port = 8812, database = "dev.sqlite3", salt = "", password = Secret "" }