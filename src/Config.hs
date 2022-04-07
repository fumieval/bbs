
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Config (Config, ConfigH, ConfigB(..), getConfig) where
import Barbies.TH
import Panfiguration
import Data.Text (Text)

passthroughBareB [d|
    data Config = Config
        { port :: Int
        , sqlite :: Text
        , salt :: Text
        , password :: Secret Text
        }
    |]

getConfig :: IO Config
getConfig = run $ envs <> opts <> fullDefaults
    Config { port = 8812, sqlite = "dev.sqlite3", salt = "", password = Secret "1509" }