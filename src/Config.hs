
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
    , googleOauthClientId :: Text
    , googleOauthClientSecret :: Text
    , googleOauthWhitelist :: ByteString
    }
  |]

getConfig :: IO Config
getConfig = run $ envs <> opts <> defaults
  Config
  { port = Just 8812
  , database = Just "dev.sqlite3"
  , salt = Just ""
  , googleOauthClientId = Nothing
  , googleOauthClientSecret = Nothing
  , googleOauthWhitelist = Nothing
  }