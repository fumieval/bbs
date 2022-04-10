{-# LANGUAGE OverloadedStrings #-}
module Lib.Authentication where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Network.Wai
import Panfiguration (Secret(..))

type Credential = ()

check :: Secret B.ByteString -> Request -> IO (Either BL.ByteString Credential)
check (Secret password) req = do
  body <- BL.toStrict <$> strictRequestBody req
  pure $ if body == password
    then Right ()
    else Left "incorrect password"