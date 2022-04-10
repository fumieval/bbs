{-# LANGUAGE OverloadedStrings #-}
module Domain.Tripcode (tripcode) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base64 qualified as Base64

tripcode :: T.Text -> T.Text -> T.Text
tripcode salt str
  | (name, input) <- T.break (=='#') str, not $ T.null input
  = mconcat [name, "◆", T.take 10 $ T.decodeUtf8 $ Base64.encode $ SHA256.hash $ T.encodeUtf8 $ salt <> input]
  | otherwise = str
