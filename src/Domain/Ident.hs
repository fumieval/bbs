{-# LANGUAGE  AllowAmbiguousTypes #-}
module Domain.Ident (Ident(..), IdentPrefix(..), generate) where

import Crypto.Random (getRandomBytes)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Base64.URL qualified as Base64
import Data.Text qualified as T
import Database.Persist
import Database.Persist.Sqlite
import Lib.Types
import Web.Scotty (Parsable(..))

newtype Ident t = Ident { unIdent :: Text }
  deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, PersistField, PersistFieldSql, Parsable)

class IdentPrefix t where
  identPrefix :: Text

generate :: forall t. IdentPrefix t => IO (Ident t)
generate = Ident . (identPrefix @t <>) . T.pack . B.unpack . Base64.encode <$> getRandomBytes 6