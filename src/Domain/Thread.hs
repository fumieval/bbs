{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Domain.Thread where

import Lib.DB
import Lib.Types

import Database.Persist.Sqlite
import Domain.Ident
import Lib.Scotty ()

declare [persistLowerCase|
Thread
    ident (Ident Thread)
    name Text
    createdAt UTCTime
    updatedAt UTCTime
    UniqueIdent ident
    deriving Show Generic FromJSON ToJSON
|]

instance IdentPrefix Thread where
  identPrefix = "T-"