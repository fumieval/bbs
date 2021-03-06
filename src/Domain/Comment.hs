{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Domain.Comment where

import Database.Persist
import Domain.Thread (ThreadId)
import Lib.DB
import Lib.Types

declare [persistLowerCase|
Comment
    thread ThreadId
    seqNo Int
    name Text
    date UTCTime
    content Text
    deriving Show Generic FromJSON ToJSON
|]
