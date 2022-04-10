{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Domain.Comment where

import Database.Persist
import Lib.DB
import Lib.Types

declare [persistLowerCase|
Comment
    name Text
    date UTCTime
    content Text
    deriving Show Generic FromJSON ToJSON
|]

selectComments :: Maybe UTCTime -> DBM [(Key Comment, Comment)]
selectComments since = map (\(Entity i val) -> (i, val))
  <$> selectList [CommentDate >=. t | t <- maybe [] pure since] []
