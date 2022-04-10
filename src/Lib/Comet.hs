{-# LANGUAGE DataKinds #-}
module Lib.Comet where

import Control.Concurrent.STM
import Control.Monad
import GHC.Records

newtype Comet = Comet (TVar Int)

new :: IO Comet
new = Comet <$> newTVarIO 0

instance HasField "wait" Comet (IO ()) where
  getField (Comet v) = do
    current <- readTVarIO v
    atomically $ do
        i <- readTVar v
        when (i <= current) retry

instance HasField "notify" Comet (IO ()) where
  getField (Comet v) = atomically $ modifyTVar' v (+1)
