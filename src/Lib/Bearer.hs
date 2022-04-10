{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Lib.Bearer
  ( middleware
  ) where

import Control.Concurrent
import Control.Monad (forever, unless)
import Crypto.Random (getRandomBytes)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.IORef
import Data.Map.Strict qualified as M
import GHC.Clock
import Network.HTTP.Types
import Network.Wai

type Token = B.ByteString

data TokenKeeper a = TokenKeeper
  { expire :: IO ()
  , generate :: a -> IO Token
  , check :: Token -> IO (Maybe a)
  }

newTokenKeeper :: Double -> IO (TokenKeeper a)
newTokenKeeper timeout = do
  vTokens <- newIORef M.empty
  pure TokenKeeper
    { expire = do
      now <- getMonotonicTime
      expired <- atomicModifyIORef' vTokens $ M.partition ((now<) . (+timeout) . fst)
      unless (null expired) $ putStrLn $ show (M.keys expired) <> " expired"
    , generate = \a -> do
      now <- getMonotonicTime
      token <- ("Bearer "<>) . Base64.encode <$> getRandomBytes 64
      atomicModifyIORef' vTokens $ \m -> (M.insert token (now, a) m, token)
    , check = \token -> fmap snd . M.lookup token <$> readIORef vTokens
    }

middleware
  :: (Request -> IO (Either BL.ByteString a))
  -> IO (Middleware, Request -> IO (Header, Maybe a))
middleware checkCredential = do
  tokenKeeper <- newTokenKeeper 3600

  _ <- forkIO $ forever $ do
    tokenKeeper.expire
    threadDelay $ 10 * 1000 * 1000


  pure (\app req sendResp -> case (requestMethod req, pathInfo req) of
    ("POST", ["login"]) -> checkCredential req >>= \case
      Left e -> sendResp $ responseLBS status403 [] e
      Right a -> do
        token <- tokenKeeper.generate a
        sendResp $ responseLBS status200 [] $ BL.fromStrict token
    _ -> app req sendResp
    , \req -> case requestToken req of
      Nothing -> pure (wwwAuthenticate "token_required", Nothing)
      Just t -> tokenKeeper.check t <&> \case
        Nothing -> (wwwAuthenticate "invalid_token", Nothing)
        Just a -> (wwwAuthenticate "", Just a))

wwwAuthenticate realm = ("WWW-Authenticate", "Bearer realm=\"" <> realm <> "\"")

requestToken :: Request -> Maybe Token
requestToken = lookup hAuthorization . requestHeaders