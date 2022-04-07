{-# LANGUAGE OverloadedStrings #-}
module Auth
  ( volatileTokenAuthorisation
  ) where

import Control.Concurrent
import Control.Monad (forever, unless)
import Crypto.Random (getRandomBytes)
import Data.ByteString.Lazy qualified as BL
import qualified Data.ByteString.Base64 as Base64
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Clock
import Network.HTTP.Types
import Network.Wai

volatileTokenAuthorisation :: String
  -> Text -> IO Middleware
volatileTokenAuthorisation webhook password = do
  vTokens <- newIORef M.empty

  let timeout = 3600

  -- expire tokens
  _ <- forkIO $ forever $ do
      now <- getMonotonicTime
      expired <- atomicModifyIORef' vTokens $ M.partition ((now<) . (+timeout))
      unless (null expired) $ putStrLn $ show (M.keys expired) <> " expired"
      threadDelay $ 10 * 1000 * 1000

  pure $ \app req sendResp -> case (requestMethod req, pathInfo req) of
    ("GET", ["login"]) -> do
      sendResp $ responseFile status200 [] "login.html" Nothing
    ("POST", ["login"]) -> do
      body <- T.decodeUtf8 . BL.toStrict <$> strictRequestBody req
      if password == body
        then do
          now <- getMonotonicTime
          token <- Base64.encode <$> getRandomBytes 64
          atomicModifyIORef' vTokens $ \m -> (M.insert token now m, ())
          sendResp $ responseLBS status200 [] $ BL.fromStrict token
        else sendResp $ responseLBS status403 [] "unauthorised"
    _ | Just token <- lookup "Authorization" $ requestHeaders req -> do
      m <- readIORef vTokens
      case M.lookup token m of
        Nothing -> sendResp $ responseLBS status403 [] "unauthorised"
        Just _ -> app req sendResp
    _ -> app req sendResp