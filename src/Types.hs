module Types (Text, UTCTime, Generic, ISO8601UTCTime(..)) where
import Control.Monad.Fail
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import Data.Text.Lazy qualified as TL
import Web.Scotty (Parsable(..))
import GHC.Generics (Generic)

newtype Result e a = Result { unResult :: Either e a }
    deriving (Functor, Applicative, Monad, Generic)
instance IsString e => MonadFail (Result e) where
    fail = Result . Left . fromString

newtype ISO8601UTCTime = ISO8601UTCTime UTCTime deriving Generic

instance Parsable ISO8601UTCTime where
  parseParam = unResult . fmap ISO8601UTCTime . iso8601ParseM . TL.unpack
