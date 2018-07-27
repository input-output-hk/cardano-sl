module Pos.Core.Update.ApplicationName
       ( ApplicationName (..)
       , applicationNameMaxLength
       , checkApplicationName
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson.TH (deriveToJSON, defaultOptions)
import           Data.Aeson (FromJSON (..))
import           Data.Char (isAscii)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text as T

import           Pos.Binary.Class (Bi (..))

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable, NFData)

instance Bi ApplicationName where
    encode appName = encode (getApplicationName appName)
    decode = ApplicationName <$> decode

instance FromJSON ApplicationName where
    -- FIXME does the defaultOptions derived JSON encode directly as text? Or
    -- as an object with a single key?
    parseJSON v = ApplicationName <$> parseJSON v

-- | Smart constructor of 'ApplicationName'.
checkApplicationName :: MonadError Text m => ApplicationName -> m ()
checkApplicationName (ApplicationName appName)
    | length appName > applicationNameMaxLength =
        throwError "ApplicationName: too long string passed"
    | T.any (not . isAscii) appName =
        throwError "ApplicationName: not ascii string passed"
    | otherwise = pure ()

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 12

deriveToJSON defaultOptions ''ApplicationName

deriveSafeCopySimple 0 'base ''ApplicationName
