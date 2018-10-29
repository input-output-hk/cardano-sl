module Pos.Chain.Update.ApplicationName
       ( ApplicationName (..)
       , applicationNameMaxLength
       , checkApplicationName
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson (FromJSON (..))
import           Data.Aeson.TH (defaultOptions, deriveToJSON)
import           Data.Char (isAscii)
import           Data.List ((!!))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text as T
import           Test.QuickCheck

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

instance Arbitrary ApplicationName where
    arbitrary =
        ApplicationName .
        toText . map selectAlpha . take applicationNameMaxLength <$>
        arbitrary
      where
        selectAlpha n = alphabet !! (n `mod` length alphabet)
        alphabet = "-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

deriveToJSON defaultOptions ''ApplicationName

deriveSafeCopySimple 0 'base ''ApplicationName
