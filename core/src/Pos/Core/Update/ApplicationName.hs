module Pos.Core.Update.ApplicationName
       ( ApplicationName (..)
       , applicationNameMaxLength
       , checkApplicationName
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Char (isAscii)
import qualified Data.Text as T

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable, NFData)

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
