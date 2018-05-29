module Pos.Util.Log.Severity
       ( Severity(..)
       ) where

import           Data.Yaml   as Y
import           GHC.Generics

import           Universum


data Severity = Debug | Info | Warning | Notice | Error
                deriving (Generic, Show)

-- | Handwritten 'FromJSON' instance because the log config files
--   contain a '+' after their severity that has to be dropped to
--   be parsed into our Severity datatype.
instance ToJSON Severity
instance FromJSON Severity where
    parseJSON = withText "severity" $ \case
                    "Debug+"   -> pure Debug
                    "Debug"    -> pure Debug
                    "Info+"    -> pure Info
                    "Info"     -> pure Info
                    "Notice+"  -> pure Notice
                    "Notice"   -> pure Notice
                    "Warning+" -> pure Warning
                    "Warning"  -> pure Warning
                    "Error+"   -> pure Error
                    "Error"    -> pure Error
                    _          -> pure Debug  -- catch all
