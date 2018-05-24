{-# LANGUAGE DeriveGeneric #-}

module Pos.Util.LogSeverity
       ( Severity(..)
       ) where

import          Data.Yaml   as Y
import          GHC.Generics

import           Universum



-- | abstract libraries' severity
data Severity = Debug | Info | Warning | Notice | Error
                deriving (Generic, Show)

-- | Handwritten 'FromJSON' instance because the log config files
--   contain a '+' after their severity that has to be dropped to 
--   be parsed into our Severity datatype.
instance FromJSON Severity where
    parseJSON (Object v) = Severity <$>
        (init $ v .: "severity")

