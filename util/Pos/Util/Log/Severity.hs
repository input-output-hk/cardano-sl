{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pos.Util.Log.Severity
       ( Severity(..)
       ) where

import          Data.HashMap.Strict  as H
import          Data.Yaml   as Y
import          GHC.Generics

import           Universum


data Severity = Debug | Info | Warning | Notice | Error
                deriving (Generic, Show)

-- | Handwritten 'FromJSON' instance because the log config files
--   contain a '+' after their severity that has to be dropped to 
--   be parsed into our Severity datatype.
instance FromJSON Severity where
    parseJSON =  withObject "Severity" $ \o -> 
        case H.lookup "severity" o of    
            Nothing -> fail "key severity not present"
            Just v -> case v of
                "Debug+"   -> pure Debug
                "Info+"    -> pure $ Info 
                "Notice+"  -> pure $ Notice 
                "Warning+" -> pure $ Warning 
                "Error+"   -> pure $ Error
                _          -> fail "Unknown Severity"

