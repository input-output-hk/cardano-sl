{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pos.Util.LogSeverity
       ( Severity(..)
       ) where

import          Data.HashMap.Strict  as H
import          Data.Yaml   as Y
import          GHC.Generics

import           Universum


data Level = Debug | Info | Warning | Notice | Error
                deriving (Generic, Show)

instance FromJSON Level

newtype Severity = Severity { level :: !Level }
                deriving (Generic, Show)

-- | Handwritten 'FromJSON' instance because the log config files
--   contain a '+' after their severity that has to be dropped to 
--   be parsed into our Severity datatype.
instance FromJSON Severity where
    parseJSON  (Object v) =  
        \v ->  case H.lookup "severity" obj of    
            Nothing -> fail ("key " ++ show key ++ " not present")
            Just v  -> case v of
                "Debug+"   -> pure  Debug
                "Info+"    -> pure  Info
                "Notice+"  -> pure  Notice
                "Warning+" -> pure  Warning
                "Error+"   -> pure  Error 
                _          -> fail  $ toString $ "Unknown Severity"

