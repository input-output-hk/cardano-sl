{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.Util.Util
       (
       -- Instances
       -- ** Lift Byte
       -- ** FromJSON Byte
       -- ** ToJSON Byte
       ) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Language.Haskell.TH.Syntax (Lift (..))
import           Serokell.Data.Memory.Units (Byte, fromBytes, toBytes)
import           Universum

instance Lift Byte where
    lift x = let b = toBytes x in [|fromBytes b :: Byte|]

instance FromJSON Byte where
    parseJSON = fmap fromBytes . parseJSON

instance ToJSON Byte where
    toJSON = toJSON . toBytes
