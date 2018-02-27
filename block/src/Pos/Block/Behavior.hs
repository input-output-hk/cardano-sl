{-# LANGUAGE RankNTypes #-}
module Pos.Block.Behavior
       ( BlockBehavior (..)
       , ForgeHeaderParams (..)
       ) where

import           Universum

import qualified Data.Aeson as A
import           Data.Default (Default (..))
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Util.Util (toAesonError)

----------------------------------------------------------------------------
-- Types for the behavior config
----------------------------------------------------------------------------

-- | Block settings (a part of the behavior config).
--
-- The syntax of this config section is as follows:
--
-- @
-- block:
--     forgeHeader: Normal | WrongLeader
-- @
data BlockBehavior = BlockBehavior
    { -- | Block header forging settings
      bbForgeHeader :: !ForgeHeaderParams
    }
    deriving (Eq, Show, Generic)

data ForgeHeaderParams
    = HeaderNormal      -- ^ Do not forge anything
    | HeaderWrongLeader -- ^ Create blocks header with a wrong slot leader
    deriving (Eq, Show)

----------------------------------------------------------------------------
-- JSON/YAML parsing
----------------------------------------------------------------------------

instance A.FromJSON BlockBehavior where
    parseJSON = A.genericParseJSON defaultOptions

instance A.FromJSON ForgeHeaderParams where
    parseJSON = A.withText "ForgeHeaderParams" $ toAesonError . \case
        "Normal"          -> Right HeaderNormal
        "WrongLeader"     -> Right HeaderWrongLeader
        other    -> Left ("invalid value " <> show other <>
                          ", acceptable values are Normal|WrongLeader")

----------------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------------

instance Default BlockBehavior where
    def = BlockBehavior def

instance Default ForgeHeaderParams where
    def = HeaderNormal
