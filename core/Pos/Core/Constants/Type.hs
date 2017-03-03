{-# LANGUAGE DeriveLift #-}

-- | Type 'CoreConstants', which has to be in a separate module due to GHC
-- stage restriction. See "Pos.Core.Constants".
module Pos.Core.Constants.Type
       ( CoreConstants(..)
       ) where

import           Data.Aeson                 (FromJSON (..), genericParseJSON)
import           Data.Tagged                (Tagged (..))
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Aeson.Options     (defaultOptions)
import           Universum

import           Pos.Util.Config            (IsConfig (..))

data CoreConstants = CoreConstants
    {
      -- | Security parameter from paper
      ccK                          :: !Int
    , -- | Magic constant for separating real/testnet
      ccProtocolMagic              :: !Int32
    , -- | Start time of network (in @Production@ running mode). If set to
      -- zero, then running time is 2 minutes after build.
      ccProductionNetworkStartTime :: !Int
    }
    deriving (Show, Lift, Generic)

instance FromJSON CoreConstants where
    parseJSON = genericParseJSON defaultOptions

instance IsConfig CoreConstants where
    configPrefix = Tagged Nothing
