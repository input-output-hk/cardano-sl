{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Pos.Ssc.Behavior
       ( SscBehavior (..)
       , SscOpeningParams (..)
       , SscSharesParams (..)
       ) where

import           Universum

import qualified Data.Aeson as A
import           Data.Aeson.Options (defaultOptions)
import           Data.Default (Default (..))

import           Pos.Util.Util (toAesonError)

----------------------------------------------------------------------------
-- Types for the behavior config
----------------------------------------------------------------------------

-- | SSC settings (a part of the behavior config).
--
-- The syntax of this config section is as follows:
--
-- @
-- ssc:
--     sendOpening: Normal | None | Wrong
--     sendShares: Normal | None | Wrong
-- @
data SscBehavior = SscBehavior
    { -- | Opening settings (e.g. whether to send rubbish instead of openings)
      sbSendOpening :: !SscOpeningParams
      -- | Shares settings (e.g. whether to send rubbish instead of shares)
    , sbSendShares  :: !SscSharesParams
    }
    deriving (Eq, Show, Generic)

-- | An SSC setting to define how the node should behave when sending openings.
data SscOpeningParams
    = SscOpeningNormal    -- ^ Normal mode of operation
    | SscOpeningNone      -- ^ An opening is not sent to other nodes
    | SscOpeningWrong     -- ^ An opening is generated at random and doesn't
                          --    correspond to the commitment. This setting is
                          --    typically only specified for testing purposes
                          --    as the node will only send rubbish openings.
    deriving (Eq, Show)

-- | An SSC setting to define how the node should behave when sending shares.
data SscSharesParams
    = SscSharesNormal     -- ^ Normal mode of operation
    | SscSharesNone       -- ^ Decrypted shares are not sent to other nodes
    | SscSharesWrong      -- ^ We randomly change contents of decrypted shares
    deriving (Eq, Show)

----------------------------------------------------------------------------
-- JSON/YAML parsing
----------------------------------------------------------------------------

instance A.FromJSON SscBehavior where
    parseJSON = A.genericParseJSON defaultOptions

instance A.FromJSON SscOpeningParams where
    parseJSON = A.withText "SscOpeningParams" $ toAesonError . \case
        "Normal" -> Right SscOpeningNormal
        "None"   -> Right SscOpeningNone
        "Wrong"  -> Right SscOpeningWrong
        other    -> Left ("invalid value " <> show other <>
                          ", acceptable values are Normal|None|Wrong")

instance A.FromJSON SscSharesParams where
    parseJSON = A.withText "SscSharesParams" $ toAesonError . \case
        "Normal" -> Right SscSharesNormal
        "None"   -> Right SscSharesNone
        "Wrong"  -> Right SscSharesWrong
        other    -> Left ("invalid value " <> show other <>
                          ", acceptable values are Normal|None|Wrong")

----------------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------------

instance Default SscBehavior where
    def = SscBehavior def def

instance Default SscOpeningParams where
    def = SscOpeningNormal

instance Default SscSharesParams where
    def = SscSharesNormal
