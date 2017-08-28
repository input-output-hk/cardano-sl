module Pos.Ssc.GodTossing.Behavior
       ( GtBehavior (..)
       , GtOpeningParams (..)
       , GtSharesParams (..)
       ) where

import           Universum

import qualified Data.Aeson             as A
import           Data.Default           (Default (..))
import           Serokell.Aeson.Options (defaultOptions)

----------------------------------------------------------------------------
-- Types for the behavior config
----------------------------------------------------------------------------

-- | Godtossing algorithm settings (a part of the behavior config).
--
-- The syntax of the config is as follows:
--
-- @
--     sendOpening: Normal | None | Wrong
--     sendShares: Normal | None | Wrong
-- @
data GtBehavior = GtBehavior
    { -- | Opening settings (e.g. whether to send the opening at all)
      gbSendOpening :: !GtOpeningParams
      -- | Shares settings (e.g. whether to send rubbish instead of shares)
    , gbSendShares  :: !GtSharesParams
    }
    deriving (Eq, Show, Generic)

data GtOpeningParams
    = GtOpeningNormal    -- ^ Normal mode of operation
    | GtOpeningNone      -- ^ An opening is not sent to other nodes
    | GtOpeningWrong     -- ^ An opening is generated at random and doesn't
                         --    correspond to the commitment
    deriving (Eq, Show)

data GtSharesParams
    = GtSharesNormal     -- ^ Normal mode of operation
    | GtSharesNone       -- ^ Decrypted shares are not sent to other nodes
    | GtSharesWrong      -- ^ We randomly change contents of decrypted shares
    deriving (Eq, Show)

----------------------------------------------------------------------------
-- JSON/YAML parsing
----------------------------------------------------------------------------

instance A.FromJSON GtBehavior where
    parseJSON = A.genericParseJSON defaultOptions

instance A.FromJSON GtOpeningParams where
    parseJSON = A.withText "GtOpeningParams" $ \case
        "Normal" -> pure GtOpeningNormal
        "None"   -> pure GtOpeningNone
        "Wrong"  -> pure GtOpeningWrong
        other    -> fail ("invalid value " <> show other <>
                          ", acceptable values are Normal|None|Wrong")

instance A.FromJSON GtSharesParams where
    parseJSON = A.withText "GtSharesParams" $ \case
        "Normal" -> pure GtSharesNormal
        "None"   -> pure GtSharesNone
        "Wrong"  -> pure GtSharesWrong
        other    -> fail ("invalid value " <> show other <>
                          ", acceptable values are Normal|None|Wrong")

----------------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------------

instance Default GtBehavior where
    def = GtBehavior def def

instance Default GtOpeningParams where
    def = GtOpeningNormal

instance Default GtSharesParams where
    def = GtSharesNormal
