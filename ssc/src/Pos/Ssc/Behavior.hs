module Pos.Ssc.Behavior
       ( SscBehavior (..)
       , SscOpeningParams (..)
       , SscSharesParams (..)
       ) where

import           Universum

import qualified Data.Aeson as A
import           Data.Default (Default (..))
import           Serokell.Aeson.Options (defaultOptions)

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
    { -- | Opening settings (e.g. whether to send the opening at all)
      sbSendOpening :: !SscOpeningParams
      -- | Shares settings (e.g. whether to send rubbish instead of shares)
    , sbSendShares  :: !SscSharesParams
    }
    deriving (Eq, Show, Generic)

data SscOpeningParams
    = SscOpeningNormal    -- ^ Normal mode of operation
    | SscOpeningNone      -- ^ An opening is not sent to other nodes
    | SscOpeningWrong     -- ^ An opening is generated at random and doesn't
                          --    correspond to the commitment
    deriving (Eq, Show)

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
