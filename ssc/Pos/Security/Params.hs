module Pos.Security.Params
       ( SecurityParams(..)
       , AttackType(..)
       , AttackTarget(..)
       , NodeAttackedError(..)
       ) where

import           Universum

import           Control.Lens (_Left)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Default (Default (..))
import           Serokell.Aeson.Options (defaultOptions)
import qualified Text.Parsec as Parsec

import           Pos.Aeson.Crypto ()
import           Pos.Core.Common (StakeholderId)
import           Pos.Util.TimeWarp (NetworkAddress, addrParser)
import           Pos.Util.Util (toAesonError, aesonError)

-- | Network attack settings (a part of the behavior config).
--
-- The syntax of this config section is as follows:
--
-- @
-- networkAttacks:
--     attackTypes: [NoBlocks, NoCommitments]
--     attackTargets:
--         - Network: 152.63.42.61:59237
--         - Network: 8.32.11.13:6394
--         - Network: 68.70.135.33:9477
--         - PubKey:
--         - PubKey: ???
-- @

data SecurityParams = SecurityParams
    { -- | List of attack types used by malicious emulation
      spAttackTypes   :: ![AttackType]
      -- | List of targets to attack by malicious emulation
    , spAttackTargets :: ![AttackTarget]
    }
    deriving (Eq, Show, Generic)

instance Default SecurityParams where
    def = SecurityParams [] []

instance A.FromJSON SecurityParams where
    parseJSON = A.genericParseJSON defaultOptions

data AttackType
    = AttackNoBlocks
    | AttackNoCommitments
    deriving (Eq, Show)

instance A.FromJSON AttackType where
    parseJSON = A.withText "AttackType" $ toAesonError . \case
        "NoBlocks"      -> Right AttackNoBlocks
        "NoCommitments" -> Right AttackNoCommitments
        other           -> Left $
            "invalid value " <> show other <>
            ", acceptable values are NoBlocks|NoCommitments"

data AttackTarget
    = NetworkAddressTarget { attNetworkAddr :: NetworkAddress}
    | PubKeyAddressTarget { attPkAddr :: StakeholderId}
    deriving (Eq, Show)

instance A.FromJSON AttackTarget where
    parseJSON = A.withObject "AttackTarget" $ \o ->
        asum [ NetworkAddressTarget <$>
                 (parseNetworkAddress =<< o A..: "Network")
             , PubKeyAddressTarget  <$> o A..: "PubKey"
             , aesonError "expected a key 'Network' or 'PubKey'"
             ]

-- TODO Move to Pos.Security.Types

data NodeAttackedError = AttackNoBlocksTriggered
    deriving Show

instance Exception NodeAttackedError

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

parseNetworkAddress :: A.Value -> A.Parser NetworkAddress
parseNetworkAddress = A.withText "NetworkAddress" $ \s ->
    toAesonError . over _Left show $ Parsec.parse addrParser "" s
