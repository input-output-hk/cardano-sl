{-# LANGUAGE DeriveLift #-}

{-| Compile-time configuration is represented by 'CompileConfig' data type.
    This configuration is parsed at compile-time using /file-embed/ library
    and stores constants from paper and also some network-specific.
-}

module Pos.CompileConfig.Type
       ( CompileConfig (..)
       ) where

import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Util                   ()

-- | Compile time configuration. See example in /constants.yaml/ file.
data CompileConfig = CompileConfig
    {
----------------------------------------------------------------------------
-- Fundamental constants
----------------------------------------------------------------------------
      ccK                             :: !Int
      -- ^ Security parameter from paper

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------
    , ccGenesisN                      :: !Int
      -- ^ Number of pre-generated keys
    , ccGenesisSlotDurationSec        :: !Int
      -- ^ Length of slot in seconds
    , ccGenesisMaxBlockSize           :: !Byte
      -- ^ Maximum block size in bytes

----------------------------------------------------------------------------
-- -- Txp
----------------------------------------------------------------------------
    , ccGenesisMaxTxSize              :: !Byte
      -- ^ Maximum tx size in bytes

----------------------------------------------------------------------------
-- -- SSC
----------------------------------------------------------------------------
    , ccGenesisMpcThd                 :: !Double
      -- ^ Eligibility threshold for MPC

----------------------------------------------------------------------------
-- -- Delegation
----------------------------------------------------------------------------
    , ccGenesisHeavyDelThd            :: !Double
      -- ^ Threshold for heavyweight delegation

----------------------------------------------------------------------------
-- -- UpdateSystem
----------------------------------------------------------------------------
    , ccGenesisUpdateVoteThd          :: !Double
      -- ^ Portion of total stake necessary to vote for or against update.
    , ccGenesisUpdateProposalThd      :: !Double
      -- ^ Portion of total stake such that block containing
      -- UpdateProposal must contain positive votes for this proposal
      -- from stakeholders owning at least this amount of stake.
    , ccGenesisUpdateImplicit         :: !Word
      -- ^ Number of slots after which update is implicitly approved
      -- unless it has more negative votes than positive.
    , ccGenesisUpdateSoftforkThd      :: !Double
      -- ^ Portion of total stake such that if total stake of issuers of blocks
      -- with some block version is bigger than this portion, this block
      -- version is adopted.

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------
    , ccNetworkDiameter               :: !Int
      -- ^ Estimated time for broadcasting messages
    , ccNeighboursSendThreshold       :: !Int
      -- ^ Broadcasting threshold
    , ccMaxLocalTxs                   :: !Word
      -- ^ Max number of transactions in Storage
    , ccDefaultPeers                  :: ![String]
      -- ^ List of default peers
    , ccSysTimeBroadcastSlots         :: !Int
      -- ^ Number of slots to broadcast system time
    , ccMpcSendInterval               :: !Word
      -- ^ Length of interval for sending MPC message
    , ccMdNoBlocksSlotThreshold       :: !Int
      -- ^ Threshold of slots for malicious activity detection
    , ccMdNoCommitmentsEpochThreshold :: !Int
      -- ^ Threshold of epochs for malicious activity detection
    , ccVssMaxTTL                     :: !Word64
      -- ^ VSS certificates max timeout to live (number of epochs)
    , ccVssMinTTL                     :: !Word64
      -- ^ VSS certificates min timeout to live (number of epochs)
    , ccProtocolMagic                 :: !Int
      -- ^ Magic constant for separating real/testnet
    , ccLightDlgConfirmationTimeout   :: !Int
      -- ^ Timeout for holding light psks confirmations
    , ccEnhancedMessageBroadcast      :: !Word
      -- ^ True if we should enable enhanced bessage broadcast
    , ccRecoveryHeadersMessage        :: !Int
      -- ^ Numbers of headers put in message in recovery mode.
    , ccMessageCacheTimeout           :: !Int
      -- ^ Interval we ignore cached messages in components that
      -- support caching
    , ccKademliaDumpInterval          :: !Int
      -- ^ Interval for dumping Kademlia state in slots
    , ccUpdateServers                 :: ![String]
      -- ^ Servers for downloading application updates
    , ccNtpResponseTimeout            :: !Int
      -- ^ How often request to NTP server and response collection
    , ccNtpPollDelay                  :: !Int
      -- ^ How often send request to NTP server
    , ccNetworkConnectionTimeout      :: !Int
      -- ^ Network connection timeout in milliseconds
    , ccBlockRetrievalQueueSize       :: !Int
      -- ^ Block retrieval queue capacity
    , ccNetworkReceiveTimeout         :: !Int
      -- ^ Network timeout on `recv` in milliseconds
    , ccProductionNetworkStartTime    :: !Int
      -- ^ Start time of network (in `Prodution` running mode).
      -- If set to zero, then running time is 2 minutes after build.
    } deriving (Show, Lift)
