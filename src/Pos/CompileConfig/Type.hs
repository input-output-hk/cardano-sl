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
-- Genesis
----------------------------------------------------------------------------
      ccGenesisN                      :: !Int
      -- ^ Number of pre-generated keys
    , ccGenesisSlotDurationSec        :: !Int
      -- ^ Length of slot in seconds
    , ccGenesisMaxBlockSize           :: !Byte
      -- ^ Maximum block size in bytes
    , ccGenesisMaxHeaderSize          :: !Byte
      -- ^ Maximum block header size in bytes

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
    , ccGenesisMaxUpdateProposalSize  :: !Byte
      -- ^ Maximum update proposal size in bytes
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
    , ccLightDlgConfirmationTimeout   :: !Int
      -- ^ Timeout for holding light psks confirmations
    , ccEnhancedMessageBroadcast      :: !Word
      -- ^ True if we should enable enhanced bessage broadcast
    , ccRecoveryHeadersMessage        :: !Int
      -- ^ Numbers of headers put in message in recovery mode.
    , ccMessageCacheTimeout           :: !Int
      -- ^ Interval we ignore cached messages in components that
      -- support caching
    , ccNetworkConnectionTimeout      :: !Int
      -- ^ Network connection timeout in milliseconds
    , ccBlockRetrievalQueueSize       :: !Int
      -- ^ Block retrieval queue capacity
    , ccPropagationQueueSize          :: !Int
      -- ^ InvMsg propagation queue capacity
    } deriving (Show, Lift)
