{-| Compile-time configuration is represented by 'CompileConfig' data type.
    This configuration stores constants from paper and also some
    network-specific constants.
-}

module Pos.CompileConfig.Type
       ( CompileConfig (..)
       ) where

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
    } deriving (Show)
