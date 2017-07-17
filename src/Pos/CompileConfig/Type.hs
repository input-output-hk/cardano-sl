{-| Compile-time configuration is represented by 'CompileConfig' data type.
    This configuration stores constants from paper and also some
    network-specific constants.
-}

module Pos.CompileConfig.Type
       ( CompileConfig (..)
       ) where

import           Universum

import           Pos.Util  ()

-- | Compile time configuration. See example in /constants.yaml/ file.
data CompileConfig = CompileConfig
    {
      ccNetworkDiameter               :: !Int
      -- ^ Estimated time for broadcasting messages
    , ccDefaultPeers                  :: ![String]
      -- ^ List of default peers
    , ccMdNoBlocksSlotThreshold       :: !Int
      -- ^ Threshold of slots for malicious activity detection
    , ccMdNoCommitmentsEpochThreshold :: !Int
      -- ^ Threshold of epochs for malicious activity detection
    , ccLightDlgConfirmationTimeout   :: !Int
      -- ^ Timeout for holding light psks confirmations
    , ccDlgCacheParam                 :: !Int
      -- ^ This value parameterizes size of cache used in Delegation.
      -- Not bytes, but number of elements.
    , ccEnhancedMessageTimeout        :: !Word
      -- ^ We consider node as known if it was pinged at most 10 sec ago.
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
