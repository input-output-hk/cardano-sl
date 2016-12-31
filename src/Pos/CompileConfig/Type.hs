{-# LANGUAGE DeriveLift #-}

{-| Compile-time configuration is represented by 'CompileConfig' data type.
    This configuration is parsed at compile-time using /file-embed/ library
    and stores constants from paper and also some network-specific.
-}

module Pos.CompileConfig.Type
    ( CompileConfig (..)
    ) where

import           Language.Haskell.TH.Syntax (Lift)
import           Universum

-- | Compile time configuration. See example in /constants.yaml/ file.
data CompileConfig = CompileConfig
    { ccK                             :: !Int
      -- ^ Security parameter from paper
    , ccSlotDurationSec               :: !Int
      -- ^ Length of slot in seconds
    , ccNetworkDiameter               :: !Int
      -- ^ Estimated time for broadcasting messages
    , ccNeighboursSendThreshold       :: !Int
      -- ^ Broadcasting threshold
    , ccGenesisN                      :: !Int
      -- ^ Number of pre-generated keys
    , ccMaxLocalTxs                   :: !Word
      -- ^ Max number of transactions in Storage
    , ccDefaultPeers                  :: ![[Char]]
      -- ^ List of default peers
    , ccSysTimeBroadcastSlots         :: !Int
      -- ^ Number of slots to broadcast system time
    , ccMpcSendInterval               :: !Word
      -- ^ Length of interval for sending MPC message
    , ccMdNoBlocksSlotThreshold       :: !Int
      -- ^ Threshold of slots for malicious activity detection
    , ccMdNoCommitmentsEpochThreshold :: !Int
      -- ^ Threshold of epochs for malicious activity detection
    , ccProtocolMagic                 :: !Int
      -- ^ Magic constant for separating real/testnet
    , ccEnchancedMessageBroadcast     :: !Word
      -- ^ True if we should enable enchanced bessage broadcast
    } deriving (Show, Lift)
