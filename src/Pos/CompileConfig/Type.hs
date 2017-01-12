{-# LANGUAGE DeriveLift #-}

{-| Compile-time configuration is represented by 'CompileConfig' data type.
    This configuration is parsed at compile-time using /file-embed/ library
    and stores constants from paper and also some network-specific.
-}

module Pos.CompileConfig.Type
       ( CompileConfig (..)
       ) where

import           Data.String                (String)
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
    , ccVssMaxTTL                     :: !Word64
      -- ^ VSS certificates max timeout to live (number of epochs)
    , ccProtocolMagic                 :: !Int
      -- ^ Magic constant for separating real/testnet
    , ccEnchancedMessageBroadcast     :: !Word
      -- ^ True if we should enable enchanced bessage broadcast
    , ccDelegationThreshold           :: !Double
      -- ^ Threshold for heavyweight delegation.
    , ccUpdateServers                 :: ![String]
      -- ^ Servers for downloading application updates
    , ccMaxBlockProxySKs              :: !Int
      -- ^ Maximum number of PSKs allowed in block
    , ccNtpResponseTimeout            :: !Int
      -- ^ How often request to NTP server and response collection
    , ccNtpPollDelay                  :: !Int
      -- ^ How often send request to NTP server
    , ccUpdateProposalThreshold       :: !Double
      -- ^ Portion of total stake such that block containing
      -- UpdateProposal must contain positive votes for this proposal
      -- from stakeholders owning at least this amount of stake.
    , ccUpdateVoteThreshold           :: !Double
      -- ^ Portion of total stake necessary to vote for or against update.
    , ccUpdateImplicitApproval        :: !Word
      -- ^ Number of slots after which update is implicitly approved
      -- unless it has more negative votes than positive.
    } deriving (Show, Lift)
