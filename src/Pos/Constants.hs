{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Constants used by algorithm. See paper for more details.
    Some constants are parsed at compile-time (see 'Pos.CompileConfig.Type').
    Others are derived from those.
-}

module Pos.Constants
       (
       -- * Constants mentioned in paper
         blkSecurityParam
       , slotSecurityParam
       , epochSlots
       , networkDiameter

       -- * SSC constants
       , sharedSeedLength
       , mpcSendInterval
       , mpcThreshold

       -- * Dev/production mode, system start
       , isDevelopment
       , staticSysStart

       -- * Genesis constants
       , genesisN
       , genesisSlotDuration
       , genesisMaxBlockSize

       -- * Other constants
       , maxLocalTxs
       , maxBlockProxySKs
       , neighborsSendThreshold
       , networkConnectionTimeout
       , blockRetrievalQueueSize
       , defaultPeers
       , sysTimeBroadcastSlots
       , vssMaxTTL
       , vssMinTTL
       , protocolMagic
       , enhancedMessageBroadcast
       , delegationThreshold
       , recoveryHeadersMessage
       , kademliaDumpInterval

       -- * Malicious activity detection constants
       , mdNoBlocksSlotThreshold
       , mdNoCommitmentsEpochThreshold

       -- * Update system constants
       , lastKnownBlockVersion
       , curSoftwareVersion
       , ourAppName
       , appSystemTag
       , updateServers
       , updateProposalThreshold
       , updateVoteThreshold
       , updateImplicitApproval
       , usSoftforkThreshold

       -- * NTP
       , ntpMaxError
       , ntpResponseTimeout
       , ntpPollDelay
       ) where

import           Data.Time.Units            (Microsecond)
import           Language.Haskell.TH.Syntax (lift, runIO)
import           Pos.Util.TimeWarp          (ms, sec)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (staticAssert)
import           System.Environment         (lookupEnv)
import qualified Text.Parsec                as P
import           Universum                  hiding (lift)
#ifndef DEV_MODE
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           System.IO.Unsafe           (unsafePerformIO)
#endif

import           Pos.CLI                    (dhtNodeParser)
import           Pos.CompileConfig          (CompileConfig (..), compileConfig)
import           Pos.DHT.Model.Types        (DHTNode)
import           Pos.Types.Timestamp        (Timestamp (..))
import           Pos.Types.Types            (CoinPortion, unsafeCoinPortion)
import           Pos.Types.Version          (ApplicationName, BlockVersion (..),
                                             SoftwareVersion (..), mkApplicationName)
import           Pos.Update.Core            (SystemTag, mkSystemTag)
import           Pos.Util                   ()
import           Pos.Util.TimeWarp          (mcs)

----------------------------------------------------------------------------
-- Main constants mentioned in paper
----------------------------------------------------------------------------

-- | Security parameter which is maximum number of blocks which can be
-- rolled back.
blkSecurityParam :: Integral a => a
blkSecurityParam = fromIntegral . ccK $ compileConfig

-- | Security parameter expressed in number of slots. It uses chain
-- quality property. It's basically 'blkSecurityParam / chain_quality'.
slotSecurityParam :: Integral a => a
slotSecurityParam = 2 * blkSecurityParam

-- | Number of slots inside one epoch.
epochSlots :: Integral a => a
epochSlots = 12 * blkSecurityParam

-- | Estimated time needed to broadcast message from one node to all
-- other nodes. Also see 'Pos.CompileConfig.ccNetworkDiameter'.
networkDiameter :: Microsecond
networkDiameter = sec . ccNetworkDiameter $ compileConfig

----------------------------------------------------------------------------
-- SSC
----------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32

-- | Length of interval during which node should send her MPC
-- message. Relevant only for one SSC implementation.
-- Also see 'Pos.CompileConfig.ccMpcSendInterval'.
mpcSendInterval :: Microsecond
mpcSendInterval = sec . fromIntegral . ccMpcSendInterval $ compileConfig

-- | Threshold value for mpc participation.
mpcThreshold :: CoinPortion
mpcThreshold = unsafeCoinPortion $ ccMpcThreshold compileConfig

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

-- | See 'Pos.CompileConfig.ccGenesisN'.
genesisN :: Integral i => i
genesisN = fromIntegral . ccGenesisN $ compileConfig

-- | Length of slot.
genesisSlotDuration :: Microsecond
genesisSlotDuration = sec . ccGenesisSlotDurationSec $ compileConfig

-- | Maximum size of a block (in bytes)
genesisMaxBlockSize :: Byte
genesisMaxBlockSize = ccGenesisMaxBlockSize $ compileConfig

----------------------------------------------------------------------------
-- Other constants
----------------------------------------------------------------------------

-- | Maximum amount of transactions we have in storage
-- (i.e. we can accept without putting them in block).
-- There're next kind of storages in our implementation:
--
-- * temporary storage of transactions
--
-- * utxo map that corresponds to it
--
-- * utxo of blocks in history
--
-- This constant is size of first set.
-- Also see 'Pos.CompileConfig.ccMaxLocalTxs'.
maxLocalTxs :: Integral i => i
maxLocalTxs = fromIntegral . ccMaxLocalTxs $ compileConfig

-- | Maximum number of PSKs allowed in block
maxBlockProxySKs :: Integral i => i
maxBlockProxySKs = fromIntegral . ccMaxBlockProxySKs $ compileConfig

-- | /Time-lord/ node announces system start time by broadcast. She does it
-- during first 'Pos.CompileConfig.ccSysTimeBroadcastSlots' slots.
sysTimeBroadcastSlots :: Integral i => i
sysTimeBroadcastSlots = fromIntegral . ccSysTimeBroadcastSlots $ compileConfig

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ compileConfig

networkConnectionTimeout :: Microsecond
networkConnectionTimeout = ms . fromIntegral . ccNetworkConnectionTimeout $ compileConfig

blockRetrievalQueueSize :: Integral a => a
blockRetrievalQueueSize =
    fromIntegral . ccBlockRetrievalQueueSize $ compileConfig

-- | @True@ if current mode is 'Development'.
isDevelopment :: Bool
isDevelopment = isNothing staticSysStart

-- | System start time embeded into binary.
staticSysStart :: Maybe Timestamp
#ifdef DEV_MODE
staticSysStart = Nothing
#else
staticSysStart = Just $ Timestamp $ sec $
    let st = ccProductionNetworkStartTime compileConfig
    in if st > 0 then st
       else let pause = 30
                divider = 10
                after3Mins = pause + unsafePerformIO (round <$> getPOSIXTime)
                minuteMod = after3Mins `mod` divider
                alignment = if minuteMod > (divider `div` 2) then 1 else 0
            in (after3Mins `div` divider + alignment) * divider
               -- ^ If several local nodes are started within 20 sec,
               -- they'll have same start time
#endif

-- | See 'Pos.CompileConfig.ccDefaultPeers'.
defaultPeers :: [DHTNode]
defaultPeers = map parsePeer . ccDefaultPeers $ compileConfig
  where
    parsePeer :: String -> DHTNode
    parsePeer =
        either (panic . show) identity .
        P.parse dhtNodeParser "Compile time config"

-- | Max VSS certificate TTL (Ssc.GodTossing part)
vssMaxTTL :: Integral i => i
vssMaxTTL = fromIntegral . ccVssMaxTTL $ compileConfig

-- | Min VSS certificate TTL (Ssc.GodTossing part)
vssMinTTL :: Integral i => i
vssMinTTL = fromIntegral . ccVssMinTTL $ compileConfig

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . ccProtocolMagic $ compileConfig

-- | Setting this to true enables enhanced message broadcast
enhancedMessageBroadcast :: Integral a => a
enhancedMessageBroadcast = fromIntegral $ ccEnhancedMessageBroadcast compileConfig

-- | Portion of total stake necessary to vote for or against update.
delegationThreshold :: CoinPortion
delegationThreshold = unsafeCoinPortion $ ccDelegationThreshold compileConfig

-- | Maximum amount of headers node can put into headers message while
-- in "after offline" or "recovery" mode. Should be more than
-- 'blkSecurityParam'.
recoveryHeadersMessage :: (Integral a) => a
recoveryHeadersMessage = fromIntegral . ccRecoveryHeadersMessage $ compileConfig

-- | Interval for dumping state of Kademlia in slots
kademliaDumpInterval :: (Integral a) => a
kademliaDumpInterval = fromIntegral . ccKademliaDumpInterval $ compileConfig

----------------------------------------------------------------------------
-- Malicious activity
----------------------------------------------------------------------------

-- | Number of slots used by malicious actions detection to check if
-- we are not receiving generated blocks.
mdNoBlocksSlotThreshold :: Integral i => i
mdNoBlocksSlotThreshold = fromIntegral . ccMdNoBlocksSlotThreshold $ compileConfig

-- | Number of epochs used by malicious actions detection to check if
-- our commitments are not included in blockchain.
mdNoCommitmentsEpochThreshold :: Integral i => i
mdNoCommitmentsEpochThreshold = fromIntegral . ccMdNoCommitmentsEpochThreshold $ compileConfig

----------------------------------------------------------------------------
-- Update system
----------------------------------------------------------------------------

cardanoSlAppName :: ApplicationName
cardanoSlAppName = either (panic . (<>) "Failed to init cardanoSlAppName: ")
                      identity $ mkApplicationName "cardano"

appSystemTag :: SystemTag
appSystemTag = $(do
    mbTag <- runIO (lookupEnv "CSL_SYSTEM_TAG")
    case mbTag of
        Nothing ->
#ifdef DEV_MODE
            [|panic "'appSystemTag' can't be used if \
                    \env var \"CSL_SYSTEM_TAG\" wasn't set \
                    \during compilation" |]
#else
            fail "Failed to init appSystemTag: \
                 \couldn't find env var \"CSL_SYSTEM_TAG\""
#endif
        Just tag -> lift =<< mkSystemTag (toText tag))

-- | Last block version application is aware of.
lastKnownBlockVersion :: BlockVersion
lastKnownBlockVersion = BlockVersion 0 0 0

-- | Version of application (code running)
curSoftwareVersion :: SoftwareVersion
curSoftwareVersion = SoftwareVersion cardanoSlAppName 0

-- | Name of our application.
ourAppName :: ApplicationName
ourAppName = cardanoSlAppName

-- | Update servers
updateServers :: [String]
updateServers = ccUpdateServers compileConfig

----------------------------------------------------------------------------
-- NTP
----------------------------------------------------------------------------
-- | Inaccuracy in call threadDelay (actually it is error much less than 1 sec)
ntpMaxError :: Microsecond
ntpMaxError = 1000000 -- 1 sec

-- | How often request to NTP server and response collection
ntpResponseTimeout :: Microsecond
ntpResponseTimeout = mcs . ccNtpResponseTimeout $ compileConfig

-- | How often send request to NTP server
ntpPollDelay :: Microsecond
ntpPollDelay = mcs . ccNtpPollDelay $ compileConfig

-- | Portion of total stake such that block containing
-- UpdateProposal must contain positive votes for this proposal
-- from stakeholders owning at least this amount of stake.
updateProposalThreshold :: CoinPortion
updateProposalThreshold = unsafeCoinPortion $ ccUpdateProposalThreshold compileConfig

staticAssert
    (ccUpdateProposalThreshold compileConfig >= 0)
    "updateProposalThreshold is negative"

staticAssert
    (ccUpdateProposalThreshold compileConfig <= 1)
    "updateProposalThreshold is more than 1"

-- | Portion of total stake necessary to vote for or against update.
updateVoteThreshold :: CoinPortion
updateVoteThreshold = unsafeCoinPortion $ ccUpdateVoteThreshold compileConfig

staticAssert
    (ccUpdateVoteThreshold compileConfig >= 0)
    "updateVoteThreshold is negative"

staticAssert
    (ccUpdateVoteThreshold compileConfig <= 1)
    "updateVoteThreshold is more than 1"

-- | Number of slots after which update is implicitly approved
-- unless it has more negative votes than positive.
updateImplicitApproval :: Integral i => i
updateImplicitApproval = fromIntegral $ ccUpdateImplicitApproval compileConfig

-- | Portion of total stake such that if total stake of issuers of blocks
-- with some block version is bigger than this portion, this block
-- version is adopted.
usSoftforkThreshold :: CoinPortion
usSoftforkThreshold = unsafeCoinPortion $ ccUsSoftforkThreshold compileConfig

staticAssert
    (ccUsSoftforkThreshold compileConfig >= 0)
    "usSoftforkThreshold is negative"

staticAssert
    (ccUsSoftforkThreshold compileConfig <= 1)
    "usSoftforkThreshold is more than 1"
