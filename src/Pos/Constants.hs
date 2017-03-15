{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Constants used by algorithm. See paper for more details.
    Some constants are parsed at compile-time (see 'Pos.CompileConfig.Type').
    Others are derived from those.
-}

module Pos.Constants
       (
         module Pos.Core.Constants
       , module Pos.DHT.Constants
       , module Pos.Communication.Constants
       , module Pos.Slotting.Constants

       -- * Constants mentioned in paper
       , networkDiameter

       -- * SSC constants
       , sharedSeedLength
       , mpcSendInterval

       -- * Genesis constants
       , genesisN
       , genesisSlotDuration
       , genesisMaxBlockSize
       , genesisMaxHeaderSize
       , genesisMaxTxSize
       , genesisMpcThd
       , genesisHeavyDelThd
       , genesisUpdateVoteThd
       , genesisMaxUpdateProposalSize
       , genesisUpdateProposalThd
       , genesisUpdateImplicit
       , genesisUpdateSoftforkThd

       -- * Other constants
       , maxLocalTxs
       , networkConnectionTimeout
       , blockRetrievalQueueSize
       , propagationQueueSize
       , defaultPeers
       , sysTimeBroadcastSlots
       , vssMaxTTL
       , vssMinTTL
       , recoveryHeadersMessage
       , messageCacheTimeout

       -- * Delegation
       , lightDlgConfirmationTimeout

       -- * Malicious activity detection constants
       , mdNoBlocksSlotThreshold
       , mdNoCommitmentsEpochThreshold

       -- * Update system constants
       , lastKnownBlockVersion
       , curSoftwareVersion
       , ourAppName
       , appSystemTag
       ) where

import           Data.Time.Units             (Microsecond, Millisecond, convertUnit)
import           Language.Haskell.TH.Syntax  (lift, runIO)
import           Serokell.Data.Memory.Units  (Byte)
import           Serokell.Util               (ms, sec, staticAssert)
import           System.Environment          (lookupEnv)
import qualified Text.Parsec                 as P
import           Universum                   hiding (lift)

import           Pos.CompileConfig           (CompileConfig (..), compileConfig)
import           Pos.Core.Types              (ApplicationName, BlockVersion (..),
                                              CoinPortion, SoftwareVersion (..),
                                              unsafeCoinPortionFromDouble)
import           Pos.Core.Version            (mkApplicationName)
import           Pos.DHT.Model.Types         (DHTNode, dhtNodeParser)
import           Pos.Update.Core             (SystemTag, mkSystemTag)
import           Pos.Util                    ()

-- Reexports
import           Pos.Communication.Constants
import           Pos.Core.Constants
import           Pos.DHT.Constants
import           Pos.Slotting.Constants

----------------------------------------------------------------------------
-- Main constants mentioned in paper
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

cc :: CompileConfig
cc = compileConfig

-- | See 'Pos.CompileConfig.ccGenesisN'.
genesisN :: Integral i => i
genesisN = fromIntegral . ccGenesisN $ compileConfig

-- | Length of slot.
genesisSlotDuration :: Millisecond
genesisSlotDuration =
    convertUnit . sec . ccGenesisSlotDurationSec $ compileConfig

-- | Maximum size of a block (in bytes)
genesisMaxBlockSize :: Byte
genesisMaxBlockSize = ccGenesisMaxBlockSize $ compileConfig

-- | Maximum size of a block header (in bytes)
genesisMaxHeaderSize :: Byte
genesisMaxHeaderSize = ccGenesisMaxHeaderSize $ compileConfig

-- | See 'Pos.CompileConfig.ccGenesisMaxTxSize'.
genesisMaxTxSize :: Byte
genesisMaxTxSize = ccGenesisMaxTxSize cc

-- | See 'Pos.CompileConfig.ccGenesisMpcThd'.
genesisMpcThd :: CoinPortion
genesisMpcThd = unsafeCoinPortionFromDouble $ ccGenesisMpcThd compileConfig

staticAssert
    (ccGenesisMpcThd compileConfig >= 0 && ccGenesisMpcThd compileConfig < 1)
    "genesisMpcThd is not in range [0, 1)"

-- | See 'Pos.CompileConfig.ccGenesisHeavyDelThd'.
genesisHeavyDelThd :: CoinPortion
genesisHeavyDelThd = unsafeCoinPortionFromDouble $ ccGenesisHeavyDelThd cc

staticAssert
    (ccGenesisHeavyDelThd compileConfig >= 0 && ccGenesisMpcThd compileConfig < 1)
    "genesisHeavyDelThd is not in range [0, 1)"

-- | See 'Pos.CompileConfig.ccGenesisUpdateVoteThd'.
genesisUpdateVoteThd :: CoinPortion
genesisUpdateVoteThd = unsafeCoinPortionFromDouble $ ccGenesisUpdateVoteThd cc

staticAssert
    (ccGenesisUpdateVoteThd compileConfig >= 0 && ccGenesisUpdateVoteThd compileConfig < 1)
    "genesisUpdateVoteThd is not in range [0, 1)"

-- | See 'Pos.CompileConfig.ccGenesisMaxUpdateProposalSize'.
genesisMaxUpdateProposalSize :: Byte
genesisMaxUpdateProposalSize = ccGenesisMaxUpdateProposalSize cc

-- | See 'Pos.CompileConfig.ccGenesisUpdateProposalThd'.
genesisUpdateProposalThd :: CoinPortion
genesisUpdateProposalThd = unsafeCoinPortionFromDouble $ ccGenesisUpdateProposalThd cc

staticAssert
    (ccGenesisUpdateProposalThd compileConfig > 0 && ccGenesisUpdateProposalThd compileConfig < 1)
    "genesisUpdateProposalThd is not in range (0, 1)"

-- | See 'Pos.CompileConfig.ccGenesisUpdateImplicit'.
genesisUpdateImplicit :: Integral i => i
genesisUpdateImplicit = fromIntegral . ccGenesisUpdateImplicit $ cc

-- | See 'Pos.CompileConfig.ccGenesisUpdateSoftforkThd'.
genesisUpdateSoftforkThd :: CoinPortion
genesisUpdateSoftforkThd = unsafeCoinPortionFromDouble $ ccGenesisUpdateSoftforkThd cc

staticAssert
    (ccGenesisUpdateSoftforkThd compileConfig > 0 && ccGenesisUpdateSoftforkThd compileConfig < 1)
    "genesisUpdateSoftforkThd is not in range (0, 1)"

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

-- | /Time-lord/ node announces system start time by broadcast. She does it
-- during first 'Pos.CompileConfig.ccSysTimeBroadcastSlots' slots.
sysTimeBroadcastSlots :: Integral i => i
sysTimeBroadcastSlots = fromIntegral . ccSysTimeBroadcastSlots $ compileConfig

networkConnectionTimeout :: Microsecond
networkConnectionTimeout = ms . fromIntegral . ccNetworkConnectionTimeout $ compileConfig

blockRetrievalQueueSize :: Integral a => a
blockRetrievalQueueSize =
    fromIntegral . ccBlockRetrievalQueueSize $ compileConfig

propagationQueueSize :: Integral a => a
propagationQueueSize =
    fromIntegral $ ccPropagationQueueSize $ compileConfig

-- | See 'Pos.CompileConfig.ccDefaultPeers'.
defaultPeers :: [DHTNode]
defaultPeers = map parsePeer . ccDefaultPeers $ compileConfig
  where
    parsePeer :: String -> DHTNode
    parsePeer =
        either (error . show) identity .
        P.parse dhtNodeParser "Compile time config"

-- | Max VSS certificate TTL (Ssc.GodTossing part)
vssMaxTTL :: Integral i => i
vssMaxTTL = fromIntegral . ccVssMaxTTL $ compileConfig

-- | Min VSS certificate TTL (Ssc.GodTossing part)
vssMinTTL :: Integral i => i
vssMinTTL = fromIntegral . ccVssMinTTL $ compileConfig

-- | Maximum amount of headers node can put into headers message while
-- in "after offline" or "recovery" mode. Should be more than
-- 'blkSecurityParam'.
recoveryHeadersMessage :: (Integral a) => a
recoveryHeadersMessage = fromIntegral . ccRecoveryHeadersMessage $ compileConfig

-- | Timeout for caching system. Components that use caching on
-- messages can use this timeout to invalidate caches.
messageCacheTimeout :: (Integral a) => a
messageCacheTimeout = fromIntegral . ccMessageCacheTimeout $ compileConfig

----------------------------------------------------------------------------
-- Delegation
----------------------------------------------------------------------------

-- | Amount of time we hold confirmations for light PSKs.
lightDlgConfirmationTimeout :: (Integral a) => a
lightDlgConfirmationTimeout = fromIntegral . ccLightDlgConfirmationTimeout $ compileConfig

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
cardanoSlAppName = either (error . (<>) "Failed to init cardanoSlAppName: ")
                      identity $ mkApplicationName "cardano"

appSystemTag :: SystemTag
appSystemTag = $(do
    mbTag <- runIO (lookupEnv "CSL_SYSTEM_TAG")
    case mbTag of
        Just tag -> lift =<< mkSystemTag (toText tag)
        Nothing
            | isDevelopment ->
                  [|error "'appSystemTag' can't be used if \
                          \env var \"CSL_SYSTEM_TAG\" wasn't set \
                          \during compilation" |]
            | otherwise ->
                  fail "Failed to init appSystemTag: \
                       \couldn't find env var \"CSL_SYSTEM_TAG\"")

-- | Last block version application is aware of.
lastKnownBlockVersion :: BlockVersion
lastKnownBlockVersion = BlockVersion 0 0 0

-- | Version of application (code running)
curSoftwareVersion :: SoftwareVersion
curSoftwareVersion = SoftwareVersion cardanoSlAppName 0

-- | Name of our application.
ourAppName :: ApplicationName
ourAppName = cardanoSlAppName
