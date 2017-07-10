-- | This module contains core constants with more descriptive types
-- (comparing to 'Pos.Core.Constants.Raw').

module Pos.Core.Constants.Typed
       (
         staticSysStart
       , blkSecurityParam
       , slotSecurityParam
       , chainQualityThreshold
       , epochSlots

       -- * Genesis constants
       , genesisBlockVersionData
       , genesisScriptVersion
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
       , genesisUnlockStakeEpoch
       ) where

import           Universum

import           Data.Time.Units            (Millisecond, convertUnit)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (sec)

import           Pos.Core.Constants.Raw     (CoreConstants (..), coreConstants,
                                             staticSysStartRaw)
import           Pos.Core.Fee               (TxFeePolicy)
import           Pos.Core.Fee.Config        (ConfigOf (..))
import           Pos.Core.Types             (BlockCount, BlockVersionData (..),
                                             CoinPortion, EpochIndex (..), ScriptVersion,
                                             SlotCount, Timestamp (..),
                                             unsafeCoinPortionFromDouble)

----------------------------------------------------------------------------
-- Constants taken from the config
----------------------------------------------------------------------------

-- | System start time embedded into binary.
staticSysStart :: Timestamp
staticSysStart = Timestamp staticSysStartRaw

-- | Security parameter which is maximum number of blocks which can be
-- rolled back.
blkSecurityParam :: BlockCount
blkSecurityParam = fromIntegral $ ccK coreConstants

-- | Security parameter expressed in number of slots. It uses chain
-- quality property. It's basically @blkSecurityParam / chainQualityThreshold@.
slotSecurityParam :: SlotCount
slotSecurityParam = fromIntegral $ 2 * ccK coreConstants

-- We don't have a special newtype for it, so it can be any
-- 'Fractional'. I think adding newtype here would be overkill
-- (@gromak). Also this value is not actually part of the protocol,
-- but rather implementation detail, so we don't need to ensure
-- conrete precision. Apart from that, in reality we know that it's
-- 0.5, so any fractional type should be fine ☺
--
-- | Minimal chain quality (number of blocks divided by number of
-- slots) necessary for security of the system.
chainQualityThreshold :: Fractional fractional => fractional
chainQualityThreshold =
    realToFrac blkSecurityParam / realToFrac slotSecurityParam

-- | Number of slots inside one epoch.
epochSlots :: SlotCount
epochSlots = fromIntegral $ 10 * ccK coreConstants

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

-- | 'BlockVersionData' for genesis 'BlockVersion'.
genesisBlockVersionData :: BlockVersionData
genesisBlockVersionData =
    BlockVersionData
    { bvdScriptVersion = genesisScriptVersion
    , bvdSlotDuration = genesisSlotDuration
    , bvdMaxBlockSize = genesisMaxBlockSize
    , bvdMaxHeaderSize = genesisMaxHeaderSize
    , bvdMaxTxSize = genesisMaxTxSize
    , bvdMaxProposalSize = genesisMaxUpdateProposalSize
    , bvdMpcThd = genesisMpcThd
    , bvdHeavyDelThd = genesisHeavyDelThd
    , bvdUpdateVoteThd = genesisUpdateVoteThd
    , bvdUpdateProposalThd = genesisUpdateProposalThd
    , bvdUpdateImplicit = genesisUpdateImplicit
    , bvdUpdateSoftforkThd = genesisUpdateSoftforkThd
    , bvdTxFeePolicy = genesisTxFeePolicy
    , bvdUnlockStakeEpoch = genesisUnlockStakeEpoch
    }

-- | ScriptVersion used at the very beginning
genesisScriptVersion :: ScriptVersion
genesisScriptVersion = 0

-- | Initial length of slot.
genesisSlotDuration :: Millisecond
genesisSlotDuration = convertUnit . sec $
    ccGenesisSlotDurationSec coreConstants

-- | Initial block size limit.
genesisMaxBlockSize :: Byte
genesisMaxBlockSize = ccGenesisMaxBlockSize coreConstants

-- | Maximum size of a block header (in bytes)
genesisMaxHeaderSize :: Byte
genesisMaxHeaderSize = ccGenesisMaxHeaderSize coreConstants

-- | See 'Pos.CompileConfig.ccGenesisMaxTxSize'.
genesisMaxTxSize :: Byte
genesisMaxTxSize = ccGenesisMaxTxSize coreConstants

-- | See 'ccGenesisMaxUpdateProposalSize'.
genesisMaxUpdateProposalSize :: Byte
genesisMaxUpdateProposalSize =
    ccGenesisMaxUpdateProposalSize coreConstants

-- | See 'Pos.CompileConfig.ccGenesisMpcThd'.
genesisMpcThd :: CoinPortion
genesisMpcThd = unsafeCoinPortionFromDouble $
    ccGenesisMpcThd coreConstants

-- | See 'Pos.CompileConfig.ccGenesisHeavyDelThd'.
genesisHeavyDelThd :: CoinPortion
genesisHeavyDelThd = unsafeCoinPortionFromDouble $
    ccGenesisHeavyDelThd coreConstants

-- | See 'ccGenesisUpdateVoteThd'.
genesisUpdateVoteThd :: CoinPortion
genesisUpdateVoteThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateVoteThd coreConstants

-- | See 'ccGenesisUpdateProposalThd'.
genesisUpdateProposalThd :: CoinPortion
genesisUpdateProposalThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateProposalThd coreConstants

-- | See 'ccGenesisUpdateImplicit'.
genesisUpdateImplicit :: Integral i => i
genesisUpdateImplicit = fromIntegral $
    ccGenesisUpdateImplicit coreConstants

-- | See 'ccGenesisUpdateSoftforkThd'.
genesisUpdateSoftforkThd :: CoinPortion
genesisUpdateSoftforkThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateSoftforkThd coreConstants

genesisTxFeePolicy :: TxFeePolicy
genesisTxFeePolicy = getConfigOf (ccGenesisTxFeePolicy coreConstants)

genesisUnlockStakeEpoch :: EpochIndex
genesisUnlockStakeEpoch = EpochIndex $
    ccGenesisUnlockStakeEpoch coreConstants
