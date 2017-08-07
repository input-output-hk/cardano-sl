-- | This module contains core constants with more descriptive types
-- (comparing to 'Pos.Core.Constants.Raw').

module Pos.Core.Constants.Typed
       (
         staticSysStart
       , staticBlkSecurityParam
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
       , genesisSoftforkRule
       , genesisUnlockStakeEpoch
       ) where

import           Universum

import           Data.Time.Units            (Millisecond, convertUnit)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (sec)

import           Pos.Core.Constants.Raw     (CoreConfig (..), coreConfig,
                                             staticSysStartRaw)
import           Pos.Core.Fee               (TxFeePolicy)
import           Pos.Core.Fee.Config        (ConfigOf (..))
import           Pos.Core.Types             (BlockCount, BlockVersionData (..),
                                             CoinPortion, EpochIndex (..), ScriptVersion,
                                             SlotCount, SoftforkRule (..), Timestamp (..),
                                             unsafeCoinPortionFromDouble)

----------------------------------------------------------------------------
-- Constants taken from the config
----------------------------------------------------------------------------

-- | System start time embedded into binary.
staticSysStart :: Timestamp
staticSysStart = Timestamp staticSysStartRaw

-- | Security parameter which is maximum number of blocks which can be
-- rolled back. This value is embedded into library and can be used
-- only for initialization. The actual value should be fetched from
-- runtime context (it can differ from this one).
staticBlkSecurityParam :: BlockCount
staticBlkSecurityParam = fromIntegral $ ccK coreConfig

-- | This function computes security parameter expressed in number of
-- slots. It uses chain quality property. It takes security parameter
-- expressed in number of blocks as an argument. It's basically
-- @blkSecurityParam / chainQualityThreshold@.
slotSecurityParam :: BlockCount -> SlotCount
slotSecurityParam = fromIntegral . (2 *)

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
chainQualityThreshold = realToFrac k / realToFrac (slotSecurityParam k)
  where
    -- Doesn't really matter, it's just ½.
    k = staticBlkSecurityParam

-- | Number of slots inside one epoch (depends on security parameter).
epochSlots :: BlockCount -> SlotCount
epochSlots blkSecurityParam = fromIntegral $ 10 * blkSecurityParam

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
    , bvdSoftforkRule = genesisSoftforkRule
    , bvdTxFeePolicy = genesisTxFeePolicy
    , bvdUnlockStakeEpoch = genesisUnlockStakeEpoch
    }

-- | ScriptVersion used at the very beginning
genesisScriptVersion :: ScriptVersion
genesisScriptVersion = 0

-- | Initial length of slot.
genesisSlotDuration :: Millisecond
genesisSlotDuration = convertUnit . sec $
    ccGenesisSlotDurationSec coreConfig

-- | Initial block size limit.
genesisMaxBlockSize :: Byte
genesisMaxBlockSize = ccGenesisMaxBlockSize coreConfig

-- | Maximum size of a block header (in bytes)
genesisMaxHeaderSize :: Byte
genesisMaxHeaderSize = ccGenesisMaxHeaderSize coreConfig

-- | See 'Pos.CompileConfig.ccGenesisMaxTxSize'.
genesisMaxTxSize :: Byte
genesisMaxTxSize = ccGenesisMaxTxSize coreConfig

-- | See 'ccGenesisMaxUpdateProposalSize'.
genesisMaxUpdateProposalSize :: Byte
genesisMaxUpdateProposalSize =
    ccGenesisMaxUpdateProposalSize coreConfig

-- | See 'Pos.CompileConfig.ccGenesisMpcThd'.
genesisMpcThd :: CoinPortion
genesisMpcThd = unsafeCoinPortionFromDouble $
    ccGenesisMpcThd coreConfig

-- | See 'Pos.CompileConfig.ccGenesisHeavyDelThd'.
genesisHeavyDelThd :: CoinPortion
genesisHeavyDelThd = unsafeCoinPortionFromDouble $
    ccGenesisHeavyDelThd coreConfig

-- | See 'ccGenesisUpdateVoteThd'.
genesisUpdateVoteThd :: CoinPortion
genesisUpdateVoteThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateVoteThd coreConfig

-- | See 'ccGenesisUpdateProposalThd'.
genesisUpdateProposalThd :: CoinPortion
genesisUpdateProposalThd = unsafeCoinPortionFromDouble $
    ccGenesisUpdateProposalThd coreConfig

-- | See 'ccGenesisUpdateImplicit'.
genesisUpdateImplicit :: Integral i => i
genesisUpdateImplicit = fromIntegral $
    ccGenesisUpdateImplicit coreConfig

-- | Genesis softfork resolution rule.
genesisSoftforkRule :: SoftforkRule
genesisSoftforkRule =
    SoftforkRule
    { srMinThd =
          unsafeCoinPortionFromDouble $ ccGenesisSoftforkMin coreConfig
    , srInitThd =
          unsafeCoinPortionFromDouble $ ccGenesisSoftforkInit coreConfig
    , srThdDecrement =
          unsafeCoinPortionFromDouble $ ccGenesisSoftforkDec coreConfig
    }

genesisTxFeePolicy :: TxFeePolicy
genesisTxFeePolicy = getConfigOf (ccGenesisTxFeePolicy coreConfig)

genesisUnlockStakeEpoch :: EpochIndex
genesisUnlockStakeEpoch = EpochIndex $
    ccGenesisUnlockStakeEpoch coreConfig
