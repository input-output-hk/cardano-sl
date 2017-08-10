-- | This module contains core constants with more descriptive types
-- (comparing to 'Pos.Core.Constants.Raw').

module Pos.Core.Constants.Typed
       (
         staticSysStart
       , staticBlkSecurityParam

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
                                             SoftforkRule (..), Timestamp (..),
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
