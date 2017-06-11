-- | This module contains core constants with more descriptive types
-- (comparing to 'Pos.Core.Constants.Raw').

module Pos.Core.Constants.Typed
       (
         staticSysStart

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
       ) where

import           Universum

import           Data.Time.Units            (Millisecond, convertUnit)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util              (sec)

import           Pos.Core.Constants.Raw     (CoreConstants (..), coreConstants,
                                             staticSysStartRaw)
import           Pos.Core.Types             (BlockVersionData (..), CoinPortion,
                                             ScriptVersion, Timestamp (..),
                                             unsafeCoinPortionFromDouble)

----------------------------------------------------------------------------
-- Constants taken from the config
----------------------------------------------------------------------------

-- | System start time embedded into binary.
staticSysStart :: Timestamp
staticSysStart = Timestamp staticSysStartRaw

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
