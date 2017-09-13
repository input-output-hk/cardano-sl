{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.BlockVersionData
       ( HasBlockVersionData
       , withBlockVersionData
       , blockVersionData
       , scriptVersion
       , slotDuration
       , maxBlockSize
       , maxHeaderSize
       , maxTxSize
       , mpcThd
       , heavyDelThd
       , updateVoteThd
       , maxUpdateProposalSize
       , updateProposalThd
       , updateImplicit
       , softforkRule
       , txFeePolicy
       , unlockStakeEpoch
       ) where

import           Universum

import           Data.Reflection            (Given (..), give)
import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core.Fee               (TxFeePolicy)
import           Pos.Core.Types             (BlockVersionData (..), ScriptVersion,
                                             CoinPortion, EpochIndex (..),
                                             SoftforkRule (..))

type HasBlockVersionData = Given BlockVersionData

withBlockVersionData :: BlockVersionData -> (HasBlockVersionData => r) -> r
withBlockVersionData = give

blockVersionData :: HasBlockVersionData => BlockVersionData
blockVersionData = given

-- | ScriptVersion used at the very beginning
scriptVersion :: HasBlockVersionData => ScriptVersion
scriptVersion = bvdScriptVersion blockVersionData

-- | Initial length of slot.
slotDuration :: HasBlockVersionData => Millisecond
slotDuration = bvdSlotDuration blockVersionData

-- | Initial block size limit.
maxBlockSize :: HasBlockVersionData => Byte
maxBlockSize = bvdMaxBlockSize blockVersionData

-- | Maximum size of a block header (in bytes)
maxHeaderSize :: HasBlockVersionData => Byte
maxHeaderSize = bvdMaxHeaderSize blockVersionData

maxTxSize :: HasBlockVersionData => Byte
maxTxSize = bvdMaxTxSize blockVersionData

maxUpdateProposalSize :: HasBlockVersionData => Byte
maxUpdateProposalSize = bvdMaxProposalSize blockVersionData

-- | See 'Pos.CompileConfig.ccGenesisMpcThd'.
mpcThd :: HasBlockVersionData => CoinPortion
mpcThd = bvdMpcThd blockVersionData

-- | See 'Pos.CompileConfig.ccGenesisHeavyDelThd'.
heavyDelThd :: HasBlockVersionData => CoinPortion
heavyDelThd = bvdHeavyDelThd blockVersionData

-- | See 'ccGenesisUpdateVoteThd'.
updateVoteThd :: HasBlockVersionData => CoinPortion
updateVoteThd = bvdUpdateVoteThd blockVersionData

-- | See 'ccGenesisUpdateProposalThd'.
updateProposalThd :: HasBlockVersionData => CoinPortion
updateProposalThd = bvdUpdateProposalThd blockVersionData

-- | See 'ccGenesisUpdateImplicit'.
updateImplicit :: (HasBlockVersionData, Integral i) => i
updateImplicit = fromIntegral $ bvdUpdateImplicit blockVersionData

-- | Genesis softfork resolution rule.
softforkRule :: HasBlockVersionData => SoftforkRule
softforkRule = bvdSoftforkRule blockVersionData

txFeePolicy :: HasBlockVersionData => TxFeePolicy
txFeePolicy = bvdTxFeePolicy blockVersionData

unlockStakeEpoch :: HasBlockVersionData => EpochIndex
unlockStakeEpoch = bvdUnlockStakeEpoch blockVersionData
