module Chain.Validation.Parameters where

import Chain.Abstract
import qualified Data.Set as Set
import Universum

-- | Parameters to chain validation.
--
--   This is defined in section 4 of the paper. These parameters act as the
--   environment within which we can validate chain extension or other
--   operations.
data Parameters st h a = Parameters
  { slotLeader :: st -> StakeDistribution a -> SlotId -> a
  , currentSlot :: st -> SlotId
  , height :: Chain h a -> Int
  , quality :: Int -> Int
  , inCommitmentPhase :: SlotId -> Bool
  , inOpenPhase :: SlotId -> Bool
  , inRecoveryPhase :: SlotId -> Bool
  , maxMempoolSize :: Int
    -- | The chain rollback limit. This is defined in the original Ouroboros
    -- paper. After this many blocks, a block is considered stable and may not
    -- be rolled back.
  , k :: Int
  , initialStakeDistribution :: StakeDistribution a
  , initialSeed :: st
  , minFee :: Transaction h a -> Int
  , initTransactions :: [Transaction h a]
  , bootstrapStakeholders :: Set.Set a
  }
