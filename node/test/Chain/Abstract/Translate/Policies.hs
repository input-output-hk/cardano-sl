-- | Policies for constructing chains.
module Chain.Abstract.Translate.Policies where

import           Chain.Abstract
import           Chain.Abstract.Translate.FromUTxO
import           Chain.Policy
import           Control.Lens.TH
import           Data.Default
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Universum

-- | Exception which can be thrown when generating blocks.
data PolicyGenException = PolicyGenException
    deriving (Show, Eq)

instance Exception PolicyGenException

-- | Exception which can be thrown when validating blocks.
data PolicyValException = PolicyValException
    deriving (Show, Eq)

instance Exception PolicyValException

-- | Generation monad
type GenM h = TranslateT h PolicyGenException Gen

-- | Percentage type - numbers must be bound between 0 and 100.
newtype Percentage = Percentage Int
    deriving (Eq, Ord)

mkPercent :: Int -> Maybe Percentage
mkPercent i | i >= 0 && i <= 100 = Just $ Percentage i
mkPercent _ = Nothing

instance Arbitrary Percentage where
    arbitrary = Percentage <$> choose (0, 100)

--------------------------------------------------------------------------------
-- Block predecessor
--------------------------------------------------------------------------------

data PredecessorConfig = PredecessorConfig
    { _predecessorConfigValidLikelihood :: Percentage
    }

instance Default PredecessorConfig where
    def = PredecessorConfig $ Percentage 99

makeFields ''PredecessorConfig

-- | The previous block hash in a new block should point to the last block.
predecessor :: PredecessorConfig
            -> Policy h (GenM h)
predecessor conf = Policy pname (BlockModifier mb)
    where
        pname = PolicyName "Block Predecessor"
        mb block = do
            c :| _ <- use tsCheckpoints
            valid <- lift arbitrary
            return $ if (valid >= conf ^. validLikelihood)
                then (block { blockPred = icBlockHash c }, mempty)
                else ( block { blockPred = invalidBlockHash }
                     , [PolicyViolation pname "Predecessor hash does not match hash of previous block."]
                     )

--------------------------------------------------------------------------------
-- Not future slot
--------------------------------------------------------------------------------

data NotFutureSlotConfig = NotFutureSlotConfig
    { _notFutureSlotConfigValidLikelihood :: Percentage
    }

instance Default NotFutureSlotConfig where
    def = NotFutureSlotConfig $ Percentage 99

makeFields ''NotFutureSlotConfig

-- | Slot for a block must be larger than the slot number for the last block.
notFutureSlot :: NotFutureSlotConfig
              -> Policy h (GenM h)
notFutureSlot conf = Policy pname (BlockModifier mb)
    where
        pname = PolicyName "Not future slot"
        mb block =  do
            currentSlot <- use tsCurrentSlot
            valid <- lift arbitrary
            return $ if valid >= conf ^. validLikelihood
                -- By default, blocks are issued in the current slot
                then (block, mempty)
                else ( block { blockSlot = nextSlot currentSlot }
                     , [PolicyViolation pname "Block issued in a future slot."]
                     )


--------------------------------------------------------------------------------
-- Not past slot
--------------------------------------------------------------------------------

data NotPastSlotConfig = NotPastSlotConfig
    { _notPastSlotConfigValidLikelihood :: Percentage
    }

instance Default NotPastSlotConfig where
    def = NotPastSlotConfig $ Percentage 99

makeFields ''NotPastSlotConfig

-- | Slot for a block must be larger than the slot number for the last block.
notPastSlot :: NotPastSlotConfig
            -> Policy h (GenM h)
notPastSlot conf = Policy pname (BlockModifier mb)
    where
        pname = PolicyName "Not past slot"
        mb block =  do
            c :| _ <- use tsCheckpoints
            let lastSlot = icSlotId c
            valid <- lift arbitrary
            return $ if valid >= conf ^. validLikelihood
                -- By default, blocks are issued in the current slot
                then (block, mempty)
                else ( block { blockSlot = lastSlot }
                     , [PolicyViolation pname "Block issued in a past slot."]
                     )

--------------------------------------------------------------------------------
-- Slot leader
--------------------------------------------------------------------------------

data SlotLeaderConfig = SlotLeaderConfig
    { _slotLeaderConfigValidLikelihood :: Percentage
    }

instance Default SlotLeaderConfig where
    def = SlotLeaderConfig $ Percentage 99

makeFields ''SlotLeaderConfig

-- | Block must be issued by the slot leader for its slot.
slotLeader :: SlotLeaderConfig
           -> Policy h (GenM h)
slotLeader conf = Policy pname (BlockModifier mb)
    where
        pname = PolicyName "Slot Leader"
        mb block = do
            st <- get
            c :| _ <- use tsCheckpoints
            params <- view parameters
            let seed = currentSeed params $ st
                stake = icStakes c
                slotId = currentSlot params $ st
                sl = (Chain.Abstract.slotLeader params) seed stake slotId
            valid <- lift arbitrary
            return $ if valid >= conf ^. validLikelihood
                then (block { blockIssuer = sl }, mempty)
                else ( block
                     , [PolicyViolation pname "Block issued by somebody other than the slot leader."]
                     )
