-- | Policies for constructing chains.
module Chain.Abstract.Translate.Policies where

import           Chain.Abstract
import           Chain.Abstract.Translate.FromUTxO
import           Chain.Policy
import           Control.Lens.TH
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
-- Slot leader
--------------------------------------------------------------------------------

data SlotLeaderConfig = SlotLeaderConfig
    { _slotLeaderConfigValidLikelihood :: Percentage
    }

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

--------------------------------------------------------------------------------
-- Block validation policies
--------------------------------------------------------------------------------
checkPredecessor :: Policy h (GenM h)
checkPredecessor = Policy pname (BlockModifier validator)
    where
        pname = PolicyName "Block Predecessor"
        validator block = do
            c :| r <- use tsCheckpoints
            return (block, pv c (1 + length r) block)
        pv _ _  block | blockPred block == invalidBlockHash    = [violation]
        pv _ ht block | blockPred block == genesisBlockHash &&
                        ht == 1                                = []
        pv c _  block | blockPred block /= icBlockHash c       = [violation]
        pv _ _ _                                               = []
        violation = PolicyViolation pname description
        description = "Predecessor hash does not match hash of previous block."
