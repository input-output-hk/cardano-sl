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
            -> Policy (GenM h)
predecessor conf = Policy pg
    where
        pg = BlockModifier mb
        mb block = do
            c :| _ <- use tsCheckpoints
            valid <- lift arbitrary
            return $ if (valid >= conf ^. validLikelihood)
                then block { blockPred = icBlockHash c }
                else block { blockPred = invalidBlockHash }

--------------------------------------------------------------------------------
-- Slot leader
--------------------------------------------------------------------------------

data SlotLeaderConfig = SlotLeaderConfig
    { _slotLeaderConfigValidLikelihood :: Percentage
    }

makeFields ''SlotLeaderConfig

-- | Block must be issued by the slot leader for its slot.
slotLeader :: SlotLeaderConfig
           -> Policy (GenM h)
slotLeader conf = Policy pg
    where
        pg = BlockModifier mb
        mb block = return block
