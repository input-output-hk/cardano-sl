-- | Chain policies represent validation properties plus generators to construct
--   both validating and non-validating chains.
module Chain.Policy where

import           Chain.Abstract
import           Universum
import qualified UTxO.DSL as DSL

-- | A block modifier.
newtype BlockModifier genM h a = BlockModifier
  { modifyBlock :: Block h a -> genM (Block h a) }

instance Monad genM => Semigroup (BlockModifier genM h a) where
  (BlockModifier f1) <> (BlockModifier f2) = BlockModifier $ \bl ->
      f1 bl >>= f2

instance Monad genM => Monoid (BlockModifier genM h a) where
  mempty = BlockModifier return
  mappend = (<>)

-- | A 'Policy' should correspond to a particular aspect of the system we want
-- to test. It provides both the means to validate a chain extension against the
-- policy, and to generate valid or valid blocks.
data Policy genM = Policy
  { polGenerator  :: BlockModifier genM DSL.IdentityAsHash Addr
  }

data BlockModifierException = BlockModifierException deriving Show

instance Exception BlockModifierException
