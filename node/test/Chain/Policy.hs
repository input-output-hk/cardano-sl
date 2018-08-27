-- | Chain policies represent validation properties plus generators to construct
--   both validating and non-validating chains.
module Chain.Policy where

import           Chain.Abstract
import           Universum
import qualified UTxO.DSL as DSL
import qualified Data.Text as T

newtype PolicyName = PolicyName T.Text

-- | Record the violation of a policy. Generators may choose to create invalid
-- blocks, and if they do so then they should tag them with a relevant
-- 'PolicyViolation' value.
data PolicyViolation = PolicyViolation
  { pvPolName :: PolicyName
  , pvDescription :: T.Text
  }

-- | A block modifier.
newtype BlockModifier genM h a = BlockModifier
  { modifyBlock :: Block h a -> genM (Block h a, [PolicyViolation]) }

instance Monad genM => Semigroup (BlockModifier genM h a) where
  (BlockModifier f1) <> (BlockModifier f2) = BlockModifier $ \bl -> do
    (a, vs) <- f1 bl
    (a', vs') <- f2 a
    return (a', vs ++ vs')

instance Monad genM => Monoid (BlockModifier genM h a) where
  mempty = BlockModifier $ \a -> return (a, mempty)
  mappend = (<>)

-- | A 'Policy' should correspond to a particular aspect of the system we want
-- to test. It provides both the means to validate a chain extension against the
-- policy, and to generate valid or valid blocks.
data Policy genM = Policy
  { -- | Name of this policy. This is included when a policy has been violated.
    polName :: PolicyName
  , polGenerator :: BlockModifier genM DSL.IdentityAsHash Addr
  }

data BlockModifierException = BlockModifierException deriving Show

instance Exception BlockModifierException
