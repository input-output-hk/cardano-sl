{-# LANGUAGE DeriveFunctor #-}

module InputSelection.Generator (
    Event(..)
    -- * Generators
    -- ** Test graph output
  , TestParams(..)
  , defTestParams
  , test
    -- ** Trivial
  , World(..)
  , trivial
    -- * Auxiliary: normal distribution
  , Mean(..)
  , StdDev(..)
  , drawFromNormal
  , drawFromNormal'
  ) where

import           Universum

import           Data.Conduit
import           Data.Random.Normal (normal)
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen

import           InputSelection.Policy (LiftQuickCheck (..))
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event h a =
    Deposit (Utxo h a)
  | Pay [Output a]
  | NextSlot

{-------------------------------------------------------------------------------
  Testing
-------------------------------------------------------------------------------}

-- | Parameters for 'test'
--
-- For each output value in the range
--
-- > (min, min + incr .. max)
--
-- we generate @count@ outputs. After that, we generate transactions that
-- use up those outputs in precisely the same order, generating no change.
data TestParams = TestParams {
      testParamsMin   :: Value
    , testParamsMax   :: Value
    , testParamsIncr  :: Value
    , testParamsCount :: Int
    }

defTestParams :: TestParams
defTestParams = TestParams {
      testParamsMin   = 10
    , testParamsMax   = 100
    , testParamsIncr  = 10
    , testParamsCount = 10
    }

-- | Series of events to test the graph output
--
-- The point is that this allows us to visually see immediately if the resulting
-- graph animation makes sense. See 'TestParams' for details.
test :: Monad m => TestParams -> ConduitT () (Event GivenHash ()) m ()
test TestParams{..} = do
    forM_ vals $ \n ->
      forM_ ixs $ \m ->
        yield $ Deposit $ utxoFromList [
            (Input (GivenHash (fromIntegral n)) m, Output () n)
          ]

    forM_ vals $ \n ->
      forM_ ixs $ \_m ->
        yield $ Pay [Output () n]
  where
    vals :: [Value]
    vals = [testParamsMin, testParamsMin + testParamsIncr .. testParamsMax]

    ixs :: [Word32]
    ixs = [1 .. fromIntegral testParamsCount]

{-------------------------------------------------------------------------------
  Trivial generator
-------------------------------------------------------------------------------}

-- | It is well-known that the world divides into us versus them.
data World = Us | Them
  deriving (Eq)

-- | Trivial generator where single deposits drawn from a normal distribution
-- are followed by single withdrawals from that same distribution.
trivial :: LiftQuickCheck m
        => Mean Value    -- ^ Mean
        -> StdDev Value  -- ^ Standard deviation
        -> Word32        -- ^ Number of deposit/withdraw/confirm cycles
        -> ConduitT () (Event GivenHash World) m ()
trivial mean sigma n = do
    forM_ [1 .. n] $ \i -> do
      [dep, pay] <- lift $ liftQuickCheck $ replicateM 2 $ drawFromNormal' mean sigma
      yield $ Deposit (utxoFromList [(Input (GivenHash 1) i, Output Us dep)])
      yield $ Pay [Output Them pay]
      yield $ NextSlot

{-------------------------------------------------------------------------------
  Auxiliary: normal distribution
-------------------------------------------------------------------------------}

newtype Normal a = Normal a

instance (Floating a, Random a) => Random (Normal a) where
  random  = (\ (x, g) -> (Normal x, g)) . normal
  randomR = error "randomR not defined for Normal"

instance (Floating a, Random a) => Arbitrary (Normal a) where
  arbitrary = chooseAny

-- | Mean
newtype Mean a = Mean a
  deriving (Functor)

-- | Standard derivation
newtype StdDev a = StdDev a
  deriving (Functor)

drawFromNormal :: forall a. (Floating a, Random a)
               => Mean a -> StdDev a -> Gen a
drawFromNormal (Mean mean) (StdDev sigma) = aux <$> chooseAny
  where
    aux :: Normal a -> a
    aux (Normal x) = x * sigma + mean

drawFromNormal' :: Mean Value -> StdDev Value -> Gen Value
drawFromNormal' mean sigma =
    aux <$> drawFromNormal (fromIntegral <$> mean) (fromIntegral <$> sigma)
  where
    aux :: Double -> Value
    aux = round
