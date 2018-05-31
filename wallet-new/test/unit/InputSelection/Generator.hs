module InputSelection.Generator (
    Event(..)
    -- * Generators
    -- ** Test graph output
  , TestParams(..)
  , defTestParams
  , test
    -- ** Simple generators
  , World(..)
  , trivial
  , FromDistrParams(..)
  , fromDistr
  ) where

import           Universum

import           Data.Conduit
import           Test.QuickCheck

import           Cardano.Wallet.Kernel.CoinSelection.Types

import           InputSelection.Policy (HasTreasuryAddress (..), LiftQuickCheck (..))
import           Util.Distr
import           UTxO.DSL


{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event h a =
    Deposit (Utxo h a)
  | Pay ExpenseRegulation [Output a]
  -- ^ A list of 'Output' we want to pay together with an 'ExpenseRegulation'
  -- policy for all of them.
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
        yield $ Pay SenderPaysFees [Output () n]
  where
    vals :: [Value]
    vals = [testParamsMin, testParamsMin + testParamsIncr .. testParamsMax]

    ixs :: [Word32]
    ixs = [1 .. fromIntegral testParamsCount]

{-------------------------------------------------------------------------------
  Trivial generator
-------------------------------------------------------------------------------}

-- | It is well-known that the world divides into us versus them.
data World = Us | Them | Treasury
  deriving (Eq)

instance HasTreasuryAddress World where
  treasuryAddr = Treasury

-- | Trivial generator where single deposits drawn from a normal distribution
-- are followed by single withdrawals from that same distribution.
trivial :: LiftQuickCheck m
        => NormalDistr Value  -- ^ Distribution to draw from
        -> Int                -- ^ Number of deposit/withdraw/confirm cycles
        -> ConduitT () (Event GivenHash World) m ()
trivial d n = fromDistr $ FromDistrParams d d (ConstDistr 1) (ConstDistr 1) n

{-------------------------------------------------------------------------------
  Generalization of 'trivial'
-------------------------------------------------------------------------------}

-- | Parameters for 'fromDistr'
data FromDistrParams fDep fPay fNumDep fNumPay =
    ( Distribution fDep
    , Distribution fPay
    , Distribution fNumDep
    , Distribution fNumPay
    ) => FromDistrParams {
      -- | Distribution of deposit values
      fromDistrDep    :: fDep Value

      -- | Distribution of payment values
    , fromDistrPay    :: fPay Value

      -- | Distribution of number of deposits
    , fromDistrNumDep :: fNumDep Int

      -- | Distribution of number of payments
    , fromDistrNumPay :: fNumPay Int

      -- | Number of cycles
    , fromDistrCycles :: Int
    }

-- | Like 'trivial', but with different distributions for payments and deposits,
-- and generalized to have distributions also for the /number/ of payments
-- and deposits.
fromDistr :: LiftQuickCheck m
          => FromDistrParams fDep fPay fNumDep fNumPay
          -> ConduitT () (Event GivenHash World) m ()
fromDistr FromDistrParams{..} = do
    forM_ [1 .. fromDistrCycles] $ \i -> do
      events <- lift $ liftQuickCheck $ do
        let mkDep :: Int -> Gen (Event GivenHash World)
            mkDep j =
                Deposit . aux <$> drawFromDistr' fromDistrDep
              where
                aux :: Value -> Utxo GivenHash World
                aux val = utxoSingleton (Input (GivenHash i) j') (Output Us val)

                j' :: Word32
                j' = fromIntegral j

            mkPay :: Gen (Event GivenHash World)
            mkPay =
                Pay SenderPaysFees . aux <$> drawFromDistr' fromDistrPay
              where
                aux :: Value -> [Output World]
                aux val = [Output Them val]

        numDep <- drawFromDistr' fromDistrNumDep
        numPay <- drawFromDistr' fromDistrNumPay
        deps <- forM [1 .. numDep] $ mkDep
        pays <- replicateM numPay  $ mkPay

        (reverse . (NextSlot :)) <$> shuffle (deps ++ pays)

      mapM_ yield events
