{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module InputSelection.Evaluation (
    evaluateInputPolicies
  ) where

import           Universum hiding (Ratio (..))

import           Data.Conduit
import qualified Data.Map.Strict as Map
import           Data.Time
import           Formatting (build, sformat, (%))
import           Serokell.Util (listJson)
import           System.IO (hFlush, stdout)

import           Cardano.Wallet.Kernel.CoinSelection.Generic (CoinSelPolicy)

import           InputSelection.Evaluation.Generic
import           InputSelection.Evaluation.Options
import           InputSelection.Evaluation.TimeSeries (SlotNr (..))
import           InputSelection.FromGeneric
import           InputSelection.Generator (Event (..), World (..))
import qualified InputSelection.Generator as Gen
import           InputSelection.SortedUtxo (SortedUtxo)
import           InputSelection.TxStats
import           Util.Distr
import           Util.GenHash
import           UTxO.DSL
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Instantiate state
-------------------------------------------------------------------------------}

instance ValueToDouble (DSL h a) where
  valueToDouble = fromIntegral . fromSafeValue

instance (Hash h a, Buildable a) => IsUtxo (Utxo h a) where
  utxoSize    = DSL.utxoSize
  utxoOutputs = map (Value . DSL.outVal . snd) . DSL.utxoToList
  utxoFromMap = DSL.utxoFromMap
  utxoToMap   = DSL.utxoToMap
  utxoUnion   = DSL.utxoUnion

{-------------------------------------------------------------------------------
  Wrap policies
-------------------------------------------------------------------------------}

-- | Wrap coin selection policy to return information required by evaluator
wrap :: forall utxo h a m. (Eq a, Hash h a, Monad m)
     => Proxy utxo
     -> (a -> Word64 -> CoinSelPolicy utxo m (Transaction h a, TxStats, utxo))
     -> (a -> Word64 -> CoinSelPolicy utxo m (CoinSelSummary (DSL h a), utxo))
wrap _p f changeAddr maxNumInputs outs initUtxo =
    fmap aux <$> f changeAddr maxNumInputs outs initUtxo
  where
    aux :: (Transaction h a, TxStats, utxo)
        -> (CoinSelSummary (DSL h a), utxo)
    aux (tr, stats, finalUtxo) = (summarize tr stats, finalUtxo)

    summarize :: Transaction h a -> TxStats -> CoinSelSummary (DSL h a)
    summarize tr stats = CoinSelSummary {
          csSummaryStats     = stats
        , csSummaryOurChange = ourChange tr
        }

    ourChange :: Transaction h a -> Map (Input h a) (Output h a)
    ourChange = DSL.utxoToMap
              . utxoRestrictToAddr (== changeAddr)
              . trUtxo

-- | For the largest-first input selection we use a specialized sorted UTxO
sortedUtxo :: Proxy (SortedUtxo (DSL h a))
sortedUtxo = Proxy

-- | For the random input selection we use a standard UTxO
standardUtxo :: Proxy (Utxo h a)
standardUtxo = Proxy

largest :: Hash h World => NamedPolicy (DSL h World) (GenHashT IO)
largest = NamedPolicy "largest" $
    simpleCompPolicy
      (wrap sortedUtxo largestFirst Us maxInps)
  where
    maxInps = 50

randomOn :: Hash h World => NamedPolicy (DSL h World) (GenHashT IO)
randomOn = NamedPolicy "randomOn" $
    simpleCompPolicy
      (wrap standardUtxo (random PrivacyModeOn) Us maxInps)
  where
    maxInps = 50

randomOff :: Hash h World => NamedPolicy (DSL h World) (GenHashT IO)
randomOff = NamedPolicy "randomOff" $
    simpleCompPolicy
      (wrap standardUtxo (random PrivacyModeOff) Us maxInps)
  where
    maxInps = 50

largeThenRandom :: Hash h World => Int -> NamedPolicy (DSL h World) (GenHashT IO)
largeThenRandom changeAfter = NamedPolicy "largeThenRandom" $
    firstThen
      changeAfter
      (wrap sortedUtxo   largestFirst           Us largestMaxInps)
      (wrap standardUtxo (random PrivacyModeOn) Us randomMaxInps)
  where
    largestMaxInps = 50
    randomMaxInps  = 10

{-------------------------------------------------------------------------------
  Distributions
-------------------------------------------------------------------------------}

constant :: Int -> Constant
constant n = Constant (1000 * fromIntegral n)

normal :: Int -> Normal
normal n = Normal (1000 * fromIntegral n) (100 * fromIntegral n)

erlang :: Int -> Int -> Erlang
erlang k n = Erlang k (1 / (1000 * fromIntegral n))

{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

data Ratio =
    -- | There are more deposits than payments
    --
    -- This is the typical case for an exchange.
    --
    -- The number indicates the ratio.
    MoreDeposits Int

    -- | There are more payments than deposits
    --
    -- This might be the case for an end user.
  | MorePayments Int

evaluateInputPolicies :: EvalOptions -> SimulationOptions -> IO ()
evaluateInputPolicies evalOptions@EvalOptions{..} SimulationOptions{..} = do
    -- Our chosen policy, against different distributions of deposits/payments

    go "constant-1to1"  [randomOn] numCycles $ nTo1 (MoreDeposits  1) constant
    go "constant-3to1"  [randomOn] numCycles $ nTo1 (MoreDeposits  3) constant
    go "constant-10to1" [randomOn] numCycles $ nTo1 (MoreDeposits 10) constant
    go "constant-1to3"  [randomOn] numCycles $ nTo1 (MorePayments  3) constant
    go "constant-1to10" [randomOn] numCycles $ nTo1 (MorePayments 10) constant

    go "normal-1to1"    [randomOn] numCycles $ nTo1 (MoreDeposits  1) normal
    go "normal-3to1"    [randomOn] numCycles $ nTo1 (MoreDeposits  3) normal
    go "normal-10to1"   [randomOn] numCycles $ nTo1 (MoreDeposits 10) normal
    go "normal-1to3"    [randomOn] numCycles $ nTo1 (MorePayments  3) normal
    go "normal-1to10"   [randomOn] numCycles $ nTo1 (MorePayments 10) normal

    go "erlang1-1to1"   [randomOn] numCycles $ nTo1 (MoreDeposits  1) (erlang 1)
    go "erlang1-3to1"   [randomOn] numCycles $ nTo1 (MoreDeposits  3) (erlang 1)
    go "erlang1-10to1"  [randomOn] numCycles $ nTo1 (MoreDeposits 10) (erlang 1)
    go "erlang1-1to3"   [randomOn] numCycles $ nTo1 (MorePayments  3) (erlang 1)
    go "erlang1-1to10"  [randomOn] numCycles $ nTo1 (MorePayments 10) (erlang 1)

    go "erlang2-1to1"   [randomOn] numCycles $ nTo1 (MoreDeposits  1) (erlang 2)
    go "erlang2-3to1"   [randomOn] numCycles $ nTo1 (MoreDeposits  3) (erlang 2)
    go "erlang2-10to1"  [randomOn] numCycles $ nTo1 (MoreDeposits 10) (erlang 2)
    go "erlang2-1to3"   [randomOn] numCycles $ nTo1 (MorePayments  3) (erlang 2)
    go "erlang2-1to10"  [randomOn] numCycles $ nTo1 (MorePayments 10) (erlang 2)

    go "erlang3-1to1"   [randomOn] numCycles $ nTo1 (MoreDeposits  1) (erlang 3)
    go "erlang3-3to1"   [randomOn] numCycles $ nTo1 (MoreDeposits  3) (erlang 3)
    go "erlang3-10to1"  [randomOn] numCycles $ nTo1 (MoreDeposits 10) (erlang 3)
    go "erlang3-1to3"   [randomOn] numCycles $ nTo1 (MorePayments  3) (erlang 3)
    go "erlang3-1to10"  [randomOn] numCycles $ nTo1 (MorePayments 10) (erlang 3)

    go "erlang10-1to1"  [randomOn] numCycles $ nTo1 (MoreDeposits  1) (erlang 10)
    go "erlang10-3to1"  [randomOn] numCycles $ nTo1 (MoreDeposits  3) (erlang 10)
    go "erlang10-10to1" [randomOn] numCycles $ nTo1 (MoreDeposits 10) (erlang 10)
    go "erlang10-1to3"  [randomOn] numCycles $ nTo1 (MorePayments  3) (erlang 10)
    go "erlang10-1to10" [randomOn] numCycles $ nTo1 (MorePayments 10) (erlang 10)

   -- Other policies

    go "normal-1to1"  [largest]                          600          $ nTo1 (MoreDeposits 1) normal
    go "normal-1to1"  [randomOff]                  numCycles          $ nTo1 (MoreDeposits 1) normal
    go "normal-3to1"  [largest]                    numCycles          $ nTo1 (MoreDeposits 3) normal
    go "normal-3to1"  [largeThenRandom numCycles] (numCycles .* 1.15) $ nTo1 (MoreDeposits 3) normal
    go "normal-1to3"  [largest]                    numCycles          $ nTo1 (MorePayments 3) normal
    go "normal-1to3"  [largeThenRandom numCycles] (numCycles .* 1.15) $ nTo1 (MorePayments 3) normal
  where
    go :: FilePath  -- Prefix for this event stream
       -> [NamedPolicy (DSL GivenHash World) (GenHashT IO)] -- Policies to evaluate
       -> Int       -- Total number of cycles
       -> (Int -> ConduitT () (Event (DSL GivenHash World)) (GenHashT IO) ())
                    -- Event stream (parameterized by number of cycles)
       -> IO ()
    go eventsPrefix policies cycles events = do
      putStr $ sformat ("Running " % build % " " % listJson % ".. ")
                 eventsPrefix
                 (map namedPolicyName policies)
      hFlush stdout
      start <- getCurrentTime
      withHash 1 $ evaluateUsingEvents
                     evalOptions
                     eventsPrefix
                     initUtxo
                     policies
                     (renderEvery (cycles `div` numFrames))
                     (events cycles)
      finish <- getCurrentTime
      putStrLn $ sformat ("ok (" % build % ")") (finish `diffUTCTime` start)

    -- Render every n steps
    renderEvery :: Int -> SlotNr -> Bool
    renderEvery n step = overallSlotNr step `mod` n == 0

    -- Event stream
    nTo1 :: Distribution distr
         => Ratio
         -> (Int -> distr)       -- Distribution
         -> Int                  -- Number of cycles
         -> ConduitT () (Event (DSL GivenHash World)) (GenHashT IO) ()
    nTo1 (MoreDeposits n) distr cycles = Gen.fromDistr Gen.FromDistrParams {
          Gen.fromDistrDep    = distr 1
        , Gen.fromDistrPay    = distr n
        , Gen.fromDistrNumDep = Constant (fromIntegral n)
        , Gen.fromDistrNumPay = Constant 1
        , Gen.fromDistrCycles = cycles
        }
    nTo1 (MorePayments n) distr cycles = Gen.fromDistr Gen.FromDistrParams {
          Gen.fromDistrDep    = distr n
        , Gen.fromDistrPay    = distr 1
        , Gen.fromDistrNumDep = Constant 1
        , Gen.fromDistrNumPay = Constant (fromIntegral n)
        , Gen.fromDistrCycles = cycles
        }

    -- Initial UTxO for all these tests
    initUtxo :: Map (Input GivenHash World) (Output GivenHash World)
    initUtxo = let i = Input (GivenHash 0) 0
                   o = Output Us initBalance
               in Map.singleton i o


    (.*) :: Int -> Double -> Int
    (.*) n f = round $ fromIntegral n * f
