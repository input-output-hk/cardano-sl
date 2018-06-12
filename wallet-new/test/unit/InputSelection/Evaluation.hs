{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module InputSelection.Evaluation (
    evaluateInputPolicies
  ) where

import           Universum

import           Data.Conduit
import qualified Data.Map.Strict as Map
import           Data.Time
import           Formatting (build, sformat, (%))
import           Serokell.Util (listJson)
import           System.IO (hFlush, stdout)

import           Cardano.Wallet.Kernel.CoinSelection.Generic (CoinSelPolicy)

import           InputSelection.Evaluation.Generic
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

normal :: Int -> Normal
normal n = Normal (1000 * fromIntegral n) (100 * fromIntegral n)

erlang :: Int -> Int -> Erlang
erlang k n = Erlang k (1 / (1000 * fromIntegral n))

{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

evaluateInputPolicies :: PlotParams -> IO ()
evaluateInputPolicies plotParams@PlotParams{..} = do
    -- Our chosen policy, against different distributions of deposits/payments

    go "normal-1to1"   [randomOn] 100000 $ nTo1  1 normal
    go "normal-3to1"   [randomOn] 100000 $ nTo1  3 normal
    go "normal-10to1"  [randomOn] 100000 $ nTo1 10 normal

    go "erlang1-1to1"  [randomOn] 100000 $ nTo1  1 (erlang 1)
    go "erlang1-3to1"  [randomOn] 100000 $ nTo1  3 (erlang 1)
    go "erlang1-10to1" [randomOn] 100000 $ nTo1 10 (erlang 1)

    go "erlang2-1to1"  [randomOn] 100000 $ nTo1  1 (erlang 2)
    go "erlang2-3to1"  [randomOn] 100000 $ nTo1  3 (erlang 2)
    go "erlang2-10to1" [randomOn] 100000 $ nTo1 10 (erlang 2)

    go "erlang3-1to1"  [randomOn] 100000 $ nTo1  1 (erlang 3)
    go "erlang3-3to1"  [randomOn] 100000 $ nTo1  3 (erlang 3)
    go "erlang3-10to1" [randomOn] 100000 $ nTo1 10 (erlang 3)

   -- Other policies

    go "normal-1to1"  [largest]                   600 $ nTo1  1 normal
    go "normal-1to1"  [randomOff]              100000 $ nTo1  1 normal
    go "normal-3to1"  [largest]                100000 $ nTo1  3 normal
    go "normal-3to1"  [largeThenRandom 100000] 115000 $ nTo1  3 normal
  where
    go :: FilePath  -- Prefix for this event stream
       -> [NamedPolicy (DSL GivenHash World) (GenHashT IO)] -- Policies to evaluate
       -> Int       -- Total number of cycles
       -> (Int -> ConduitT () (Event (DSL GivenHash World)) (GenHashT IO) ())
                    -- Event stream (parameterized by number of cycles)
       -> IO ()
    go eventsPrefix policies numCycles events = do
      putStr $ sformat ("Running " % build % " " % listJson % ".. ")
                 eventsPrefix
                 (map namedPolicyName policies)
      hFlush stdout
      start <- getCurrentTime
      withHash 1 $ evaluateUsingEvents
                     plotParams
                     eventsPrefix
                     initUtxo
                     policies
                     (renderEvery (numCycles `div` numFrames))
                     (events numCycles)
      finish <- getCurrentTime
      putStrLn $ sformat ("ok (" % build % ")") (finish `diffUTCTime` start)

    -- Number of frames we want for each animation
    numFrames :: Int
    numFrames = 200

    -- Render every n steps
    renderEvery :: Int -> SlotNr -> Bool
    renderEvery n step = overallSlotNr step `mod` n == 0

    -- Event stream
    nTo1 :: Distribution distr
         => Int                  -- Ratio of deposits:withdrawals (@N:1@)
         -> (Int -> distr)       -- Distribution
         -> Int                  -- Number of cycles
         -> ConduitT () (Event (DSL GivenHash World)) (GenHashT IO) ()
    nTo1 n distr numCycles = Gen.fromDistr Gen.FromDistrParams {
          Gen.fromDistrDep    = distr 1
        , Gen.fromDistrPay    = distr n
        , Gen.fromDistrNumDep = Constant (fromIntegral n)
        , Gen.fromDistrNumPay = Constant 1
        , Gen.fromDistrCycles = numCycles
        }

    -- Initial UTxO for all these tests
    initUtxo :: Map (Input GivenHash World) (Output GivenHash World)
    initUtxo = let i = Input (GivenHash 0) 0
                   o = Output Us 1000000
               in Map.singleton i o
