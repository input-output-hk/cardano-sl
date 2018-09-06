{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Infrastructure.Generator
  ( GeneratorModel
  , genChainUsingModel
  , simpleModel
  , simpleGen
  , seeds
  ) where

import           Universum

import           Data.Functor.Identity (runIdentity)
import           Data.List (findIndex, scanl', (!!))
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import           Test.QuickCheck (Gen)

import           UTxO.DSL (GivenHash, Hash, Output (Output),
                     Transaction (Transaction), Value, trExtra, trFee, trFresh,
                     trHash, trIns, trOuts, trUtxo, utxoRestrictToAddr)
import           UTxO.Generator (defChainParams, genChain, initTrState)

import           Chain.Abstract (Addr, Chain, Parameters (Parameters),
                     Seed (Seed), SlotId (SlotId),
                     StakeDistribution (StakeDistribution),
                     bootstrapStakeholders, currentSeed, currentSlot, fApply,
                     fSum, fSupport, height, inCommitmentPhase, inOpenPhase,
                     inRecoveryPhase, initTransactions, initialSeed,
                     initialStakeDistribution, k, maxMempoolSize, minFee,
                     quality, slotLeader)
import           Chain.Abstract.Translate.FromUTxO (ChainValidity, IntException,
                     TransState, translate)
import qualified UTxO.DSL as DSL


data GeneratorModel h a = GeneratorModel
  { gmBoot         :: Transaction h a
  , gmAllAddresses :: [a]
  , gmEstimateFee  :: Int -> [Value] -> Value
  }

genChainUsingModel :: (Hash h a, Ord a) => GeneratorModel h a -> Gen (DSL.Chain h a)
genChainUsingModel GeneratorModel{gmBoot, gmAllAddresses, gmEstimateFee} =
    evalStateT (genChain params) initState
  where
    params    = defChainParams gmEstimateFee gmAllAddresses
    initUtxo  = utxoRestrictToAddr (`elem` gmAllAddresses) $ trUtxo gmBoot
    initState = initTrState initUtxo 1

{-------------------------------------------------------------------------------
  Simple model
-------------------------------------------------------------------------------}

-- | Simplified generator model
--
-- Small values, simple addresses, and no fees
simpleModel :: GeneratorModel GivenHash Char
simpleModel = GeneratorModel {
      gmAllAddresses  = addrs
    , gmEstimateFee   = \_ _ -> 0
    , gmBoot          = Transaction {
                            trFresh = fromIntegral (length addrs) * initBal
                          , trIns   = Set.empty
                          , trOuts  = [Output a initBal | a <- addrs]
                          , trFee   = 0
                          , trHash  = 0
                          , trExtra = ["Simple bootstrap"]
                          }
    }
  where
    addrs :: [Char]
    addrs = ['a' .. 'g']

    initBal :: Value
    initBal = 10000

-- | Instantiate a simple generator
simpleGen :: Gen (DSL.Chain GivenHash Char)
simpleGen = genChainUsingModel simpleModel

-- | Simple instance of a 'DSL.Chain' to an 'Abstract.Chain' translation.
asAbstractChain
  :: DSL.Chain GivenHash Addr
  -> Either IntException (Chain GivenHash Addr, ChainValidity)
asAbstractChain ch = runIdentity $ translate addrs ch [] params
  where
    addrs = undefined
    params = simpleParams

simpleParams :: Parameters (TransState GivenHash) GivenHash Addr
simpleParams = Parameters
  { slotLeader = mSlotLeader
  , currentSeed = undefined
  , currentSlot = undefined
  , height = undefined
  , quality = undefined
  , inCommitmentPhase = undefined
  , inOpenPhase = undefined
  , inRecoveryPhase = undefined
  , maxMempoolSize = undefined
  , k = undefined
  , initialStakeDistribution = undefined
  , initialSeed = undefined
  , minFee = undefined
  , initTransactions = undefined
  , bootstrapStakeholders = undefined
  }

-- | Implementation of the slot leader function, called @sl@ in section 7.2.
--
-- This function relies on a /number generator/ function, which at the moment
-- does not have /good randomness properties/. Furthermore, correctness of this
-- function relies on the total stake (that is held by all the agents) being
-- greater than zero (otherwise the number generator function is not defined).
--
mSlotLeader :: Seed -> StakeDistribution Addr -> SlotId -> Addr
mSlotLeader s0 (StakeDistribution d) (SlotId n) = addrs !! idx
  where
    -- | List of addresses, which are taken from the support of 'd'.
    addrs :: [Addr]
    addrs = Set.toList (fSupport d)

    -- | Index of the first cumulative stake bigger than 'un'.
    --
    -- NOTE: here is an example where it'd make sense not to have the @Stake@
    -- represented as an 'Int'. We could have used the 'Data.List.find'
    -- function, and still have everything type-check.
    idx :: Int
    idx = fromMaybe err $ findIndex (un <) cStake

    -- | Cumulative sum of stakes of each address.
    cStake :: [Int]
    cStake = scanl' (+) 0 $ map (getSum . fApply d) addrs

    -- | Stake held by all the agents.
    totalStake :: Int
    Sum totalStake = fSum d

    -- | Value between 0 and the total stake that we use to choose a random
    -- agent.
    --
    -- TODO: we need to assert that the '0 < totalStake'.
    un :: Int
    (un, _) = seeds s0 totalStake !! n

    -- | Note that we should be able to find an index @i@ such that @un <
    -- cStake !! i@, since @un < cStake !! (length cStake - 1) == totalStake@.
    --
    -- TODO: consider using liquid Haskell to actually verify this!
    err =  error $  "The impossible has happened: "
                 <> "could not find an index that accumulates enough stake."

-- | Sequence of random number and seeds.
seeds :: Seed -> Int -> [(Int, Seed)]
seeds _ 0  = []
seeds s0 n = (n, s0):[ numGen n sn | (_, sn) <- seeds s0 n ]

-- | Implementation of the number generator function as described in Section
-- 7.2 (called @f@).
--
-- Here the 'Int' values represent stake values.
--
-- Ideally `numGen` should be surjective so that each stakeholder could be
-- selected by some random seed.
--
-- Note that this function is not defined when @i == 0@
numGen :: Int  -> Seed -> (Int, Seed)
-- Some dummy implementation for now...
numGen i (Seed s) = (s `mod` i, Seed (s + 1))
