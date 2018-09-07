{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Infrastructure.Generator
  ( GeneratorModel
  , genChainUsingModel
  , simpleModel
  , simpleGen
  , seeds
  , mInCommitmentPhase
  , chainAddresses
  ) where

import           Universum hiding ((^.))

import           Control.Lens (to, (^.))
import           Data.Functor.Identity (runIdentity)
import           Data.List (findIndex, scanl', (!!))
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import           Test.QuickCheck (Gen)

import           UTxO.DSL (GivenHash, Hash, Output (Output), Value, outAddr,
                     trExtra, trFee, trFresh, trHash, trIns, trOuts, trUtxo,
                     utxoRestrictToAddr)
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
                     TransState, translate, tsCheckpoints, _tsCurrentSlot)
import qualified UTxO.DSL as DSL


data GeneratorModel h a = GeneratorModel
  { gmBoot         :: DSL.Transaction h a
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
    , gmBoot          = DSL.Transaction {
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
asAbstractChain ch = runIdentity $ translate addrs ch [] (params addrs initTs)
  where
    addrs = undefined
    initTs = fromMaybe [] . safeHead . map toList . toList $ ch
    params = simpleParams

simpleParams
  :: NonEmpty Addr
  -> [DSL.Transaction GivenHash Addr]
  -> Parameters (TransState GivenHash) GivenHash Addr
simpleParams addrs initTs = Parameters
  { slotLeader = mSlotLeader
  , currentSeed = mCurrentSeed
  , currentSlot = _tsCurrentSlot
  , height = mHeight
  , quality = mQuality
  , inCommitmentPhase = mInCommitmentPhase
  , inOpenPhase = mInOpenPhase
  , inRecoveryPhase = mInRecoveryPhase
  , maxMempoolSize = 200
  , k = 2160
  , initialStakeDistribution = mInitialStakeDistribution addrs
  , initialSeed = Seed 0
  , minFee = const 0 -- TODO: QUESTION: what properties should 'minFee' satisfy?
  , initTransactions = initTs
  , bootstrapStakeholders = Set.fromList $ toList addrs
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

-- | For now we return the same seed. Ideally we'd need to perform an SCC
-- computation, which uses the stake distribution and the VSS payloads (which
-- should be tracked in the state).
--
mCurrentSeed :: TransState GivenHash -> Seed
mCurrentSeed = const (Seed 15)

-- | Compute the number of blocks as the height of the chain (See Section 7.1).
-- The number of blocks can be obtained from the list of checkpoints in the
-- translation state, which contains a checkpoint for each block that we
-- translate.
mHeight :: TransState GivenHash -> Int
mHeight st = st ^. tsCheckpoints . to length

-- | Quality of a chain. It maps a number of blocks into a number of blocks.
mQuality :: Int -> Int
mQuality = (*2)

-- | Is the slot in the commitment phase?
mInCommitmentPhase :: SlotId -> Bool
mInCommitmentPhase (SlotId k) = 0 <= k && k < mQuality k

-- | Is the slot in the opening phase.
mInOpenPhase :: SlotId -> Bool
mInOpenPhase (SlotId k) = 2 * mQuality k <= k && k < 3 * mQuality k

-- | Is the slot in the recovery phase.
mInRecoveryPhase :: SlotId -> Bool
mInRecoveryPhase (SlotId k) = 4 * mQuality k <= k && k < 5 * mQuality k

mInitialStakeDistribution :: NonEmpty Addr -> StakeDistribution Addr
mInitialStakeDistribution addrs = StakeDistribution
                                $ Map.fromList
                                $ zip (toList addrs) (repeat 1)

-- | Extract the set of addresses in a chain.
chainAddresses :: Ord a => DSL.Chain h a -> Set a
chainAddresses = Set.fromList
               . map outAddr
               . concatMap trOuts
               . concatMap toList
               . toList

