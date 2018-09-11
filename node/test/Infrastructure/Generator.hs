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
  , asAbstractChain
  ) where

import           Universum hiding (head, tail, (^.))

import           Control.Lens (to, (^.))
import           Data.Functor.Identity (runIdentity)
import           Data.List (findIndex, scanl', (!!))
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import           Data.Traversable (traverse)
import           Test.QuickCheck (Gen)


import           UTxO.DSL (GivenHash, Hash, Value, trUtxo, utxoRestrictToAddr)
import           UTxO.Generator (defChainParams, genChain, initTrState)

import           Chain.Abstract (Addr (Addr), Chain, Output (Output),
                     Parameters (Parameters), Repartition (Repartition),
                     Seed (Seed), SlotId (SlotId),
                     StakeDistribution (StakeDistribution),
                     Transaction (Transaction), bootstrapStakeholders,
                     currentSeed, currentSlot, fApply, fSum, fSupport, height,
                     inCommitmentPhase, inOpenPhase, inRecoveryPhase,
                     initTransactions, initialSeed, initialStakeDistribution,
                     k, maxMempoolSize, minFee, outAddr, outRepartition,
                     outVal, quality, slotLeader, trExtra, trFee, trFresh,
                     trHash, trIns, trOuts, trWitness)
import           Chain.Abstract.Translate.FromUTxO (ChainValidity, IntException (IntEmptyAddresses, IntEmptyInputs, IntEmptyOutputs),
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
simpleModel :: GeneratorModel GivenHash Addr
simpleModel = GeneratorModel {
      gmAllAddresses  = addrs
    , gmEstimateFee   = \_ _ -> 0
    , gmBoot          = DSL.Transaction {
                            trFresh = fromIntegral (length addrs) * initBal
                          , trIns   = Set.empty
                          , trOuts  = [DSL.Output a initBal | a <- addrs]
                          , trFee   = 0
                          , trHash  = 0
                          , trExtra = ["Simple bootstrap"]
                          }
    }
  where
    addrs :: [Addr]
    addrs = Addr <$> [0 .. 7]

    initBal :: Value
    initBal = 10000

-- | Instantiate a simple generator
simpleGen :: Gen (DSL.Chain GivenHash Addr)
simpleGen = genChainUsingModel simpleModel

-- | Simple instance of a 'DSL.Chain' to an 'Abstract.Chain' translation.
asAbstractChain
  :: DSL.Chain GivenHash Addr
  -> Either IntException (Chain GivenHash Addr, ChainValidity)
asAbstractChain ch = do
  addrs <- case chainAddresses ch of
    []   -> Left  IntEmptyAddresses
    a:as -> Right (a :| as)
  params <- simpleParams addrs initTs
  runIdentity $ translate addrs ch [] params
  where
    initTs = fromMaybe [] . safeHead . map toList . toList $ ch

simpleParams
  :: NonEmpty Addr
  -> [DSL.Transaction GivenHash Addr]
  -> Either IntException (Parameters (TransState GivenHash) GivenHash Addr)
simpleParams addrs initTs = do
  abstractInitTs <- traverse (dslTransactionToAbstract addrs) initTs
  return Parameters
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
    , initTransactions = abstractInitTs
    -- TODO: how do we want to determine the bootstrap stakeholders?
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

-- | Initial stake distribution that assigns 1 to each address passed as
-- parameter.
--
-- TODO: shouldn't this be determined based on other parameters?
mInitialStakeDistribution :: NonEmpty Addr -> StakeDistribution Addr
mInitialStakeDistribution addrs = StakeDistribution
                                $ Map.fromList
                                $ zip (toList addrs) (repeat 1)

-- | Extract the set of addresses in a chain.
chainAddresses :: DSL.Chain h a -> [a]
chainAddresses = map DSL.outAddr
               . concatMap DSL.trOuts
               . concatMap toList
               . toList

-- | Translation of DSL transactions onto Abstract transactions
--
-- TODO: this isn't right. How to translate one into the other?
dslTransactionToAbstract
  :: Ord a
  => NonEmpty a
  -> DSL.Transaction h a
  -> Either IntException (Transaction h a)
dslTransactionToAbstract addrs tx = do
  neIns <- case ins of
    []   -> Left IntEmptyInputs
    i:is -> Right $  i :| is
  neOuts <- case outs of
    []   -> Left IntEmptyOutputs
    o:os -> Right $ o :| os
  return Transaction
    { trFresh = DSL.trFresh tx
    , trIns = neIns
    , trOuts = neOuts
    , trFee = DSL.trFee tx
    , trHash = DSL.trHash tx
    , trExtra = DSL.trExtra tx
    , trWitness = addrs
    }
  where ins = Set.toList (DSL.trIns tx)
        outs = dslOutputToAbstract <$> DSL.trOuts tx

-- TODO: we need to unify this with 'FromUTxO.intOutput'!
dslOutputToAbstract :: Ord a => DSL.Output h a -> Output h1 a
dslOutputToAbstract out = Output
  { outAddr = DSL.outAddr out
  , outVal = DSL.outVal out
  -- TODO: how do we want to determine this properly?
  , outRepartition = Repartition $ Map.empty
  }
