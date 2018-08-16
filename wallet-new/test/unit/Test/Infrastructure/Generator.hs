module Test.Infrastructure.Generator (
    -- * Generator model and corresponding generators
    GeneratorModel(..)
  , genChainUsingModel
  , genInductiveUsingModel
    -- * Specific models
    -- ** Simple model
  , simpleModel
    -- ** Cardano
  , cardanoModel
  ) where

import           Universum

import qualified Data.Set as Set
import           Test.QuickCheck

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (estimateCardanoFee)
import           UTxO.Context
import           UTxO.DSL
import           UTxO.Generator
import           Wallet.Inductive
import           Wallet.Inductive.Generator

import           Pos.Core (TxSizeLinear)

{-------------------------------------------------------------------------------
  Generator model
-------------------------------------------------------------------------------}

-- | 'Chain' and 'Inductive' generator model
--
--  The generators are polymorphic in the types of addresses we have, and need
--  various parameters. Here we introduce a simple model from which we can
--  derive all of these arguments. See 'simpleModel' and 'cardanoModel'.
data GeneratorModel h a = GeneratorModel {
      -- | Bootstrap transaction
      gmBoot         :: Transaction h a

      -- | Addresses to work with
      --
      -- These will be the addresses we can transfers funds from and to
    , gmAllAddresses :: [a]

     -- | Which subset of 'gmAllAddresses' can we choose from for @ours@?
    , gmOurs         :: Set a

      -- | Estimate fees
    , gmEstimateFee  :: Int -> [Value] -> Value
    }

genChainUsingModel :: (Hash h a, Ord a) => GeneratorModel h a -> Gen (Chain h a)
genChainUsingModel GeneratorModel{..} =
    evalStateT (genChain params) initState
  where
    params    = defChainParams gmEstimateFee gmAllAddresses
    initUtxo  = utxoRestrictToAddr (`elem` gmAllAddresses) $ trUtxo gmBoot
    initState = initTrState initUtxo 1

genInductiveUsingModel :: (Hash h a, Ord a)
                       => GeneratorModel h a -> Gen (Inductive h a)
genInductiveUsingModel GeneratorModel{..} = do
    events <- evalStateT (genWalletEvents (params gmOurs)) initState
    return Inductive {
        inductiveBoot   = gmBoot
      , inductiveOurs   = gmOurs
      , inductiveEvents = events
      }
  where
    initUtxo  = utxoRestrictToAddr (`elem` gmAllAddresses) $ trUtxo gmBoot
    initState = initEventsGlobalState 1

    params ours'  = defEventsParams gmEstimateFee gmAllAddresses ours' initUtxo

{-------------------------------------------------------------------------------
  Simple model
-------------------------------------------------------------------------------}

-- | Simplified generator model
--
-- Small values, simple addresses, and no fees
simpleModel :: GeneratorModel GivenHash Char
simpleModel = GeneratorModel {
      gmAllAddresses  = addrs
    , gmOurs          = Set.fromList addrs
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

{-------------------------------------------------------------------------------
  Cardano model
-------------------------------------------------------------------------------}

-- | The Cardano itself (given the bootstrap transaction).
--
-- This is a model that results in something that we can translate to Cardano,
-- but since it deals with the " real world " it has all kinds of different
-- actors, large values, etc., and so is a bit difficult to debug when
-- looking at values manually.
cardanoModel :: TxSizeLinear
             -> Int    -- ^ "our" actor, the owner of all "our" addresses
             -> [Addr] -- ^ list of all addresses (including "ours")
             -> Transaction GivenHash Addr
             -> GeneratorModel GivenHash Addr
cardanoModel linearFeePolicy ourActor allAddrs boot =
    GeneratorModel {
      gmBoot          = boot
    , gmAllAddresses  = allAddrs
    , gmOurs          = Set.fromList ourAddrs
    , gmEstimateFee   = estimateCardanoFee linearFeePolicy
    }
    where
        ours (Addr (IxPoor actor) _) = (ourActor == actor)
        ours _                       = False

        ourAddrs = filter ours allAddrs
