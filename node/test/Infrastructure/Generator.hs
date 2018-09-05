{-# LANGUAGE NamedFieldPuns #-}
module Infrastructure.Generator
  ( GeneratorModel
  , genChainUsingModel
  , simpleModel
  , simpleGen
  ) where

import           Data.Functor.Identity (runIdentity)
import qualified Data.Set as Set
import           Test.QuickCheck (Gen)
import           Universum

import           UTxO.DSL (GivenHash, Hash, Output (Output),
                     Transaction (Transaction), Value, trExtra, trFee, trFresh,
                     trHash, trIns, trOuts, trUtxo, utxoRestrictToAddr)
import           UTxO.Generator (defChainParams, genChain, initTrState)

import           Chain.Abstract (Addr, Chain)
import           Chain.Abstract.Translate.FromUTxO (ChainValidity, IntException,
                     translate)
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
    params = undefined
