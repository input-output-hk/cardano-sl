module Infrastructure.Generator
  ( GeneratorModel
  , genChainUsingModel
  , simpleModel
  ) where

import qualified Data.Set as Set
import           Test.QuickCheck
import Universum
import UTxO.DSL
import UTxO.Generator

data GeneratorModel h a = GeneratorModel
  { gmBoot :: Transaction h a
  , gmAllAddresses :: [a]
  , gmEstimateFee :: Int -> [Value] -> Value
  }

genChainUsingModel :: (Hash h a, Ord a) => GeneratorModel h a -> Gen (Chain h a)
genChainUsingModel GeneratorModel{..} =
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
