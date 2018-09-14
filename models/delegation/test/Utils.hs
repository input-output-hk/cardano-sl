{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils where

import           Crypto.Hash     (hash)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import           Test.QuickCheck

import           Delegation

instance Arbitrary Coin where
  arbitrary = do
    Positive x <- arbitrary :: Gen (Positive Int)
    return $ Coin x

instance Arbitrary TxIn where
  arbitrary = do
    x <- arbitrary :: Gen String
    Positive i <- arbitrary :: Gen (Positive Int)
    return $ TxIn (TxId (hash x)) i

-- TODO separate account addresses?
instance Arbitrary Addr where
  arbitrary = do
    pay <- arbitrary :: Gen String
    stake <- arbitrary :: Gen String
    return $ AddrTxin (hash pay) (hash stake)

instance Arbitrary TxOut where
  arbitrary = do
    addr <- arbitrary
    coin <- arbitrary
    return $ TxOut addr coin

instance Arbitrary UTxO where
  arbitrary = UTxO <$> arbitrary

instance Arbitrary Owner where
  arbitrary = Owner <$> arbitrary

instance Arbitrary VKey where
  arbitrary = VKey <$> arbitrary

instance Arbitrary KeyPair where
  arbitrary = keyPair <$> arbitrary

instance Arbitrary HashKey where
  arbitrary = hashKey <$> arbitrary

genRetiring :: Gen (Map HashKey Int)
genRetiring = do
  retiring <- arbitrary :: Gen (Map HashKey (Positive Int))
  return $ Map.map getPositive retiring

instance Arbitrary LedgerState where
  arbitrary = do
    utxo <- arbitrary
    accounts <- arbitrary :: Gen (Map HashKey Coin)
    stKeys <- arbitrary :: Gen (Set HashKey)
    delegations <- arbitrary :: Gen (Map HashKey HashKey)
    stPools     <- arbitrary :: Gen (Set HashKey)
    retiring    <- genRetiring
    Positive epoch <- arbitrary :: Gen (Positive Int)
    return $ LedgerState utxo accounts stKeys delegations stPools retiring epoch

data NonTrivialState =
  NonTrivialState
  { getLS       :: LedgerState
  , getSkHolder :: KeyPair
  , getPool     :: KeyPair
  } deriving (Show, Eq)

addUTxO :: UTxO -> TxIn -> TxOut -> UTxO
addUTxO (UTxO utxo) txin txout = UTxO (Map.insert txin txout utxo)

genNonTrivialLS :: Gen NonTrivialState
genNonTrivialLS = do
  ls <- arbitrary :: Gen LedgerState
  skHold <- arbitrary
  (txinS, txoutS) <- arbitrary :: Gen ((TxIn, TxOut))
  pool <- arbitrary
  let
    ls' = ls {getUtxo = (addUTxO (getUtxo ls) txinS txoutS)}
  return $ NonTrivialState ls' skHold pool
  -- TODO register stake holder key, pool key, and pool cert


