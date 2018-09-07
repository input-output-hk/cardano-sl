{-|
Module      : UTxO
Description : basic UTxO module
Stability   : experimental

This is a basic UTxO model
-}

{-# LANGUAGE PackageImports    #-}

module Delegation.UTxO
  ( TxId(..)
  , Addr(..)
  , Coin(..)
  , Owner(..)
  , TxIn(..)
  , TxOut(..)
  , UTxO(..)
  , balance
  , (<|)
  , (!<|)
  , union
  ) where


import           Crypto.Hash           (Digest, SHA256)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid           (Monoid)
import           Data.Semigroup        (Semigroup, (<>))
import           Data.Set              (Set)
import qualified Data.Set              as Set


newtype TxId = TxId { getTxId :: Digest SHA256 }
  deriving (Show, Eq, Ord)

data Addr = AddrTxin (Digest SHA256) (Digest SHA256)
          | AddrAccount (Digest SHA256) (Digest SHA256)
          deriving (Show, Eq, Ord)

newtype Coin = Coin Int deriving (Show, Eq, Ord)

newtype Owner = Owner Int deriving (Show, Eq, Ord)

data TxIn = TxIn TxId Int deriving (Show, Eq, Ord)

data TxOut = TxOut Addr Coin deriving (Show, Eq, Ord)

newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq, Ord)

-- TODO is it okay that I've used list indices instead of implementing the Ix Type?

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty = Coin 0
  mappend = (<>)

union :: UTxO -> UTxO -> UTxO
union (UTxO a) (UTxO b) = UTxO $ Map.union a b

balance :: UTxO -> Coin
balance (UTxO utxo) = foldr addCoins mempty utxo
  where addCoins (TxOut _ a) b = a <> b

-- |Domain restriction TODO: better symbol?
(<|) :: Set TxIn -> UTxO -> UTxO
ins <| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion TODO: better symbol?
(!<|) :: Set TxIn -> UTxO -> UTxO
ins !<| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo
