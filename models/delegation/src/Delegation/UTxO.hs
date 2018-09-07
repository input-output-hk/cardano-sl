module Delegation.UTxO
  ( TxId(..)
  , Addr(..)
  , Coin(..)
  , TxIn(..)
  , TxOut(..)
  , UTxO(..)
  , balance
  , (<|)
  , (!<|)
  , union
  ) where


import           Crypto.Hash    (Digest, SHA256)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Monoid    (Monoid)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Set       (Set)
import qualified Data.Set       as Set

-- |The id of a transaction.
newtype TxId = TxId { getTxId :: Digest SHA256 }
  deriving (Show, Eq, Ord)

-- |An address for UTxO.  It can be either an account based
-- address for rewards sharing or a UTxO address.
data Addr = AddrTxin (Digest SHA256) (Digest SHA256)
          | AddrAccount (Digest SHA256) (Digest SHA256)
          deriving (Show, Eq, Ord)

-- |The amount of value held by a UTxO.
newtype Coin = Coin Int deriving (Show, Eq, Ord)

-- |The input of a UTxO.
data TxIn = TxIn TxId Int deriving (Show, Eq, Ord)

-- |The output of a UTxO.
data TxOut = TxOut Addr Coin deriving (Show, Eq, Ord)

-- |The unspent transaction outputs.
newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq, Ord)

-- TODO is it okay that I've used list indices instead of implementing the Ix Type?

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty = Coin 0
  mappend = (<>)

-- |Combine two collections of UTxO.
-- __TODO__ how best to handle repeated keys?
-- (Our current use should always be disjoint.)
union :: UTxO -> UTxO -> UTxO
union (UTxO a) (UTxO b) = UTxO $ Map.union a b

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Coin
balance (UTxO utxo) = foldr addCoins mempty utxo
  where addCoins (TxOut _ a) b = a <> b

-- |Domain restriction. __TODO__: better symbol?
(<|) :: Set TxIn -> UTxO -> UTxO
ins <| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion. __TODO__: better symbol?
(!<|) :: Set TxIn -> UTxO -> UTxO
ins !<| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo
