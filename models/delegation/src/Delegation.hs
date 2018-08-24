module Delegation where

import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Monoid    (Monoid)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Set       (Set)
import qualified Data.Set       as Set


newtype TxId = TxId Tx deriving (Show, Eq, Ord)
newtype Addr = Addr Int deriving (Show, Eq, Ord)
newtype Coin = Coin Int deriving (Show, Eq, Ord)
data TxIn = TxIn TxId Int deriving (Show, Eq, Ord)
data TxOut = TxOut Addr Coin deriving (Show, Eq, Ord)
data Tx = Tx { inputs :: Set TxIn, outputs :: [TxOut] } deriving (Show, Eq, Ord)
newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq, Ord)

-- TODO is it okay that I've used list indices instead of implementing the Ix Type?

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty = Coin 0
  mappend = (<>)

txid :: Tx -> TxId
txid = TxId

txins :: Tx -> Set TxIn
txins = inputs

txouts :: Tx -> UTxO
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0..]]
  where
    transId = txid tx

unionUTxO :: UTxO -> UTxO -> UTxO
unionUTxO (UTxO a) (UTxO b) = UTxO $ Map.union a b

balance :: UTxO -> Coin
balance (UTxO utxo) = foldr addCoins mempty utxo
  where addCoins (TxOut _ a) b = a <> b

newtype Owner = Owner Int deriving (Show, Eq, Ord)
newtype SKey = SKey Owner deriving (Show, Eq, Ord)
newtype VKey = VKey Owner deriving (Show, Eq, Ord)
newtype KeyHash = KeyHash VKey deriving (Show, Eq, Ord)
data Signature a = Signature a Owner deriving (Show, Eq)

keyHash :: VKey -> KeyHash
keyHash = KeyHash

sign :: SKey -> a -> Signature a
sign (SKey k) d = Signature d k

verify :: Eq a => VKey -> a -> Signature a -> Bool
verify (VKey vk) vd (Signature sd sk) = vk == sk && vd == sd

type Ledger = [Tx]

-- |Domain restriction TODO: better symbol?
(<|) :: Set TxIn -> UTxO -> UTxO
ins <| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion TODO: better symbol?
(!<|) :: Set TxIn -> UTxO -> UTxO
ins !<| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo

data ValidationError = UnknownInputs | IncreasedTotalBalance deriving (Show, Eq)
data Validity = Valid | Invalid [ValidationError] deriving (Show, Eq)

instance Semigroup Validity where
  Valid <> b                 = b
  a <> Valid                 = a
  (Invalid a) <> (Invalid b) = Invalid (a ++ b)

instance Monoid Validity where
  mempty = Valid
  mappend = (<>)

validInputs :: Tx -> UTxO -> Validity
validInputs (Tx inputs _) (UTxO utox) =
  if inputs `Set.isSubsetOf` Map.keysSet utox
    then Valid
    else Invalid [UnknownInputs]

preserveBalance :: Tx -> UTxO -> Validity
preserveBalance tx utxo =
  if balance (txouts tx) <= balance (txins tx <| utxo)
    then Valid
    else Invalid [IncreasedTotalBalance]

valid :: Tx -> UTxO -> Validity
valid tx utxo =
  validInputs tx utxo <> preserveBalance tx utxo

transactionTransition :: Tx -> UTxO -> Either [ValidationError] UTxO
transactionTransition tx utxo =
  case valid tx utxo of
    Valid          -> Right (txins tx !<| utxo `unionUTxO` txouts tx)
    Invalid errors -> Left errors
