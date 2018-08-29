{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module Delegation where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable         (all)
import           Data.List             (find)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (isJust)
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
data Cert = RegKey | DeRegKey | Delegate | RegPool | RetirePool
  deriving (Show, Eq, Ord)

newtype Owner = Owner Int deriving (Show, Eq, Ord)
newtype SKey = SKey Owner deriving (Show, Eq, Ord)
newtype VKey = VKey Owner deriving (Show, Eq, Ord)
data Sig a = Sig a Owner deriving (Show, Eq, Ord)
data WitTxin = WitTxin VKey (Sig TxBody) deriving (Show, Eq, Ord)
data WitCert = WitCert VKey (Sig TxBody) deriving (Show, Eq, Ord)
data Wits = Wits { txinWits :: (Set WitTxin)
                 , certWits :: (Set WitCert)
                 } deriving (Show, Eq, Ord)

data TxIn = TxIn TxId Int deriving (Show, Eq, Ord)
data TxOut = TxOut Addr Coin deriving (Show, Eq, Ord)
data TxBody = TxBody { inputs  :: Set TxIn
                     , outputs :: [TxOut]
                     , certs   :: Set Cert
                     } deriving (Show, Eq, Ord)
data Tx = Tx { body :: TxBody
             , wits :: Wits
             } deriving (Show, Eq)

newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq, Ord)

-- TODO is it okay that I've used list indices instead of implementing the Ix Type?

instance BA.ByteArrayAccess Tx where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance BA.ByteArrayAccess SKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

-- TODO how do I remove these boilerplate instances?

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty = Coin 0
  mappend = (<>)

txid :: Tx -> TxId
txid = TxId . hash

txins :: Tx -> Set TxIn
txins = inputs . body

txouts :: Tx -> UTxO
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (outs tx) [0..]]
  where
    outs = outputs . body
    transId = txid tx

unionUTxO :: UTxO -> UTxO -> UTxO
unionUTxO (UTxO a) (UTxO b) = UTxO $ Map.union a b

balance :: UTxO -> Coin
balance (UTxO utxo) = foldr addCoins mempty utxo
  where addCoins (TxOut _ a) b = a <> b

sign :: SKey -> a -> Sig a
sign (SKey k) d = Sig d k

verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

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
validInputs tx (UTxO utox) =
  if (txins tx) `Set.isSubsetOf` Map.keysSet utox
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

authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) =
  case Map.lookup txin utxo of
    Just (TxOut (AddrTxin pay _) _) -> hash key == pay
    _ -> False

witnessTxin :: Tx -> UTxO -> Bool
witnessTxin (Tx txBody (Wits ws _)) utxo =
  (Set.size ws) == (Set.size ins) && all (hasWitness ws) ins
  where
    ins = inputs txBody
    hasWitness ws input = isJust $ find (isWitness txBody input utxo) ws
    isWitness tb inp utxo (WitTxin key sig) =
      verify key tb sig && authTxin key inp utxo

authCert :: VKey -> Cert -> Bool
authCert key cert = True -- TODO

witnessCert :: Tx -> Bool
witnessCert (Tx txBody (Wits _ ws)) =
  (Set.size ws) == (Set.size cs) && all (hasWitness ws) cs
  where
    cs = certs txBody
    hasWitness ws cert = isJust $ find (isWitness txBody cert) ws
    isWitness txBody cert (WitCert key sig) =
      verify key txBody sig && authCert key cert
--TODO combine with witnessTxin?

witness :: Tx -> UTxO -> Bool
witness tx utxo = witnessTxin tx utxo && witnessCert tx
