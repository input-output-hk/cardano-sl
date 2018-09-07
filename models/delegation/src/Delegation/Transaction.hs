module Delegation.Transaction
  ( WitTxin(..)
  , WitCert(..)
  , Wits(..)
  , TxBody(..)
  , Tx(..)
  , txid
  , txins
  , txouts
  , authTxin
  ) where

import           Crypto.Hash             (hash)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Char8   as BS
import qualified Data.Map                as Map
import           Data.Set                (Set)

import           Delegation.Certificates
import           Delegation.Keys
import           Delegation.UTxO

-- |The Witness for the input of a UTxO.
data WitTxin = WitTxin VKey (Sig TxBody) deriving (Show, Eq, Ord)

-- |The Witness for a certificate.
data WitCert = WitCert VKey (Sig TxBody) deriving (Show, Eq, Ord)

-- |The Witnesses for a transaction.
data Wits = Wits { txinWits :: (Set WitTxin)
                 , certWits :: (Set WitCert)
                 } deriving (Show, Eq, Ord)

-- |The body of a transaction.
data TxBody = TxBody { inputs  :: Set TxIn
                     , outputs :: [TxOut]
                     , certs   :: Set Cert
                     } deriving (Show, Eq, Ord)

-- |A transaction.
data Tx = Tx { body :: TxBody
             , wits :: Wits
             } deriving (Show, Eq)

instance BA.ByteArrayAccess Tx where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show

-- |Comptute the id of a transaction.
txid :: Tx -> TxId
txid = TxId . hash

-- |Comptute the UTxO inputs in a transaction.
txins :: Tx -> Set TxIn
txins = inputs . body

-- |Comptute the UTxO outputs in a transaction.
txouts :: Tx -> UTxO
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (outs tx) [0..]]
  where
    outs = outputs . body
    transId = txid tx

-- |Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) =
  case Map.lookup txin utxo of
    Just (TxOut (AddrTxin pay _) _) -> hash key == pay
    _                               -> False
