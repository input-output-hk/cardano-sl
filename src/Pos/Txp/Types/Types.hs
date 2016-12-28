module Pos.Txp.Types.Types
       (
         UtxoView (..)
       , MemPool (..)
       , TxMap
       , ProcessTxRes (..)
       , mkPTRinvalid
       ) where

import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.Text           as T
import           Universum

import           Pos.DB.Types        (DB)
import           Pos.Types           (TxAux, TxId, TxIn, TxOutAux)


data UtxoView ssc = UtxoView
    {
      addUtxo :: !(HashMap TxIn TxOutAux)
    , delUtxo :: !(HashSet TxIn)
    , utxoDB  :: !(DB ssc)
    }

type TxMap = HashMap TxId TxAux

data MemPool = MemPool
    {
      localTxs     :: !TxMap
    , -- | @length@ is @O(n)@ for 'HM.HashMap' so we store it explicitly.
      localTxsSize :: !Int
    }

instance Default MemPool where
    def = MemPool
        {
          localTxs = HM.empty
        , localTxsSize = 0
        }

-- | Result of transaction processing
data ProcessTxRes
    = PTRadded -- ^ Transaction has ben successfully added to the storage
    | PTRknown -- ^ Transaction is already in the storage (cache)
    | PTRinvalid !Text -- ^ Can't add transaction
    | PTRoverwhelmed -- ^ Local transaction storage is full -- can't accept more txs
    deriving (Show, Eq)

-- | Make `ProcessTxRes` from list of error messages using
-- `PTRinvalid` constructor. Intended to be used with `VerificationRes`.
-- Note: this version forces computation of all error messages. It can be
-- made more efficient but less informative by using head, for example.
mkPTRinvalid :: [Text] -> ProcessTxRes
mkPTRinvalid = PTRinvalid . T.intercalate "; "
