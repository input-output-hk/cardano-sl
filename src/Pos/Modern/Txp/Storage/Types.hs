module Pos.Modern.Txp.Storage.Types
       (
         UtxoView (..)
       , MemPool (..)
       , TxMap
       ) where

import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import           Universum

import           Pos.DB.Types        (DB)
import           Pos.Types           (Tx, TxId, TxIn, TxOut, TxWitness)


data UtxoView ssc = UtxoView
    {
      addUtxo :: !(HashMap TxIn TxOut)
    , delUtxo :: !(HashSet TxIn)
    , utxoDB  :: !(DB ssc)
    }

type TxMap = HashMap TxId (Tx, TxWitness)

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
