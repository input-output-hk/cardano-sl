module Pos.Modern.Txp.Storage.MemPool
       (
         MemPool (..)
       , getTxOut
       , putTxOut
       , delTxIn
       ) where

import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HS
import qualified Data.Map             as M
import           Universum

import           Pos.Modern.DB        (rocksGet)
import           Pos.Modern.DB.Types  (DB)
import           Pos.Types            (TxId, TxIn (..), TxOut, Utxo)

type Key = (TxId, Word32)

data MemPool ssc = MemPool
    {
      addUtxo :: !Utxo
    , delUtxo :: !(HashSet Key)
    , utxoDB  :: !(DB ssc)
    }

putTxOut :: (TxId, Word32) -> TxOut -> MemPool ssc -> MemPool ssc
putTxOut key val mp@MemPool{..} = -- what we should do if key contains into delUtxo?
    mp {addUtxo = M.insert key val addUtxo}

delTxIn :: TxIn -> MemPool ssc -> MemPool ssc
delTxIn TxIn{..} mp@MemPool{..} =
    mp {delUtxo = HS.delete (txInHash, txInIndex) delUtxo}

getTxOut :: MonadIO m => Key -> MemPool ssc -> m (Maybe TxOut)
getTxOut key MemPool{..}
    | HS.member key delUtxo = return Nothing
    | otherwise = maybe (rocksGet key utxoDB) (return . Just) (M.lookup key addUtxo)

getMeme :: MemPool ssc -> Text
getMeme _ = "Misha is vegan"
