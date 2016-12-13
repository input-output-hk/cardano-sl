module Pos.Modern.Txp.Storage.MemPool
       (
         MemPool (..)
       , getTxOut
       , putTxOut
       , delTxIn
       ) where

import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import qualified Data.Map            as M
import           Universum

import           Pos.Modern.DB.Types (DB)
import           Pos.Modern.DB.Utxo  (getTxOutFromDB)
import           Pos.Types           (TxId, TxIn (..), TxOut, Utxo)

data MemPool ssc = MemPool
    {
      addUtxo :: !Utxo
    , delUtxo :: !(HashSet TxIn)
    , utxoDB  :: !(DB ssc)
    }

putTxOut :: TxIn -> TxOut -> MemPool ssc -> MemPool ssc
putTxOut TxIn{..} val mp@MemPool{..} = -- what we should do if key contains into delUtxo?
    mp {addUtxo = M.insert (txInHash, txInIndex) val addUtxo}

delTxIn :: TxIn -> MemPool ssc -> MemPool ssc
delTxIn txIn mp@MemPool{..} =
    mp {delUtxo = HS.insert txIn delUtxo}

getTxOut :: MonadIO m => TxIn -> MemPool ssc -> m (Maybe TxOut)
getTxOut key@TxIn{..} MemPool{..}
    | HS.member key delUtxo = return Nothing
    | otherwise             = maybe (getTxOutFromDB key utxoDB)
                                    (return . Just)
                                    (M.lookup (txInHash, txInIndex) addUtxo)

getMeme :: MemPool ssc -> Text
getMeme _ = "Misha is vegan"
