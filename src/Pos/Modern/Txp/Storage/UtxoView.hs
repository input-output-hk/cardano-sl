module Pos.Modern.Txp.Storage.UtxoView
       (
         UtxoView (..)
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
import           Pos.Types           (TxIn (..), TxOut, Utxo)

data UtxoView ssc = UtxoView
    {
      addUtxo :: !Utxo
    , delUtxo :: !(HashSet TxIn)
    , utxoDB  :: !(DB ssc)
    }

putTxOut :: TxIn -> TxOut -> UtxoView ssc -> UtxoView ssc
putTxOut TxIn{..} val mp@UtxoView{..} = -- what we should do if key contains into delUtxo?
    mp {addUtxo = M.insert (txInHash, txInIndex) val addUtxo}

delTxIn :: TxIn -> UtxoView ssc -> UtxoView ssc
delTxIn txIn mp@UtxoView{..} =
    mp {delUtxo = HS.insert txIn delUtxo}

getTxOut :: MonadIO m => TxIn -> UtxoView ssc -> m (Maybe TxOut)
getTxOut key@TxIn{..} UtxoView{..}
    | HS.member key delUtxo = return Nothing
    | otherwise             = maybe (getTxOutFromDB key utxoDB)
                                    (return . Just)
                                    (M.lookup (txInHash, txInIndex) addUtxo)
