module Pos.Modern.Txp.Storage.UtxoView
       (
         UtxoView (..)
       , getTxOut
       , putTxOut
       , delTxIn
       , createFromDB
       ) where

import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Universum

import           Pos.Modern.DB.Types (DB)
import           Pos.Modern.DB.Utxo  (getTxOutFromDB)
import           Pos.Types           (TxIn (..), TxOut)

data UtxoView ssc = UtxoView
    {
      addUtxo :: !(HashMap TxIn TxOut)
    , delUtxo :: !(HashSet TxIn)
    , utxoDB  :: !(DB ssc)
    }

createFromDB :: DB ssc -> UtxoView ssc
createFromDB = UtxoView HM.empty HS.empty

putTxOut :: TxIn -> TxOut -> UtxoView ssc -> UtxoView ssc
putTxOut key val mp@UtxoView{..} = -- what we should do if key contains into delUtxo?
    mp {addUtxo = HM.insert key val addUtxo}

delTxIn :: TxIn -> UtxoView ssc -> UtxoView ssc
delTxIn txIn mp@UtxoView{..} =
    mp {delUtxo = HS.insert txIn delUtxo}

getTxOut :: MonadIO m => TxIn -> UtxoView ssc -> m (Maybe TxOut)
getTxOut key UtxoView{..}
    | HS.member key delUtxo = return Nothing
    | otherwise             = maybe (getTxOutFromDB key utxoDB)
                                    (return . Just)
                                    (HM.lookup key addUtxo)
