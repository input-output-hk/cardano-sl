module Pos.Modern.Txp.Storage.UtxoView
       (
         getTxOut
       , putTxOut
       , delTxIn
       , createFromDB
       ) where

import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Universum

import           Pos.Modern.DB.Types          (DB)
import           Pos.Modern.DB.Utxo           (getTxOutFromDB)
import           Pos.Modern.Txp.Storage.Types (UtxoView (..))
import           Pos.Types                    (TxIn (..), TxOut)

createFromDB :: DB ssc -> UtxoView ssc
createFromDB = UtxoView HM.empty HS.empty

putTxOut :: TxIn -> TxOut -> UtxoView ssc -> UtxoView ssc
putTxOut key val mp@UtxoView{..} =
    mp { addUtxo = HM.insert key val addUtxo
       , delUtxo = HS.delete key delUtxo}

delTxIn :: TxIn -> UtxoView ssc -> UtxoView ssc
delTxIn txIn mp@UtxoView{..} =
    mp {delUtxo = HS.insert txIn delUtxo}

getTxOut
    :: (MonadIO m, MonadThrow m)
    => TxIn -> UtxoView ssc -> m (Maybe TxOut)
getTxOut key UtxoView{..}
    | HS.member key delUtxo = return Nothing
    | otherwise             = maybe (getTxOutFromDB key utxoDB)
                                    (return . Just)
                                    (HM.lookup key addUtxo)
