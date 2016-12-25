module Pos.Txp.Types.UtxoView
       (
         getTxOut
       , putTxOut
       , delTxIn
       , createFromDB
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Universum

import           Pos.DB.Types        (DB)
import           Pos.DB.Utxo         (getTxOutFromDB)
import           Pos.Txp.Types.Types (UtxoView (..))
import           Pos.Types           (TxIn (..), TxOutAux)

createFromDB :: DB ssc -> UtxoView ssc
createFromDB = UtxoView HM.empty HS.empty

putTxOut :: TxIn -> TxOutAux -> UtxoView ssc -> UtxoView ssc
putTxOut key val mp@UtxoView{..} =
    mp { addUtxo = HM.insert key val addUtxo
       , delUtxo = HS.delete key delUtxo}

delTxIn :: TxIn -> UtxoView ssc -> UtxoView ssc
delTxIn txIn mp@UtxoView{..} =
    mp {delUtxo = HS.insert txIn delUtxo}

getTxOut
    :: (MonadIO m, MonadThrow m)
    => TxIn -> UtxoView ssc -> m (Maybe TxOutAux)
getTxOut key UtxoView{..}
    | HS.member key delUtxo = return Nothing
    | otherwise             = maybe (getTxOutFromDB key utxoDB)
                                    (return . Just)
                                    (HM.lookup key addUtxo)
