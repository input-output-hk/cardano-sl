{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
-- | Utxo related operations.

module Pos.Types.Utxo
       ( applyTxToUtxo
       , deleteTxIn
       , findTxIn
       , verifyTxUtxo
       , verifyAndApplyTxs
       , normalizeTxs
       , normalizeTxs'
       , applyTxToUtxo'
       , verifyAndApplyTxs'
       , convertTo'
       , convertFrom'
       ) where

import           Control.Lens    (over, _1)
import           Data.List       ((\\))
import qualified Data.Map.Strict as M
import           Serokell.Util   (VerificationRes (..))
import           Universum

import           Pos.Crypto      (WithHash (..))
import           Pos.Types.Tx    (topsortTxs, verifyTx)
import           Pos.Types.Types (IdTxWitness, Tx (..), TxIn (..), TxOut (..), TxWitness,
                                  Utxo)

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOut
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- | Verify single Tx using Utxo as TxIn resolver.
verifyTxUtxo :: Utxo -> (Tx, TxWitness) -> VerificationRes
verifyTxUtxo utxo txw = verifyTx (`findTxIn` utxo) txw

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: WithHash Tx -> Utxo -> Utxo
applyTxToUtxo tx =
    foldl' (.) identity
        (map applyInput txInputs ++ zipWith applyOutput [0..] txOutputs)
  where
    Tx {..} = whData tx
    applyInput txIn = deleteTxIn txIn
    applyOutput idx txOut = M.insert (whHash tx, idx) txOut

-- | Accepts list of transactions and verifies its overall properties
-- plus validity of every transaction in particular. Return value is
-- verification failure (first) or topsorted list of transactions (if
-- topsort succeeded -- no loops were found) plus new
-- utxo. @VerificationRes@ is not used here because it can't be
-- applied -- no more than one error can happen. Either transactions
-- can't be topsorted at all or the first incorrect transaction is
-- encountered so we can't proceed further.
verifyAndApplyTxs
    :: [(WithHash Tx, TxWitness)]
    -> Utxo
    -> Either Text ([(WithHash Tx, TxWitness)], Utxo)
verifyAndApplyTxs txws utxo =
    maybe
        (Left "Topsort on transactions failed -- topology is broken")
        (\txs' -> (txs',) <$> applyAll txs')
        topsorted
  where
    applyAll :: [(WithHash Tx, TxWitness)] -> Either Text Utxo
    applyAll [] = Right utxo
    applyAll (txw:xs) = do
        curUtxo <- applyAll xs
        case verifyTxUtxo curUtxo (over _1 whData txw) of
            VerSuccess        -> pure $ fst txw `applyTxToUtxo` curUtxo
            VerFailure reason ->
                Left $ fromMaybe "Transaction application failed, reason not specified" $
                head reason
    topsorted = reverse <$> topsortTxs fst txws -- head is the last one
                                                -- to check

-- | Takes the set of transactions and utxo, returns only those
-- transactions that can be applied inside. Bonus -- returns them
-- sorted (topographically).
normalizeTxs :: [(WithHash Tx, TxWitness)] -> Utxo -> [(WithHash Tx, TxWitness)]
normalizeTxs = normGo []
  where
    -- checks if transaction can be applied, adds it to first arg and
    -- to utxo if ok, does nothing otherwise
    canApply :: (WithHash Tx, TxWitness)
             -> ([(WithHash Tx, TxWitness)], Utxo)
             -> ([(WithHash Tx, TxWitness)], Utxo)
    canApply txw prev@(txws, utxo) =
        case verifyTxUtxo utxo (over _1 whData txw) of
            VerFailure _ -> prev
            VerSuccess   -> (txw : txws, fst txw `applyTxToUtxo` utxo)

    normGo :: [(WithHash Tx, TxWitness)]
           -> [(WithHash Tx, TxWitness)]
           -> Utxo
           -> [(WithHash Tx, TxWitness)]
    normGo result pending curUtxo =
        let !(!canBeApplied, !newUtxo) = foldr' canApply ([], curUtxo) pending
            newPending = pending \\ canBeApplied
            newResult = result ++ canBeApplied
        in if null canBeApplied
               then result
               else normGo newResult newPending newUtxo

-- TODO change types of normalizeTxs and related

convertTo' :: [IdTxWitness] -> [(WithHash Tx, TxWitness)]
convertTo' = map (\(i, (t, w)) -> (WithHash t i, w))

convertFrom' :: [(WithHash Tx, TxWitness)] -> [IdTxWitness]
convertFrom' = map (\(WithHash t h, w) -> (h, (t, w)))

normalizeTxs' :: [IdTxWitness] -> Utxo -> [IdTxWitness]
normalizeTxs' tx utxo =
    let converted = convertTo' tx in
    convertFrom' $ normalizeTxs converted utxo

applyTxToUtxo' :: IdTxWitness -> Utxo -> Utxo
applyTxToUtxo' (i, (t, _)) = applyTxToUtxo $ WithHash t i

verifyAndApplyTxs'
    :: [IdTxWitness]
    -> Utxo
    -> Either Text ([IdTxWitness], Utxo)
verifyAndApplyTxs' txws utxo = (\(x, y) -> (convertFrom' x, y))
                               <$> verifyAndApplyTxs (convertTo' txws) utxo
