{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Utxo related operations.

module Pos.Modern.Types.Utxo
       ( applyTxToUtxo
       , deleteTxIn
       , findTxIn
       , verifyTxUtxo
       , verifyTxs
       , applyTxToUtxo'
       , convertTo'
       , convertFrom'
       ) where

import           Control.Lens             (over, _1)
import           Control.Monad.IfElse     (aifM)
import qualified Data.HashSet             as HS
import qualified Data.Map.Strict          as M
import           Database.RocksDB         (BatchOp (..))
import           Serokell.Util            (VerificationRes (..))
import           Universum

import           Pos.Crypto               (WithHash (..))
import           Pos.Modern.State.Storage (MonadDB (..), rocksGet)
import           Pos.Modern.Txp.RocksDB   (createDelTx, createPutTx)
import           Pos.Types.Tx             (topsortTxs, verifyTx)
import           Pos.Types.Types          (IdTxWitness, Tx (..), TxIn (..), TxOut (..),
                                           TxWitness, Utxo)
-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOut
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- | Verify single Tx using Utxo as TxIn resolver.
verifyTxUtxo :: Utxo -> (Tx, TxWitness) -> VerificationRes
verifyTxUtxo utxo txw = verifyTx (`findTxIn` utxo) txw

-- | Takes the set of transactions and utxo, returns only those
-- transactions that can be applied inside. Bonus -- returns them
-- sorted (topographically).
-- normalizeTxs :: [(WithHash Tx, TxWitness)] -> Utxo -> [(WithHash Tx, TxWitness)]
-- normalizeTxs = normGo []
--   where
--     -- checks if transaction can be applied, adds it to first arg and
--     -- to utxo if ok, does nothing otherwise
--     canApply :: (WithHash Tx, TxWitness)
--              -> ([(WithHash Tx, TxWitness)], Utxo)
--              -> ([(WithHash Tx, TxWitness)], Utxo)
--     canApply txw prev@(txws, utxo) =
--         case verifyTxUtxo utxo (over _1 whData txw) of
--             VerFailure _ -> prev
--             VerSuccess   -> (txw : txws, fst txw `applyTxToUtxo` utxo)

--     normGo :: [(WithHash Tx, TxWitness)]
--            -> [(WithHash Tx, TxWitness)]
--            -> Utxo
--            -> [(WithHash Tx, TxWitness)]
--     normGo result pending curUtxo =
--         let !(!canBeApplied, !newUtxo) = foldr' canApply ([], curUtxo) pending
--             newPending = pending \\ canBeApplied
--             newResult = result ++ canBeApplied
--         in if null canBeApplied
--                then result
--                else normGo newResult newPending newUtxo

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
verifyTxs
    :: MonadDB ssc m
    => [(WithHash Tx, TxWitness)]
    -> Utxo
    -> m (Either Text ([(WithHash Tx, TxWitness)], [BatchOp], Utxo)) -- should I make StateT or smth else?
verifyTxs txws initValidationCache = do
    let
        -- head is the last one
        -- to check
        topsorted = reverse <$> topsortTxs fst txws
        -- Get all TxIn which are sources, e.g. indegree equal 1
        -- This is union (Tx inputs) - union (Tx outputs)
        -- Next, We iterate by this set and get keys from RocksDB
        -- which doesn't exist in current local utxo (initValidationCache)
        initializateUtxo :: MonadDB ssc m => [(WithHash Tx, TxWitness)] -> m Utxo
        initializateUtxo xs = do
            let txs = map (\x -> (whData . fst $ x, whHash . fst $ x)) xs
                (inUnion, outUnion) =
                    foldl' (\(txInUnion, txOutUnion) (Tx{..}, txId) ->
                        ( foldl' (\accInputs TxIn{..} ->
                                    HS.insert (txInHash, txInIndex) accInputs)
                                  txInUnion
                                  txInputs
                        , foldl' (\accOutputs idx ->
                                    HS.insert (txId, fromIntegral idx) accOutputs
                                  )
                                  txOutUnion
                                  [0..length txOutputs - 1]
                        ))
                        (HS.empty, HS.empty)
                        txs

            foldM (\utxo hash'id ->
                      if (not $ M.member hash'id utxo) then
                          aifM (getUtxoDB >>= rocksGet hash'id)
                              (\txOut -> return $ M.insert hash'id txOut utxo)
                              (return utxo)
                      else
                          return utxo
                  )
                  initValidationCache
                  (inUnion `HS.difference` outUnion)

    initUtxo <- maybe (return M.empty) initializateUtxo topsorted -- refactor here
    let applyAll :: [(WithHash Tx, TxWitness)] -> Either Text Utxo
        applyAll [] = Right $ initUtxo
        applyAll (txw:xs) = do
            curUtxo <- applyAll xs
            case verifyTxUtxo curUtxo (over _1 whData txw) of
                VerSuccess        -> pure $ fst txw `applyTxToUtxo` curUtxo
                VerFailure reason ->
                    Left $
                        fromMaybe "Transaction application failed, reason not specified" $
                            head reason
    return $
        maybe
            (Left "Topsort on transactions failed -- topology is broken")
            (\txs' -> do
                utxoAfterApply <- applyAll txs'
                let delInp = map createDelTx (M.keys initUtxo)
                    putOut = map createPutTx (M.toList utxoAfterApply)
                Right (txs', delInp ++ putOut, utxoAfterApply)
            )
            topsorted

convertTo' :: [IdTxWitness] -> [(WithHash Tx, TxWitness)]
convertTo' = map (\(i, (t, w)) -> (WithHash t i, w))

convertFrom' :: [(WithHash Tx, TxWitness)] -> [IdTxWitness]
convertFrom' = map (\(WithHash t h, w) -> (h, (t, w)))

applyTxToUtxo' :: IdTxWitness -> Utxo -> Utxo
applyTxToUtxo' (i, (t, _)) = applyTxToUtxo $ WithHash t i
