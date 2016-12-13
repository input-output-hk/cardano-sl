{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
-- | Utxo related operations.

module Pos.Modern.Types.Utxo
       ( applyTxToUtxo
       , findTxIn
       , verifyTxUtxo
       , verifyTxs
       , convertTo'
       , convertFrom'
       ) where

import           Control.Lens                 (over, _1)
import           Control.Monad.IfElse         (aifM)
import qualified Data.HashSet                 as HS
import qualified Data.Map.Strict              as M
import           Database.RocksDB             (BatchOp (..))
import           Serokell.Util                (VerificationRes (..))
import           Universum

import           Pos.Crypto                   (WithHash (..))
import           Pos.Modern.DB                (MonadDB (..), getUtxoDB, rocksGet)
import           Pos.Modern.Txp.RocksDB       (createDelTx, createPutTx)
import           Pos.Modern.Txp.Storage.Types (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Modern.Types.Tx          (topsortTxs, verifyTx)
import           Pos.Types.Types              (IdTxWitness, Tx (..), TxId, TxIn (..),
                                               TxOut (..), TxWitness, Utxo)
-- | Accepts list of transactions and verifies its overall properties
-- plus validity of every transaction in particular. Return value is
-- verification failure (first) or topsorted list of transactions (if
-- topsort succeeded -- no loops were found) plus new
-- utxo. @VerificationRes@ is not used here because it can't be
-- applied -- no more than one error can happen. Either transactions
-- can't be topsorted at all or the first incorrect transaction is
-- encountered so we can't proceed further.
verifyTxs
    :: MonadUtxoRead ssc m
    => [(WithHash Tx, TxWitness)]
    -> m (Either Text [(WithHash Tx, TxWitness)])
verifyTxs txws  = do
    let
        -- head is the last one to check
        topsorted = reverse <$> topsortTxs fst txws

        applyAll [] = return VerSuccess
        applyAll (txw:xs) = do
            pure (<>)
            <*> (applyAll xs)
            <*> ( do
                  resCur <- verifyTxUtxo (over _1 whData txw)
                  return $
                      case resCur of
                          VerSuccess        -> VerSuccess
                          VerFailure reason -> notImplemented
                )
    maybe
        (return $ Left "Topsort on transactions failed -- topology is broken")
        (\txs' -> do
            res <- applyAll txs'
            return $
                case res of
                    VerSuccess        -> Right txs'
                    VerFailure reason -> Left "" -- $ head reason
        )
        topsorted

applyTxToUtxo :: MonadUtxo ssc m => WithHash Tx -> m ()
applyTxToUtxo tx = do
    mapM_ applyInput txInputs
    mapM_ (uncurry applyOutput) (zip [0..] txOutputs)
  where
    Tx {..} = whData tx
    applyInput = delTxIn
    applyOutput idx = putTxOut (whHash tx, idx)

convertTo' :: [IdTxWitness] -> [(WithHash Tx, TxWitness)]
convertTo' = map (\(i, (t, w)) -> (WithHash t i, w))

convertFrom' :: [(WithHash Tx, TxWitness)] -> [IdTxWitness]
convertFrom' = map (\(WithHash t h, w) -> (h, (t, w)))

-- applyTxToUtxo' :: IdTxWitness -> Utxo -> Utxo
-- applyTxToUtxo' (i, (t, _)) = applyTxToUtxo $ WithHash t i

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: MonadUtxoRead ssc m => TxIn -> m (Maybe TxOut)
findTxIn TxIn{..} = getTxOut (txInHash, txInIndex)

-- | Verify single Tx using Utxo as TxIn resolver.
verifyTxUtxo :: MonadUtxoRead ssc m => (Tx, TxWitness) -> m VerificationRes
verifyTxUtxo = verifyTx findTxIn

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
