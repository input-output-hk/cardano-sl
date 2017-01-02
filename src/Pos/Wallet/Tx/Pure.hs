-- | Pure functions for operations with transactions

module Pos.Wallet.Tx.Pure
       ( makePubKeyTx
       , createTx
       , getRelatedTxs
       , deriveAddrHistory
       , deriveAddrHistoryPartial
       ) where

import           Control.Lens              (over, use, uses, view, (%=), (%=), (-=), (.~),
                                            (^.), _1, _2)
import           Control.Monad.State       (StateT (..), evalStateT)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.DList                as DL
import           Data.List                 (tail)
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Universum

import           Pos.Binary                ()
import           Pos.Crypto                (SecretKey, WithHash (..), hash, sign,
                                            toPublic, withHash)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Types                 (Address, Block, Coin, MonadUtxoRead (..),
                                            Tx (..), TxAux, TxDistribution (..), TxId,
                                            TxIn (..), TxInWitness (..), TxOut (..),
                                            TxOutAux, TxWitness, Utxo, UtxoStateT (..),
                                            applyTxToUtxo, blockTxas, filterUtxoByAddr,
                                            makePubKeyAddress, topsortTxs, _txOutputs)

type TxOutIdx = (TxId, Word32)
type TxInputs = [TxOutIdx]
type TxOutputs = [TxOutAux]
type TxError = Text

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SecretKey -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx sk inputs outputs = (Tx {..}, txWitness, txDist)
  where pk = toPublic sk
        txInputs = map makeTxIn inputs
        txOutputs = map fst outputs
        txAttributes = mkAttributes ()
        txOutHash = hash txOutputs
        txDist = TxDistribution (map snd outputs)
        txDistHash = hash txDist
        makeTxIn (txInHash, txInIndex) = TxIn {..}
        makeTxInWitness (txInHash, txInIndex) =
            PkWitness {
                twKey = pk,
                twSig = sign sk (txInHash, txInIndex, txOutHash, txDistHash) }
        txWitness = V.fromList (map makeTxInWitness inputs)

type FlatUtxo = [(TxOutIdx, TxOutAux)]
type InputPicker = StateT (Coin, FlatUtxo) (Either TxError)

-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SecretKey -> TxOutputs -> Either TxError TxAux
createTx utxo sk outputs = uncurry (makePubKeyTx sk) <$> inpOuts
  where
    totalMoney = sum $ map (txOutValue . fst) outputs
    ourId = makePubKeyAddress $ toPublic sk
    allUnspent = M.toList $ filterUtxoByAddr ourId utxo
    sortedUnspent = sortBy (comparing $ Down . txOutValue . fst . snd) allUnspent
    inpOuts = do
        futxo <- evalStateT (pickInputs []) (totalMoney, sortedUnspent)
        let inputs = map fst futxo
            inputSum = sum $ map (txOutValue . fst . snd) futxo
            newOuts
                | inputSum > totalMoney =
                    (TxOut ourId (inputSum - totalMoney), []) : outputs
                | otherwise = outputs
        pure (inputs, newOuts)
    pickInputs :: FlatUtxo -> InputPicker FlatUtxo
    pickInputs inps = do
        moneyLeft <- use _1
        if moneyLeft == 0
            then return inps
            else do
                mNextOut <- uses _2 head
                case mNextOut of
                    Nothing -> fail "Not enough money to send!"
                    Just inp@(_, (TxOut{..}, _)) -> do
                        _1 -= min txOutValue moneyLeft
                        _2 %= tail
                        pickInputs (inp : inps)

----------------------------------------------------------------------
-- Deduction of history
----------------------------------------------------------------------

-- | Check if given 'Address' is one of the receivers of 'Tx'
hasReceiver :: Tx -> Address -> Bool
hasReceiver Tx {..} addr = any ((== addr) . txOutAddress) txOutputs

-- | Given some 'Utxo', check if given 'Address' is one of the senders of 'Tx'
hasSender :: MonadUtxoRead m => Tx -> Address -> m Bool
hasSender Tx {..} addr = anyM hasCorrespondingOutput txInputs
  where hasCorrespondingOutput txIn =
            fmap toBool $ fmap ((== addr) . txOutAddress . fst) <$> utxoGet txIn
        toBool Nothing  = False
        toBool (Just b) = b

type TxSelectorT m = UtxoStateT (MaybeT m)

-- | Select transactions related to given address. `Bool` indicates
-- whether the transaction is outgoing (i. e. is sent from given address)
getRelatedTxs
    :: Monad m
    => Address
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> TxSelectorT m [(TxId, Tx, Bool)]
getRelatedTxs addr txs = lift (MaybeT $ return $ topsortTxs (view _1) txs) >>=
                         foldlM step DL.empty >>= return . DL.toList
  where
    step ls (WithHash tx txId, _wit, dist) = do
        let isIncoming = tx `hasReceiver` addr
        isOutgoing <- tx `hasSender` addr
        if isOutgoing || isIncoming
            then do
            -- Filter outputs that go to 'addr'
            let outputsToAddr = do
                    (out, d) <- zip (txOutputs tx) (getTxDistribution dist)
                    guard (txOutAddress out == addr)
                    return (out, d)
            applyTxToUtxo
                (WithHash (tx & _txOutputs .~ map fst outputsToAddr) txId)
                (TxDistribution (map snd outputsToAddr))
            return $ ls <> DL.singleton (txId, tx, isOutgoing)
            else return ls

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory :: Monad m => Address -> [Block ssc] -> TxSelectorT m [(TxId, Tx, Bool)]
deriveAddrHistory addr chain = identity %= filterUtxoByAddr addr >>
                               deriveAddrHistoryPartial [] addr chain

deriveAddrHistoryPartial
    :: Monad m
    => [(TxId, Tx, Bool)]
    -> Address
    -> [Block ssc]
    -> TxSelectorT m [(TxId, Tx, Bool)]
deriveAddrHistoryPartial hist addr chain =
    DL.toList <$> foldrM updateAll (DL.fromList hist) chain
  where
    updateAll (Left _) hst = pure hst
    updateAll (Right blk) hst = do
        txs <- getRelatedTxs addr $
                   map (over _1 withHash) (blk ^. blockTxas)
        return $ DL.fromList txs <> hst
