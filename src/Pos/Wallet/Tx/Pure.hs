-- | Pure functions for operations with transactions

module Pos.Wallet.Tx.Pure
       ( makePubKeyTx
       , createTx
       , deriveAddrHistory
       , deriveAddrHistoryPartial
       ) where

import           Control.Lens        (folded, over, to, use, uses, view, (%=), (%=), (-=),
                                      (.~), (^.), (^..), _1, _2, _3)
import           Control.Monad       (fail, filterM)
import           Control.Monad.Loops (anyM)
import           Control.Monad.State (StateT (..), evalStateT)
import qualified Data.DList          as DL
import           Data.List           (tail)
import qualified Data.Map            as M
import qualified Data.Vector         as V
import           Universum

import           Pos.Binary          ()
import           Pos.Crypto          (SecretKey, WithHash (..), hash, sign, toPublic,
                                      withHash, _whData)
import           Pos.Types           (Address, Block, Coin, MainBlock, MonadUtxoRead (..),
                                      Tx (..), TxAux, TxDistribution (..), TxId,
                                      TxIn (..), TxInWitness (..), TxOut (..), TxOutAux,
                                      Utxo, UtxoStateT (..), applyTxToUtxo, blockTxas,
                                      filterUtxoByAddr, makePubKeyAddress, topsortTxs,
                                      _txOutputs)

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
    ourAddr = makePubKeyAddress $ toPublic sk
    allUnspent = M.toList $ filterUtxoByAddr ourAddr utxo
    sortedUnspent = sortBy (comparing $ Down . txOutValue . fst . snd) allUnspent
    inpOuts = do
        futxo <- evalStateT (pickInputs []) (totalMoney, sortedUnspent)
        let inputs = map fst futxo
            inputSum = sum $ map (txOutValue . fst . snd) futxo
            newOuts
                | inputSum > totalMoney =
                    (TxOut ourAddr (inputSum - totalMoney), []) : outputs
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
hasSender :: MonadUtxoRead m => Address -> Tx -> m Bool
hasSender addr Tx {..} = anyM hasCorrespondingOutput txInputs
  where hasCorrespondingOutput (TxIn h idx)  =
            fmap toBool $ fmap ((== addr) . txOutAddress . fst) <$> utxoGet (TxIn h idx)
        toBool Nothing  = False
        toBool (Just b) = b

-- | Checks if transaction is somehow related to address
relatedToAddress :: MonadUtxoRead m => Address -> Tx -> m Bool
relatedToAddress addr tx = if tx `hasReceiver` addr
                           then pure True
                           else hasSender addr tx

type TxSelector = UtxoStateT Maybe

-- | Select transactions related to given address from block
getRelatedTxs :: Address -> MainBlock ssc -> TxSelector [WithHash Tx]
getRelatedTxs addr blk = do
    txas <- lift $ topsortTxs (view _1) $
        (blk ^.. blockTxas . folded . to (over _1 withHash))
    fmap (map (view _1)) $ flip filterM txas $ \wtxa -> do
        let wtx = wtxa ^. _1
            tx = whData wtx
        -- filter outputs that go to 'addr'
        let outs = [(out, d)
                   | (out, d) <- zip (txOutputs tx)
                                     (getTxDistribution (wtxa ^. _3))
                   , txOutAddress out == addr ]
        applyTxToUtxo (wtx & _whData . _txOutputs .~ map fst outs)
                      (TxDistribution (map snd outs))
        relatedToAddress addr $ whData wtx

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory :: Address -> [Block ssc] -> TxSelector [WithHash Tx]
deriveAddrHistory addr chain = identity %= filterUtxoByAddr addr >>
                               deriveAddrHistoryPartial [] addr chain

deriveAddrHistoryPartial
    :: [WithHash Tx]
    -> Address
    -> [Block ssc]
    -> TxSelector [WithHash Tx]
deriveAddrHistoryPartial hist addr chain =
    DL.toList <$> foldrM updateAll (DL.fromList hist) chain
  where
    updateAll (Left _) hst = pure hst
    updateAll (Right blk) hst = do
        txs <- getRelatedTxs addr blk
        return $ DL.fromList txs <> hst
