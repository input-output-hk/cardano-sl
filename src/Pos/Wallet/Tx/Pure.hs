-- | Pure functions for operations with transactions

module Pos.Wallet.Tx.Pure
       ( makePubKeyTx
       , createTx
       , getRelatedTxs
       , deriveAddrHistory
       , deriveAddrHistoryPartial
       ) where

import           Control.Lens        (folded, to, use, uses, (%=), (%=), (%~), (-=),
                                      (^..), _1, _2)
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
                                      Tx (..), TxId, TxIn (..), TxInWitness (..),
                                      TxOut (..), TxWitness, Utxo, UtxoStateT (..),
                                      applyTxToUtxo, blockTxs, filterUtxoByAddr,
                                      makePubKeyAddress, topsortTxs, _txOutputs)

type TxOutIdx = (TxId, Word32)
type TxInputs = [TxOutIdx]
type TxOutputs = [TxOut]
type TxError = Text

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SecretKey -> TxInputs -> TxOutputs -> (Tx, TxWitness)
makePubKeyTx sk inputs txOutputs = (Tx {..}, txWitness)
  where pk = toPublic sk
        txInputs = map makeTxIn inputs
        txOutHash = hash txOutputs
        makeTxIn (txInHash, txInIndex) = TxIn {..}
        makeTxInWitness (txInHash, txInIndex) =
            PkWitness {
                twKey = pk,
                twSig = sign sk (txInHash, txInIndex, txOutHash) }
        txWitness = V.fromList (map makeTxInWitness inputs)

type FlatUtxo = [(TxOutIdx, TxOut)]
type InputPicker = StateT (Coin, FlatUtxo) (Either TxError)

-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SecretKey -> TxOutputs -> Either TxError (Tx, TxWitness)
createTx utxo sk outputs = uncurry (makePubKeyTx sk) <$> inpOuts
  where totalMoney = sum $ map txOutValue outputs
        ourAddr = makePubKeyAddress $ toPublic sk
        allUnspent = M.toList $ filterUtxoByAddr ourAddr utxo
        sortedUnspent = sortBy (comparing $ Down . txOutValue . snd) allUnspent
        inpOuts = do
            futxo <- evalStateT (pickInputs []) (totalMoney, sortedUnspent)
            let inputs = map fst futxo
                inputSum = sum $ map (txOutValue . snd) futxo
                newOuts = if inputSum > totalMoney
                          then TxOut ourAddr (inputSum - totalMoney) : outputs
                          else outputs
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
                    Just inp@(_, TxOut {..}) -> do
                        _1 -= min txOutValue moneyLeft
                        _2 %= tail
                        pickInputs (inp:inps)

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
            fmap toBool $ fmap ((== addr) . txOutAddress) <$> utxoGet (TxIn h idx)
        toBool Nothing  = False
        toBool (Just b) = b

-- | Checks if transaction is somehow related to address
relatedToAddress :: MonadUtxoRead m => Address -> Tx -> m Bool
relatedToAddress addr tx = if tx `hasReceiver` addr
                           then pure True
                           else hasSender addr tx

-- | Leave only outputs to given address in Tx
ownOutputs :: Address -> Tx -> Tx
ownOutputs addr = _txOutputs %~ filter ((== addr) . txOutAddress)

type TxSelector = UtxoStateT Maybe

-- | Select transactions related to given address
getRelatedTxs :: Address -> [WithHash Tx] -> TxSelector [WithHash Tx]
getRelatedTxs addr txs = do
    txs <- lift $ topsortTxs identity txs
    flip filterM txs $ \wtx -> do
        applyTxToUtxo $ wtx & _whData %~ ownOutputs addr
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
        txs <- getRelatedTxs addr $ blk ^.. blockTxs . folded . to withHash
        return $ DL.fromList txs <> hst
