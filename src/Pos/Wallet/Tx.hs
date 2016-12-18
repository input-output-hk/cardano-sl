{-# LANGUAGE FlexibleContexts #-}

-- | Functions for creating transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , submitSimpleTx
       , submitTx
       , getBalance
       , getTxHistory
       , submitTxRaw
       , createTx
       ) where

import           Control.Lens          (folded, to, use, uses, (%=), (%=), (%~), (-=),
                                        (^..), _1, _2)
import           Control.Monad         (fail, filterM)
import           Control.Monad.Loops   (anyM)
import           Control.Monad.State   (StateT (..), evalStateT)
import           Control.TimeWarp.Rpc  (NetworkAddress)
import qualified Data.DList            as DL
import           Data.List             (tail)
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Vector           as V
import           Formatting            (build, sformat, (%))
import           System.Wlog           (logError, logInfo)
import           Universum

import           Pos.Binary            ()
import           Pos.Communication     (sendTx)
import           Pos.Context           (NodeContext (..), getNodeContext)
import           Pos.Crypto            (SecretKey, WithHash (..), hash, sign, toPublic,
                                        withHash, _whData)
import           Pos.Ssc.Class.Storage (SscStorageMode)
import           Pos.State             (WorkModeDB, getBestChain, getOldestUtxo, getUtxo)
import           Pos.Types             (Address, Block, Coin, MainBlock,
                                        MonadUtxoRead (..), Tx (..), TxId, TxIn (..),
                                        TxInWitness (..), TxOut (..), TxWitness, Utxo,
                                        UtxoStateT (..), applyTxToUtxo, blockTxs,
                                        evalUtxoStateT, makePubKeyAddress, topsortTxs,
                                        txwF, _txOutputs)
import           Pos.Wallet.WalletMode (TxMode)

type TxOutIdx = (TxId, Word32)
type TxInputs = [TxOutIdx]
type TxOutputs = [TxOut]
type TxError = Text

-----------------------------------------------------------------------------
-- Pure functions
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

-- | Select only TxOuts for given addresses
filterUtxo :: Address -> Utxo -> Utxo
filterUtxo addr = M.filter ((addr ==) . txOutAddress)

type FlatUtxo = [(TxOutIdx, TxOut)]
type InputPicker = StateT (Coin, FlatUtxo) (Either TxError)

-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SecretKey -> TxOutputs -> Either TxError (Tx, TxWitness)
createTx utxo sk outputs = uncurry (makePubKeyTx sk) <$> inpOuts
  where totalMoney = sum $ map txOutValue outputs
        ourAddr = makePubKeyAddress $ toPublic sk
        allUnspent = M.toList $ filterUtxo ourAddr utxo
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

-- | Select transactions related to given address from block
getRelatedTxs :: Address -> MainBlock ssc -> TxSelector [WithHash Tx]
getRelatedTxs addr blk = do
    txs <- lift $ topsortTxs identity (blk ^.. blockTxs . folded . to withHash)
    flip filterM txs $ \wtx -> do
        applyTxToUtxo $ wtx & _whData %~ ownOutputs addr
        relatedToAddress addr $ whData wtx

-- | Leave in Utxo only outputs of given addresses
getOwnUtxo :: Address -> Utxo -> Utxo
getOwnUtxo addr = M.filter ((== addr) . txOutAddress)

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory :: Address -> [Block ssc] -> TxSelector [WithHash Tx]
deriveAddrHistory addr chain = identity %= getOwnUtxo addr >>
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

---------------------------------------------------------------------------------------
-- WorkMode scenarios
---------------------------------------------------------------------------------------

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitSimpleTx :: TxMode ssc m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m (Tx, TxWitness)
submitSimpleTx [] _ _ =
    logError "No addresses to send" >> fail "submitSimpleTx failed"
submitSimpleTx na input output = do
    sk <- ncSecretKey <$> getNodeContext
    let tx = makePubKeyTx sk [input] [uncurry TxOut output]
    submitTxRaw na tx
    pure tx

-- | Construct Tx using secret key and given list of desired outputs
submitTx :: TxMode ssc m => SecretKey -> [NetworkAddress] -> TxOutputs -> m (Tx, TxWitness)
submitTx _ [] _ = logError "No addresses to send" >> fail "submitTx failed"
submitTx sk na outputs = do
    utxo <- getUtxo
    case createTx utxo sk outputs of
        Left err -> fail $ toString err
        Right tx -> tx <$ submitTxRaw na tx

-- | Get current balance with given address
getBalance :: (SscStorageMode ssc, WorkModeDB ssc m) => Address -> m Coin
getBalance addr = getUtxo >>= return . sum . M.map txOutValue . filterUtxo addr

-- | Send the ready-to-use transaction
submitTxRaw :: TxMode ssc m => [NetworkAddress] -> (Tx, TxWitness) -> m ()
submitTxRaw na tx = do
    let txId = hash tx
    logInfo $ sformat ("Submitting transaction: "%txwF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    mapM_ (`sendTx` tx) na

-- | Get tx history for Address
getTxHistory :: TxMode ssc m => Address -> m [WithHash Tx]
getTxHistory addr = do
    chain <- getBestChain
    utxo <- getOldestUtxo
    return $ fromJust $ flip evalUtxoStateT utxo $
        deriveAddrHistory addr $ NE.toList chain
