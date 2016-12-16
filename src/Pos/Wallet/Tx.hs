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

import           Control.Lens          (use, uses, (%=), (-=), (^.), _1, _2)
import           Control.Monad         (fail)
import           Control.Monad.State   (StateT, evalStateT)
import           Control.TimeWarp.Rpc  (NetworkAddress)
import           Data.List             (tail)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.Map              as M
import qualified Data.Vector           as V
import           Formatting            (build, sformat, (%))
import           System.Wlog           (logError, logInfo)
import           Universum

import           Pos.Binary            ()
import           Pos.Communication     (sendTx)
import           Pos.Context           (NodeContext (..), getNodeContext)
import           Pos.Crypto            (SecretKey, hash, sign, toPublic, withHash)
import           Pos.Ssc.Class.Storage (SscStorageMode)
import           Pos.State             (WorkModeDB, getBestChain, getOldestUtxo, getUtxo)
import           Pos.Types             (Address, Block, Coin, MainBlock, Tx (..), TxId,
                                        TxIn (..), TxInWitness (..), TxOut (..),
                                        TxWitness, Utxo, applyTxToUtxoPure, blockTxs,
                                        makePubKeyAddress, txwF)
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

-- | Select transactions from block by given predicate
selectBlockTxs :: (Tx -> Bool) -> MainBlock ssc -> [Tx]
selectBlockTxs p blk = foldl' selectTxs [] $ blk ^. blockTxs
  where selectTxs ls tx = if p tx then tx : ls else ls

-- | Select incoming transactions for given address from block
getIncomingTxs :: Address -> MainBlock ssc -> [Tx]
getIncomingTxs addr = selectBlockTxs (`hasReceiver` addr)

-- | Check if given 'Address' is one of the receivers of 'Tx'
hasReceiver :: Tx -> Address -> Bool
hasReceiver Tx {..} addr = any ((== addr) . txOutAddress) txOutputs

-- | Given some 'Utxo', get outgoing transactions for given address
getOutgoingTxs :: Utxo -> Address -> MainBlock ssc -> [Tx]
getOutgoingTxs utxo addr = selectBlockTxs (hasSender utxo addr)

-- | Given some 'Utxo', check ig given 'Address' is one of the senders of 'Tx'
hasSender :: Utxo -> Address -> Tx -> Bool
hasSender utxo addr Tx {..} = any hasCorrespondingOutput txInputs
  where hasCorrespondingOutput (TxIn h idx)  =
            toBool $ (== addr) . txOutAddress <$> M.lookup (h, idx) utxo
        toBool Nothing  = False
        toBool (Just b) = b

-- | Leave in Utxo only outputs of given addresses
getOwnUtxo :: Address -> Utxo -> Utxo
getOwnUtxo addr = M.filter ((== addr) . txOutAddress)

-- | Given a full blockchain, derive address history and Utxo
-- TODO: Such functionality will still be useful for merging
-- blockchains when wallet state is ready, but some metadata for
-- Tx will be required.
deriveAddrHistory :: Address -> Utxo -> NonEmpty (Block ssc) -> (Utxo, [Tx], [Tx])
deriveAddrHistory addr utxo = deriveAddrHistoryPartial (ownUtxo, [], []) addr
  where ownUtxo = getOwnUtxo addr utxo

deriveAddrHistoryPartial
    :: (Utxo, [Tx], [Tx])
    -> Address
    -> NonEmpty (Block ssc)
    -> (Utxo, [Tx], [Tx])
deriveAddrHistoryPartial histData addr chain = foldr updateAll histData chain
  where updateAll (Left _) hdata = hdata
        updateAll (Right blk) (utxo, incoming, outgoing) =
            let applyAllToUtxo = foldl' $ flip (applyTxToUtxoPure . withHash)
                newIncoming = getIncomingTxs addr blk
                utxo' = applyAllToUtxo utxo newIncoming
                newOutgoing = getOutgoingTxs utxo' addr blk
                utxo'' = applyAllToUtxo utxo' newOutgoing
            in (getOwnUtxo addr utxo'', newIncoming ++ incoming, newOutgoing ++ outgoing)

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
getTxHistory :: TxMode ssc m => Address -> m ([Tx], [Tx])
getTxHistory addr = do
    chain <- getBestChain
    utxo <- getOldestUtxo
    let (_, incoming, outgoing) = deriveAddrHistory addr utxo chain
    return (incoming, outgoing)
