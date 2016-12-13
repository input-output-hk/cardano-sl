{-# LANGUAGE FlexibleContexts #-}

-- | Functions for creating transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , submitSimpleTx
       , submitTx
       , getBalance
       , submitTxRaw
       , createTx
       ) where

import           Control.Lens          (use, uses, (%=), (-=), _1, _2)
import           Control.Monad         (fail)
import           Control.Monad.State   (StateT, evalStateT)
import           Control.TimeWarp.Rpc  (NetworkAddress)
import           Data.List             (tail)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Vector           as V
import           Formatting            (build, sformat, (%))
import           System.Wlog           (logError, logInfo)
import           Universum

import           Pos.Binary            ()
import           Pos.Communication     (sendTx)
import           Pos.Context           (NodeContext (..), getNodeContext)
import           Pos.Crypto            (SecretKey)
import           Pos.Crypto            (hash, sign, toPublic)
import           Pos.Ssc.Class.Storage (SscStorageMode)
import           Pos.State             (WorkModeDB, getUtxoByDepth)
import           Pos.Types             (Address, Coin, Tx (..), TxId, TxIn (..),
                                        TxInWitness (..), TxOut (..), TxWitness, Utxo,
                                        makePubKeyAddress, txwF)
import           Pos.WorkMode          (WorkMode)

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
        makeTxIn (txInHash, txInIndex) = TxIn {..}
        makeTxInWitness (txInHash, txInIndex) =
            PkWitness {
                twKey = pk,
                twSig = sign sk (txInHash, txInIndex, txOutputs) }
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

---------------------------------------------------------------------------------------
-- WorkMode scenarios
---------------------------------------------------------------------------------------

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitSimpleTx :: WorkMode ssc m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m (Tx, TxWitness)
submitSimpleTx [] _ _ =
    logError "No addresses to send" >> fail "submitSimpleTx failed"
submitSimpleTx na input output = do
    sk <- ncSecretKey <$> getNodeContext
    let tx = makePubKeyTx sk [input] [uncurry TxOut output]
    submitTxRaw na tx
    pure tx

-- | Construct Tx using secret key and given list of desired outputs
submitTx :: WorkMode ssc m => SecretKey -> [NetworkAddress] -> TxOutputs -> m (Tx, TxWitness)
submitTx _ [] _ = logError "No addresses to send" >> fail "submitTx failed"
submitTx sk na outputs = do
    utxo <- fromJust <$> getUtxoByDepth 0
    case createTx utxo sk outputs of
        Left err -> fail $ toString err
        Right tx -> tx <$ submitTxRaw na tx

-- | Get current balance with given address
getBalance :: (SscStorageMode ssc, WorkModeDB ssc m) => Address -> m Coin
getBalance addr = fromJust <$> getUtxoByDepth 0 >>=
                  return . sum . M.map txOutValue . filterUtxo addr

-- | Send the ready-to-use transaction
submitTxRaw :: WorkMode ssc m => [NetworkAddress] -> (Tx, TxWitness) -> m ()
submitTxRaw na tx = do
    let txId = hash tx
    logInfo $ sformat ("Submitting transaction: "%txwF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    mapM_ (`sendTx` tx) na
