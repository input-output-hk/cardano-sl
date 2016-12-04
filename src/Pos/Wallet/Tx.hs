-- | Functions for creating transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , submitSimpleTx
       , submitTx
       , getBalance
       , submitTxRaw
       , createTx
       ) where

import           Control.Lens         (use, uses, (-=), _1, _2)
import           Control.Monad        (fail)
import           Control.Monad.State  (StateT, evalStateT)
import           Control.TimeWarp.Rpc (NetworkAddress)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Formatting           (build, sformat, (%))
import           System.Wlog          (logError, logInfo)
import           Universum

import           Pos.Communication    (sendTx)
import           Pos.Crypto           (SecretKey)
import           Pos.Crypto           (hash, sign, toPublic)
import           Pos.State            (getUtxoByDepth)
import           Pos.Types            (Address, Coin, Redeemer (..), Tx (..), TxId,
                                       TxIn (..), TxOut (..), Utxo, Validator (..),
                                       makePubKeyAddress, txF)
import           Pos.WorkMode         (NodeContext (..), WorkMode, getNodeContext)

type TxOutIdx = (TxId, Word32)
type TxInputs = [TxOutIdx]
type TxOutputs = [TxOut]
type TxError = Text

------------------------------------------------------------------------------------
-- Pure functions
------------------------------------------------------------------------------------

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SecretKey -> TxInputs -> TxOutputs -> Tx
makePubKeyTx sk inputs txOutputs = Tx {..}
  where pk = toPublic sk
        txInputs = map makeTxIn inputs
        makeTxIn (txInHash, txInIndex) =
            TxIn { txInValidator = PubKeyValidator pk
                 , txInRedeemer = PubKeyRedeemer $ sign sk (txInHash, txInIndex, txOutputs)
                 , ..
                 }

-- | Select only TxOuts for given addresses
filterUtxo :: Address -> Utxo -> Utxo
filterUtxo addr = M.filter ((addr ==) . txOutAddress)

type InputPicker = StateT (Coin, [(TxOutIdx, TxOut)]) (Either TxError)

-- | Make a multi-transaction using given secret key and info for outputs
createTx :: Utxo -> SecretKey -> TxOutputs -> Either TxError Tx
createTx utxo sk outputs = flip (makePubKeyTx sk) outputs <$> inputs
  where totalMoney = sum $ map txOutValue outputs
        ourAddr = makePubKeyAddress $ toPublic sk
        allUnspent = M.toList $ filterUtxo ourAddr utxo
        sortedUnspent = sortBy (comparing $ Down . txOutValue . snd) allUnspent
        inputs = evalStateT (pickInputs []) (totalMoney, sortedUnspent)

        pickInputs :: TxInputs -> InputPicker TxInputs
        pickInputs inps = do
            moneyLeft <- use _1
            if moneyLeft <= 0
                then return inps
                else do
                mNextOut <- uses _2 head
                case mNextOut of
                    Nothing -> fail "Not enough money to send!"
                    Just (inp, TxOut {..}) -> do
                        _1 -= txOutValue
                        pickInputs (inp:inps)

---------------------------------------------------------------------------------------
-- WorkMode scenarios
---------------------------------------------------------------------------------------

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitSimpleTx :: WorkMode ssc m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m Tx
submitSimpleTx [] _ _ =
    logError "No addresses to send" >> panic "submitSimpleTx failed"
submitSimpleTx na input output = do
    sk <- ncSecretKey <$> getNodeContext
    let tx = makePubKeyTx sk [input] [uncurry TxOut output]
    submitTxRaw na tx
    pure tx

-- | Construct Tx using secret key and given list of desired outputs
submitTx :: WorkMode ssc m => SecretKey -> [NetworkAddress] -> TxOutputs -> m Tx
submitTx _ [] _ = logError "No addresses to send" >> panic "submitTx failed"
submitTx sk na outputs = do
    utxo <- fromJust <$> getUtxoByDepth 0
    case createTx utxo sk outputs of
        Left err -> panic err
        Right tx -> tx <$ submitTxRaw na tx

-- | Get current balance with given address
getBalance :: WorkMode ssc m => Address -> m Coin
getBalance addr = fromJust <$> getUtxoByDepth 0 >>=
                  return . sum . M.map txOutValue . filterUtxo addr

-- | Send the ready-to-use transaction
submitTxRaw :: WorkMode ssc m => [NetworkAddress] -> Tx -> m ()
submitTxRaw na tx = do
    let txId = hash tx
    logInfo $ sformat ("Submitting transaction: "%txF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    mapM_ (`sendTx` tx) na

