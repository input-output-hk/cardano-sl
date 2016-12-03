-- | Functions for creating transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , submitTx
       , submitTxRaw
       ) where

import           Control.TimeWarp.Rpc (NetworkAddress)
import           Formatting           (build, sformat, (%))
import           System.Wlog          (logError, logInfo)
import           Universum

import           Pos.Communication    (sendTx)
import           Pos.Crypto           (SecretKey)
import           Pos.Crypto           (hash, sign, toPublic)
import           Pos.Types            (Address, Coin, Redeemer (..), Tx (..), TxId,
                                       TxIn (..), TxOut (..), Validator (..), txF)
import           Pos.WorkMode         (NodeContext (..), WorkMode, getNodeContext)

type TxInputs = [(TxId, Word32)]
type TxOutputs = [TxOut]

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

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitTx :: WorkMode ssc m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m Tx
submitTx na input output =
    if null na
      then logError "No addresses to send" >> panic "submitTx failed"
      else do
        sk <- ncSecretKey <$> getNodeContext
        let tx = makePubKeyTx sk [input] [uncurry TxOut output]
        submitTxRaw na tx
        pure tx

-- | Send the ready-to-use transaction
submitTxRaw :: WorkMode ssc m => [NetworkAddress] -> Tx -> m ()
submitTxRaw na tx = do
    let txId = hash tx
    logInfo $ sformat ("Submitting transaction: "%txF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    mapM_ (`sendTx` tx) na

