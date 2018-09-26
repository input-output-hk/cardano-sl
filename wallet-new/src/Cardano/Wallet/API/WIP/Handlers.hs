module Cardano.Wallet.API.WIP.Handlers (handlers)
where

import           Universum

import           Servant

import           Pos.Client.Txp.Util (defaultInputSelectionPolicy)

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP (API)
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (UnknownHdAccount)
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import           Cardano.Wallet.WalletLayer.Kernel.Conv (toInputGrouping)

-- | WIP @Servant@ handlers the are not part of the offical api yet.
handlers :: ActiveWalletLayer IO -> ServerT WIP.API Handler
handlers awl = checkExternalWallet pwl
           :<|> newExternalWallet pwl
           :<|> deleteExternalWallet pwl
           :<|> newUnsignedTransaction awl
           :<|> submitSignedTransaction awl
  where
    pwl = walletPassiveLayer awl

checkExternalWallet :: PassiveWalletLayer IO
                    -> PublicKeyAsBase58
                    -> Handler (WalletResponse WalletAndTxHistory)
checkExternalWallet _pwl _encodedRootPK =
    error "[CHW-54], Cardano Hardware Wallet feature, , check external wallet, unimplemented yet."

newExternalWallet :: PassiveWalletLayer IO
                  -> NewExternalWallet
                  -> Handler (WalletResponse Wallet)
newExternalWallet _pwl _newExtWallet =
    error "[CHW-80], Cardano Hardware Wallet feature, new external wallet, unimplemented yet."

deleteExternalWallet :: PassiveWalletLayer IO
                     -> PublicKeyAsBase58
                     -> Handler NoContent
deleteExternalWallet _pwl _encodedRootPK =
    error "[CHW-106], Cardano Hardware Wallet feature, , delete external wallet, unimplemented yet."

-- | Creates new unsigned transaction.
--
-- NOTE: This function does /not/ perform a payment, it just prepares raw
-- transaction which will be signed and submitted to the blockchain later.
newUnsignedTransaction :: ActiveWalletLayer IO
                       -> Payment
                       -> Handler (WalletResponse UnsignedTransaction)
newUnsignedTransaction aw payment@Payment{..} = do
    let inputGrouping = toInputGrouping $ fromMaybe (V1 defaultInputSelectionPolicy)
                                                    pmtGroupingPolicy
    res <- liftIO $ (WalletLayer.createUnsignedTx aw) inputGrouping
                                                      SenderPaysFee
                                                      payment
    case res of
        Left err         -> throwM err
        Right unsignedTx -> return $ single unsignedTx

-- | Submits externally-signed transaction to the blockchain.
submitSignedTransaction :: ActiveWalletLayer IO
                        -> SignedTransaction
                        -> Handler (WalletResponse Transaction)
submitSignedTransaction aw signedTx = liftIO $ do
    res <- liftIO $ (WalletLayer.submitSignedTx aw) signedTx
    case res of
        Left err -> throwM err
        Right (_, meta) -> txFromMeta aw WalletLayer.NewPaymentUnknownAccountId meta

-- | TODO: Remove duplication, because we already have
-- such a function in 'V1.Handlers.Transaction'.
txFromMeta :: Exception e
           => ActiveWalletLayer IO
           -> (UnknownHdAccount -> e)
           -> TxMeta
           -> IO (WalletResponse Transaction)
txFromMeta aw embedErr meta = do
    mTx <- WalletLayer.getTxFromMeta (WalletLayer.walletPassiveLayer aw) meta
    case mTx of
      Left err -> throwM (embedErr err)
      Right tx -> return $ single tx
