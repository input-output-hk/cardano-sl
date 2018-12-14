module Cardano.Wallet.API.WIP.Handlers (handlers)
where

import           Universum

import           Servant

import           Pos.Client.Txp.Util (defaultInputSelectionPolicy)

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Handlers.Transactions (txFromMeta)
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP (API)
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation (..))
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import           Cardano.Wallet.WalletLayer.Kernel.Conv (toInputGrouping)

-- | WIP @Servant@ handlers the are not part of the offical api yet.
handlers :: ActiveWalletLayer IO -> ServerT WIP.API Handler
handlers awl = newEosWallet pwl
           :<|> deleteEosWallet pwl
           :<|> newUnsignedTransaction awl
           :<|> submitSignedTransaction awl
  where
    pwl = walletPassiveLayer awl

newEosWallet :: PassiveWalletLayer IO
             -> NewEosWallet
             -> Handler (APIResponse EosWallet)
newEosWallet pwl newEosWalletRequest = do
    res <- liftIO $ WalletLayer.createEosWallet pwl newEosWalletRequest
    case res of
        Left err     -> throwM err
        Right wallet -> return $ single wallet

deleteEosWallet :: PassiveWalletLayer IO
                -> EosWalletId
                -> Handler NoContent
deleteEosWallet pwl encodedRootPK = do
    res <- liftIO $ WalletLayer.deleteEosWallet pwl encodedRootPK
    case res of
        Left err -> throwM err
        Right () -> return NoContent

-- | Creates new unsigned transaction.
--
-- NOTE: This function does /not/ perform a payment, it just prepares raw
-- transaction which will be signed and submitted to the blockchain later.
newUnsignedTransaction :: ActiveWalletLayer IO
                       -> Payment
                       -> Handler (APIResponse UnsignedTransaction)
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
                        -> Handler (APIResponse Transaction)
submitSignedTransaction aw signedTx = liftIO $ do
    res <- liftIO $ (WalletLayer.submitSignedTx aw) signedTx
    case res of
        Left err -> throwM err
        Right (_, meta) -> txFromMeta aw WalletLayer.NewPaymentUnknownAccountId meta
