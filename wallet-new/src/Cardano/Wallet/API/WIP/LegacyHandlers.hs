{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.API.WIP.LegacyHandlers (
      handlers
    ) where

import           Universum

import           Cardano.Wallet.API.Response (WalletResponse)
import           Cardano.Wallet.API.V1.Migration (MonadV1)
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP

import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.Chain.Txp (TxAux, TxpConfiguration)
import           Pos.Infra.Diffusion.Types (Diffusion (..))

import           Servant

handlers :: (forall a. MonadV1 a -> Handler a)
            -> Genesis.Config
            -> TxpConfiguration
            -> Diffusion MonadV1
            -> Server WIP.API
handlers naturalTransformation genesisConfig txpConfig diffusion =
         hoist' (Proxy @WIP.API) (handlersPlain genesisConfig txpConfig submitTx)
  where
    hoist'
        :: forall (api :: *). HasServer api '[]
        => Proxy api
        -> ServerT api MonadV1
        -> Server api
    hoist' p = hoistServer p naturalTransformation
    submitTx = sendTx diffusion

-- | All the @Servant@ handlers for wallet-specific operations.
handlersPlain :: Genesis.Config
         -> TxpConfiguration
         -> (TxAux -> MonadV1 Bool)
         -> ServerT WIP.API MonadV1
handlersPlain genesisConfig txpConfig submitTx =
         newExternalWallet genesisConfig
    :<|> deleteExternalWallet
    :<|> newUnsignedTransaction
    :<|> newSignedTransaction txpConfig submitTx

newExternalWallet
    :: Genesis.Config
    -> NewExternalWallet
    -> m (WalletResponse Wallet)
newExternalWallet _ _ =
    error "Removed as an obsolete handler, please see API/WIP/Handlers"

deleteExternalWallet
    :: PublicKeyAsBase58
    -> m NoContent
deleteExternalWallet _ =
    error "Removed as an obsolete handler, please see API/WIP/Handlers"

newUnsignedTransaction
    :: Payment
    -> m (WalletResponse UnsignedTransaction)
newUnsignedTransaction _ =
    error "Removed as an obsolete handler, please see API/WIP/Handlers"

newSignedTransaction
    :: TxpConfiguration
    -> (TxAux -> m Bool)
    -> SignedTransaction
    -> m (WalletResponse Transaction)
newSignedTransaction _ _ _ =
    error "Removed as an obsolete handler, please see API/WIP/Handlers"
