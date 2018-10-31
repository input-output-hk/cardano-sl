{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.API.WIP.LegacyHandlers (
      handlers
    ) where

import           Universum

import           Cardano.Wallet.API.Response (WalletResponse)
import           Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.API.WIP as WIP

import           Pos.Wallet.Web.Mode (WalletWebMode)

import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.Chain.Txp (TxAux, TxpConfiguration)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Servant


handlers :: (forall a. WalletWebMode a -> Handler a)
            -> Genesis.Config
            -> TxpConfiguration
            -> Diffusion WalletWebMode
            -> Server WIP.API
handlers naturalTransformation genesisConfig txpConfig diffusion =
         hoist' (Proxy @WIP.API) (handlersPlain genesisConfig txpConfig submitTx)
  where
    hoist'
        :: forall (api :: *). HasServer api '[]
        => Proxy api
        -> ServerT api WalletWebMode
        -> Server api
    hoist' p = hoistServer p naturalTransformation
    submitTx = sendTx diffusion

-- | All the @Servant@ handlers for wallet-specific operations.
handlersPlain :: Genesis.Config
         -> TxpConfiguration
         -> (TxAux -> WalletWebMode Bool)
         -> ServerT WIP.API WalletWebMode
handlersPlain genesisConfig txpConfig submitTx =
         newExternalWallet genesisConfig
    :<|> deleteExternalWallet
    :<|> newUnsignedTransaction
    :<|> newSignedTransaction txpConfig submitTx

newExternalWallet
    :: Genesis.Config
    -> NewExternalWallet
    -> m (WalletResponse ExternalWallet)
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
