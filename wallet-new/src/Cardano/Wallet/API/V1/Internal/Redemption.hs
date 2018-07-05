module Cardano.Wallet.API.V1.Internal.Redemption where

import           Universum

import           Data.Default (def)
import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration (HasConfigurations, MonadV1,
                     migrate)
import           Cardano.Wallet.API.V1.Types
import           Pos.Core (TxAux)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto.Configuration (ProtocolMagic)
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.Redeem as V0

type API
    = Summary "TODO: Describe the redemption endpoint"
    :> "redemptions"
    :> "ada"
    :> ReqBody '[ValidJSON] Redemption
    :> Get '[ValidJSON] (WalletResponse Transaction)

handlers
    :: HasConfigurations
    => ProtocolMagic
    -> (TxAux -> MonadV1 Bool)
    -> ServerT API MonadV1
handlers = redeemAda

redeemAda
    :: HasConfigurations
    => ProtocolMagic
    -> (TxAux -> MonadV1 Bool)
    -> Redemption
    -> MonadV1 (WalletResponse Transaction)
redeemAda pm submitTx r = do
    let ShieldedRedemptionCode seed = redemptionRedemptionCode r
        cwalletRedeem = error "TODO: conjure this up"
        passphrase = maybe def unV1 (redemptionPassphrase r)
    fmap single . migrate =<< case redemptionMnemonic r of
        Just mnemonic -> do
            let cpaperRedeem = error "TODO: conjure this up"
            V0.redeemAdaPaperVend pm submitTx passphrase cpaperRedeem
        Nothing ->
            V0.redeemAda pm submitTx passphrase cwalletRedeem
