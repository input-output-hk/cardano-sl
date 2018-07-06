module Cardano.Wallet.API.V1.Internal.Redemption where

import           Universum

import           Data.Default (def)
import           Servant

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration (HasConfigurations, MonadV1,
                     migrate)
import           Cardano.Wallet.API.V1.Types
import           Pos.Core (TxAux)
import           Pos.Crypto.Configuration (ProtocolMagic)
import qualified Pos.Wallet.Web.Methods.Redeem as V0
import qualified Pos.Wallet.Web.ClientTypes as V0
import qualified Pos.Util.Servant as V0

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
        passphrase = maybe def unV1 (redemptionPassphrase r)
    accountId <- migrate (redemptionWalletId r, redemptionAccountIndex r)
    let caccountId = V0.encodeCType accountId
    fmap single . migrate =<< case redemptionMnemonic r of
        Just mnemonic -> do
            phrase <- migrate mnemonic
            let cpaperRedeem = V0.CPaperVendWalletRedeem
                    { V0.pvWalletId = caccountId
                    , V0.pvSeed = seed
                    , V0.pvBackupPhrase = phrase
                    }
            V0.redeemAdaPaperVend pm submitTx passphrase cpaperRedeem
        Nothing -> do
            let cwalletRedeem = V0.CWalletRedeem
                    { V0.crWalletId = caccountId
                    , V0.crSeed = seed
                    }
            V0.redeemAda pm submitTx passphrase cwalletRedeem
