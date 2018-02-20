module Cardano.Wallet.API.Development.Helpers where

import           Universum

import qualified Cardano.Wallet.API.Development as Dev
import           Cardano.Wallet.API.Response (WalletResponse)
import           Cardano.Wallet.Server.CLI (RunMode (..), isDebugMode)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot)
import           Servant
import           Control.Exception.Safe (throwM)

developmentOnly :: RunMode -> Server Dev.API -> Handler (WalletResponse WalletStateSnapshot)
developmentOnly runMode api
    | isDebugMode runMode = api
    | otherwise = throwM err403
