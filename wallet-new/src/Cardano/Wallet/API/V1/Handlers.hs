module Cardano.Wallet.API.V1.Handlers (handlers) where

import Universum
import Servant
import System.Wlog (Severity)

import qualified Pos.Core as Core
import Pos.Crypto (PassPhrase)
import qualified Pos.Wallet.Web.Methods.Logic as L

import Cardano.Wallet.API.Response as Response
import qualified Cardano.Wallet.API.V1
import Cardano.Wallet.API.V1.Types (V1(V1), unV1)
import qualified Cardano.Wallet.API.V1.Types as V1T
import qualified Cardano.Wallet.API.V1.Wallets as V1_Wallets
import Cardano.Wallet.WalletLayer
  (ActiveWalletLayer, bracketKernelPassiveWallet)
import Cardano.Wallet.WalletLayer.Types
  (PassiveWalletLayer(_pwlCreateWallet), walletPassiveLayer)

handlers
  :: (Severity -> Text -> IO ()) -- ^ Logging function.
  -> ActiveWalletLayer IO
  -> ServerT Cardano.Wallet.API.V1.API IO
handlers logf awl =
         error "server_addresses"
    :<|> server_wallets logf awl
    :<|> error "server_accounts"
    :<|> error "server_transactions"
    :<|> error "server_settings"
    :<|> error "server_info"

server_wallets
  :: (Severity -> Text -> IO ()) -- ^ Logging function.
  -> ActiveWalletLayer IO
  -> ServerT V1_Wallets.API IO
server_wallets logf awl =
         server_wallets_new logf awl
    :<|> error "Returns a list of the available wallets."
    :<|> error "Updates the password for the given Wallet."
    :<|> error "Deletes the given Wallet and all its accounts."
    :<|> error "Returns the Wallet identified by the given walletId."
    :<|> error "Update the Wallet identified by the given walletId."

server_wallets_new
  :: (Severity -> Text -> IO ()) -- ^ Logging function.
  -> ActiveWalletLayer IO
  -> V1T.NewWallet
  -> IO (WalletResponse V1T.Wallet)
server_wallets_new logf awl nw =
  Response.single <$> _pwlCreateWallet (walletPassiveLayer awl) nw

