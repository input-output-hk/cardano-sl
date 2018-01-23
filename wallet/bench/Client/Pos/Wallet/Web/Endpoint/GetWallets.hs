-- | Run 'GetWallets' client.

module Client.Pos.Wallet.Web.Endpoint.GetWallets
    ( getWalletsIO
    ) where

import           Universum

import           Client.Pos.Wallet.Web.Api (getWallets)
import           Client.Pos.Wallet.Web.Run (runEndpointClient)

-- import           Pos.Wallet.Web.ClientTypes (CWallet (..))

-- | Run 'GetWallets' client. As a result we get a list of wallets.
getWalletsIO :: IO ()
getWalletsIO =
    runEndpointClient getWallets >>= \case
        Left problem  -> putText $ "Cannot obtain wallets information: " <> problem
        Right wallets -> print wallets     -- :: [CWallet]

{-
-- | Client Wallet (CW)
data CWallet = CWallet
    { cwId             :: !(CId Wal)
    , cwMeta           :: !CWalletMeta
    , cwAccountsNumber :: !Int
    , cwAmount         :: !CCoin
    , cwHasPassphrase  :: !Bool
    , cwPassphraseLU   :: !PassPhraseLU  -- last update time
    } deriving (Eq, Show, Generic)
-}
