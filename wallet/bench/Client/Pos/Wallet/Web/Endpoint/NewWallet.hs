-- | Function for running a client, for @NewWallet@.

module Client.Pos.Wallet.Web.Endpoint.NewWallet
    ( newWalletIO
    ) where

import           Universum

import           Test.QuickCheck                   (arbitrary, generate)
import           Data.Time.Clock                   (getCurrentTime)

import           Client.Pos.Wallet.Web.Api         (newWallet)
import           Client.Pos.Wallet.Web.Run         (runEndpointClient)
import           Client.Pos.Wallet.Web.Analyze     (analyzeResponseIfNeeded, checkResponse)
import           Bench.Pos.Wallet.Types            (BenchEndpoint (..), CompleteConfig (..),
                                                    Response, ResponseReport (..))
import           Pos.Wallet.Web.ClientTypes        (CWallet (..), CWalletInit (..),
                                                    CWalletMeta (..), CWalletAssurance (..))
import           Pos.Util.BackupPhrase             (BackupPhrase)

-- | Run 'NewWallet' client. As a result we will get a newly created wallet.
newWalletIO :: CompleteConfig -> IO ()
newWalletIO conf@CompleteConfig {..} = do
    timeForNow <- getCurrentTime
    backupPhrase <- generate arbitrary :: IO BackupPhrase
    let walletName = "Wallet " <> show timeForNow -- For unique name of wallet.
        assurance  = CWAStrict
        smallUnit  = 1 -- For Lovelaces
        initMeta   = CWalletMeta walletName assurance smallUnit
        walletInit = CWalletInit initMeta backupPhrase
        passPhrase = Nothing -- Don't use passphrase, for simplicity.
    response <- runEndpointClient conf $ newWallet passPhrase walletInit
    analyzeResponseIfNeeded NewWalletBench conf $ analyze response

-- | Analyze response with new address.
analyze
    :: Response CWallet
    -> ResponseReport
analyze response =
    checkResponse response
                  "Cannot create new wallet"
                  $ \createdWallet -> ResponseReport $ show createdWallet
