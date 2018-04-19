-- | Function for running a client, for @NewWallet@.

module Client.Cardano.Wallet.Web.Endpoint.NewWallet
    ( newWalletIO
    ) where

import           Universum

import           Data.Function                     (id)
import           Data.Time.Clock                   (getCurrentTime)
import           Crypto.Random.Entropy             (getEntropy)

import           Client.Cardano.Wallet.Web.Api     (newWallet)
import           Client.Cardano.Wallet.Web.Run     (runEndpointClient)
import           Client.Cardano.Wallet.Web.Analyze (analyzeResponseIfNeeded, checkResponse)
import           Bench.Cardano.Wallet.Types        (BenchEndpoint (..), CompleteConfig (..),
                                                    Response, ResponseReport (..))
import           Pos.Wallet.Web.ClientTypes        (CWallet (..), CWalletInit (..),
                                                    CWalletMeta (..), CWalletAssurance (..))
import           Pos.Util.BackupPhrase             (BackupPhrase (..))
import           Pos.Util.Mnemonics                (toMnemonic)

-- | Run 'NewWallet' client. As a result we will get a newly created wallet.
newWalletIO :: CompleteConfig -> IO ()
newWalletIO conf@CompleteConfig {..} = do
    timeForNow <- getCurrentTime
    backupPhrase <- generateBackupPhrase
    let walletName = "Wallet " <> show timeForNow -- For unique name of wallet.
        assurance  = CWAStrict
        smallUnit  = 1 -- For Lovelaces
        initMeta   = CWalletMeta walletName assurance smallUnit
        walletInit = CWalletInit initMeta backupPhrase
        passPhrase = Nothing -- Don't use passphrase, for simplicity.
    response <- runEndpointClient conf $ newWallet passPhrase walletInit
    analyzeResponseIfNeeded NewWalletBench conf $ analyze response

-- | Generate new backup phrase for new wallet.
generateBackupPhrase :: IO BackupPhrase
generateBackupPhrase = do
    -- The size 16 should give us 12-words mnemonic after BIP-39 encoding.
    genMnemonic <- getEntropy 16
    let newMnemonic = either (error . show) id $ toMnemonic genMnemonic
    return $ mkBackupPhrase12 $ words newMnemonic
  where
    mkBackupPhrase12 :: [Text] -> BackupPhrase
    mkBackupPhrase12 ls
        | length ls == 12 = BackupPhrase ls
        | otherwise = error "Invalid number of words in backup phrase! Expected 12 words."

-- | Analyze response with new address.
analyze
    :: Response CWallet
    -> ResponseReport
analyze response =
    checkResponse response
                  "Cannot create new wallet"
                  $ \createdWallet -> ResponseReport $ show createdWallet
