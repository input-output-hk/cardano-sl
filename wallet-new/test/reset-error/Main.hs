{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import           Universum

import           Pos.Infra.InjectFail (mkFInjects)
import           Pos.Util.Wlog (Severity)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.Internal (PassiveWallet)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (mockNodeStateDef)
import qualified Cardano.Wallet.WalletLayer as WL
import qualified Cardano.Wallet.WalletLayer.Kernel as WL

import           Control.Concurrent (threadDelay)
import           Test.QuickCheck (Gen, arbitrary, frequency, generate)

withWalletLayer
          :: (WL.PassiveWalletLayer IO -> PassiveWallet -> IO a)
          -> IO a
withWalletLayer cc = do
    Keystore.bracketTestKeystore $ \keystore -> do
        mockFInjects <- mkFInjects mempty
        WL.bracketPassiveWallet
            Kernel.UseInMemory
            devNull
            keystore
            mockNodeStateDef
            mockFInjects
            cc
  where
    devNull :: Severity -> Text -> IO ()
    devNull _ _ = return ()

genNewWalletRq :: Gen V1.NewWallet
genNewWalletRq = do
    spendingPassword <- frequency [(20, pure Nothing), (80, Just <$> arbitrary)]
    assuranceLevel   <- arbitrary
    walletName       <- arbitrary
    mnemonic <- arbitrary @(BIP39.Mnemonic 12)
    return $ V1.NewWallet (V1.BackupPhrase mnemonic)
                          spendingPassword
                          assuranceLevel
                          walletName
                          V1.CreateWallet

sleepSeconds :: MonadIO m => Integer -> m ()
sleepSeconds sec =
    liftIO . delay $ sec * 1000 * 1000
  where
    delay time = do
        let maxWait = min time $ toInteger (maxBound :: Int)
        liftIO $ threadDelay (fromInteger maxWait)
        when (maxWait /= time) $ delay (time - maxWait)

main :: IO ()
main = withWalletLayer $ \pwl _ -> do
    WL.resetWalletState pwl
    sleepSeconds 1
    void $ WL.createWallet pwl . WL.CreateWallet =<< generate genNewWalletRq
    void $ WL.getWallets pwl
    WL.resetWalletState pwl
    sleepSeconds 1
    void $ WL.createWallet pwl . WL.CreateWallet =<< generate genNewWalletRq
    void $ WL.getWallets pwl
