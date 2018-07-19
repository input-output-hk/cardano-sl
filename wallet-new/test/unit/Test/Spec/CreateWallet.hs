{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Test.Spec.CreateWallet (spec) where

import           Universum

import           Test.Hspec (Spec, describe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, frequency, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import           Data.Coerce (coerce)
import           System.Wlog (Severity)

import           Pos.Crypto (emptyPassphrase)

import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     WalletName (..), hdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create
                     (CreateHdRootError (..))
import           Cardano.Wallet.Kernel.Internal (PassiveWallet)
import qualified Cardano.Wallet.Kernel.Internal as Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.MonadDBReadAdaptor (rocksDBNotAvailable)
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Wallets (CreateWalletError (..))
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import           Cardano.Wallet.API.V1.Handlers.Wallets as Handlers
import qualified Cardano.Wallet.API.V1.Types as V1
import           Control.Monad.Except (runExceptT)
import           Servant.Server

import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = return ()

withLayer :: MonadIO m
          => (PassiveWalletLayer m -> PassiveWallet -> IO a)
          -> PropertyM IO a
withLayer cc = do
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        WalletLayer.bracketKernelPassiveWallet devNull keystore rocksDBNotAvailable $ \layer wallet -> do
            cc layer wallet

genNewWalletRq :: PropertyM IO V1.NewWallet
genNewWalletRq = do
    assuranceLevel   <- pick arbitrary
    walletName       <- pick arbitrary
    spendingPassword <- pick (frequency [(20, pure Nothing), (80, Just <$> arbitrary)])
    mnemonic <- BIP39.entropyToMnemonic <$> liftIO (BIP39.genEntropy @(BIP39.EntropySize 12))
    return $ V1.NewWallet (V1.BackupPhrase mnemonic)
                          spendingPassword
                          assuranceLevel
                          walletName
                          V1.CreateWallet

spec :: Spec
spec = describe "CreateWallet" $ do
    describe "Wallet creation (wallet layer)" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                request <- genNewWalletRq
                withLayer $ \layer _ -> do
                    liftIO $ do
                        res <- (WalletLayer._pwlCreateWallet layer) request
                        (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the wallet already exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                request <- genNewWalletRq
                withLayer $ \layer _ -> do
                    liftIO $ do
                        -- The first time it must succeed.
                        res1 <- (WalletLayer._pwlCreateWallet layer) request
                        (bimap STB STB res1) `shouldSatisfy` isRight

                        -- The second time it must not.
                        res2 <- (WalletLayer._pwlCreateWallet layer) request
                        case res2 of
                             Left (WalletLayer.CreateWalletError (CreateWalletFailed (CreateHdRootExists _))) ->
                                 return ()
                             Left unexpectedErr ->
                                 fail $ "expecting different failure than " <> show unexpectedErr
                             Right _ -> fail "expecting wallet not to be created, but it was."

        prop "supports Unicode characters" $ withMaxSuccess 1 $ do
            monadicIO $ do
                request <- genNewWalletRq
                withLayer $ \layer _ -> do
                    let w' = request { V1.newwalName = "İıÀļƒȑĕďŏŨƞįťŢęșťıİ 日本" }
                    liftIO $ do
                        res <- (WalletLayer._pwlCreateWallet layer) w'
                        (bimap STB STB res) `shouldSatisfy` isRight


    describe "Wallet creation (kernel)" $ do
        prop "correctly persists the ESK in the keystore" $ withMaxSuccess 50 $
            monadicIO $ do
                V1.NewWallet{..} <- genNewWalletRq
                withLayer @IO $ \_ wallet -> do
                    liftIO $ do
                        let hdAssuranceLevel = case newwalAssuranceLevel of
                                                    V1.NormalAssurance -> AssuranceLevelNormal
                                                    V1.StrictAssurance -> AssuranceLevelStrict
                        res <- Kernel.createHdWallet wallet
                                                     (V1.unBackupPhrase newwalBackupPhrase)
                                                     (maybe emptyPassphrase coerce newwalSpendingPassword)
                                                     hdAssuranceLevel
                                                     (WalletName newwalName)
                        case res of
                             Left e -> throwM e
                             Right hdRoot -> do
                                 --  Check that the key is in the keystore
                                 let wid = WalletIdHdRnd (hdRoot ^. hdRootId)
                                 mbEsk <- Keystore.lookup wid (wallet ^. Internal.walletKeystore)
                                 mbEsk `shouldSatisfy` isJust

    describe "Wallet creation (Servant)" $ do
        prop "works as expected in the happy path scenario" $ do
            monadicIO $ do
                rq <- genNewWalletRq
                withLayer $ \layer _ -> do
                    liftIO $ do
                        res <- runExceptT . runHandler' $ Handlers.newWallet layer rq
                        (bimap identity STB res) `shouldSatisfy` isRight
