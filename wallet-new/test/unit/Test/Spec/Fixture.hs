{--  | Helper module which tries to get rid of a bit of the boilerplate
       needed to initialise a kernel & an active/passive wallet.
--}

module Test.Spec.Fixture (
      withLayer
    , withPassiveWalletFixture
    , withActiveWalletFixture
    , GenActiveWalletFixture
    , GenPassiveWalletFixture
    -- * Useful generators
    , genSpendingPassword
    ) where

import           Universum

import           System.Wlog (Severity)

import           Test.Pos.Configuration (withDefConfiguration)
import           Test.QuickCheck (arbitrary, frequency)
import           Test.QuickCheck.Monadic (PropertyM, pick)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import           Cardano.Wallet.Kernel.Internal (ActiveWallet, PassiveWallet)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = return ()

genSpendingPassword :: PropertyM IO (Maybe V1.SpendingPassword)
genSpendingPassword =
    pick (frequency [(20, pure Nothing), (80, Just <$> arbitrary)])

withLayer :: MonadIO m
          => (PassiveWalletLayer m -> PassiveWallet -> IO a)
          -> PropertyM IO a
withLayer cc = do
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        WalletLayer.bracketKernelPassiveWallet devNull keystore $ \layer wallet -> do
            cc layer wallet

type GenPassiveWalletFixture x = PropertyM IO (PassiveWallet -> IO x)
type GenActiveWalletFixture x  = PropertyM IO (Keystore.Keystore -> ActiveWallet  -> IO x)

withPassiveWalletFixture :: MonadIO m
                         => GenPassiveWalletFixture x
                         -> (Keystore.Keystore -> PassiveWalletLayer m -> PassiveWallet -> x -> IO a)
                         -> PropertyM IO a
withPassiveWalletFixture prepareFixtures cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        WalletLayer.bracketKernelPassiveWallet devNull keystore $ \layer wallet -> do
            fixtures <- generateFixtures wallet
            cc keystore layer wallet fixtures

withActiveWalletFixture :: MonadIO m
                        => GenActiveWalletFixture x
                        -> (Keystore.Keystore -> ActiveWalletLayer m -> ActiveWallet -> x -> IO a)
                        -> PropertyM IO a
withActiveWalletFixture prepareFixtures cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        WalletLayer.bracketKernelPassiveWallet devNull keystore $ \passiveLayer passiveWallet -> do
            withDefConfiguration $ \pm -> do
                WalletLayer.bracketKernelActiveWallet pm passiveLayer passiveWallet diffusion $ \activeLayer activeWallet -> do
                    fixtures <- generateFixtures keystore activeWallet
                    cc keystore activeLayer activeWallet fixtures
    where
        diffusion :: Kernel.WalletDiffusion
        diffusion =  Kernel.WalletDiffusion {
            walletSendTx = \_tx -> return False
          }
