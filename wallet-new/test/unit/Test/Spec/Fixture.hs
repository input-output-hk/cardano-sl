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

import           Pos.Util.Wlog (Severity)

import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.InjectFail (mkFInjects)

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.QuickCheck (arbitrary, frequency)
import           Test.QuickCheck.Monadic (PropertyM, pick)

import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import           Cardano.Wallet.Kernel.Internal (ActiveWallet, PassiveWallet)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (mockNodeStateDef)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = return ()

genSpendingPassword :: PropertyM IO (Maybe V1.SpendingPassword)
genSpendingPassword =
    pick (frequency [(20, pure Nothing), (80, Just <$> arbitrary)])

withLayer :: MonadIO m
          => ProtocolMagic
          -> (PassiveWalletLayer m -> PassiveWallet -> IO a)
          -> PropertyM IO a
withLayer pm cc = do
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        mockFInjects <- mkFInjects mempty
        WalletLayer.Kernel.bracketPassiveWallet
            pm
            Kernel.UseInMemory
            devNull
            keystore
            mockNodeStateDef
            mockFInjects
            $ \layer wallet -> cc layer wallet

type GenPassiveWalletFixture x = PropertyM IO (PassiveWallet -> IO x)
type GenActiveWalletFixture x  = PropertyM IO (Keystore.Keystore -> ActiveWallet  -> IO x)

withPassiveWalletFixture :: MonadIO m
                         => ProtocolMagic
                         -> GenPassiveWalletFixture x
                         -> (Keystore.Keystore -> PassiveWalletLayer m -> PassiveWallet -> x -> IO a)
                         -> PropertyM IO a
withPassiveWalletFixture pm prepareFixtures cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        mockFInjects <- mkFInjects mempty
        WalletLayer.Kernel.bracketPassiveWallet
            pm
            Kernel.UseInMemory
            devNull
            keystore
            mockNodeStateDef
            mockFInjects
            $ \layer wallet -> do
                fixtures <- generateFixtures wallet
                cc keystore layer wallet fixtures

withActiveWalletFixture :: MonadIO m
                        => ProtocolMagic
                        -> GenActiveWalletFixture x
                        -> (Keystore.Keystore -> ActiveWalletLayer m -> ActiveWallet -> x -> IO a)
                        -> PropertyM IO a
withActiveWalletFixture pm prepareFixtures cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        mockFInjects <- mkFInjects mempty
        WalletLayer.Kernel.bracketPassiveWallet pm Kernel.UseInMemory devNull keystore mockNodeStateDef mockFInjects $ \passiveLayer passiveWallet -> do
            withProvidedMagicConfig pm $ \_ _ _ -> do
                WalletLayer.Kernel.bracketActiveWallet
                        passiveLayer
                        passiveWallet
                        diffusion
                    $ \activeLayer activeWallet -> do
                        fixtures <- generateFixtures keystore activeWallet
                        cc keystore activeLayer activeWallet fixtures
    where
        diffusion :: Kernel.WalletDiffusion
        diffusion = Kernel.WalletDiffusion {
            walletSendTx                = \_tx -> return False
          , walletGetSubscriptionStatus = return mempty
          }
