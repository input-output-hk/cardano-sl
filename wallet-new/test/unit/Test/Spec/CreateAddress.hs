{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Spec.CreateAddress (spec) where

import           Universum

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

import           Control.Lens (to)
import           Data.Acid (update)
import           Formatting (build, sformat)

import           Pos.Crypto (EncryptedSecretKey, safeDeterministicKeyGen)

import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdRootId (..), WalletName (..),
                     eskToHdRootId, hdAccountIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.MonadDBReadAdaptor (rocksDBNotAvailable)
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import           Cardano.Wallet.API.V1.Handlers.Addresses as Handlers
import qualified Cardano.Wallet.API.V1.Types as V1
import           Control.Monad.Except (runExceptT)
import           Servant.Server

import qualified Test.Spec.Fixture as Fixture
import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureHdRootId  :: HdRootId
    , fixtureESK       :: EncryptedSecretKey
    , fixtureAccountId :: AccountId
    , fixturePw        :: PassiveWallet
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: Fixture.GenPassiveWalletFixture Fixture
prepareFixtures = do
    let (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 0x42) mempty
    let newRootId = eskToHdRootId esk
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    newAccountId <- HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
    let accounts = M.singleton newAccountId mempty
    return $ \pw -> do
        void $ liftIO $ update (pw ^. wallets) (CreateHdWallet newRoot accounts)
        return $ Fixture {
                           fixtureHdRootId = newRootId
                         , fixtureAccountId = AccountIdHdRnd newAccountId
                         , fixtureESK = esk
                         , fixturePw  = pw
                         }

withFixture :: MonadIO m
            => (  Keystore.Keystore
               -> PassiveWalletLayer m
               -> PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture = Fixture.withPassiveWalletFixture prepareFixtures

spec :: Spec
spec = describe "CreateAddress" $ do
    describe "Address creation (wallet layer)" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
            monadicIO $ do
                withFixture $ \keystore layer _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (HdRootId hdRoot) = fixtureHdRootId
                        (AccountIdHdRnd myAccountId) = fixtureAccountId
                        wId = sformat build (view fromDb hdRoot)
                        accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                    res <- liftIO ((WalletLayer._pwlCreateAddress layer) (V1.NewAddress Nothing accIdx (V1.WalletId wId)))
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

    describe "Address creation (kernel)" $ do
        prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
            monadicIO $ do
                withFixture @IO $ \keystore _ _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    res <- liftIO (Kernel.createAddress mempty fixtureAccountId fixturePw)
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

        prop "fails if the account has no associated key in the keystore" $ do
            monadicIO $ do
                withFixture @IO $ \_ _ _ Fixture{..} -> do
                    res <- liftIO (Kernel.createAddress mempty fixtureAccountId fixturePw)
                    case res of
                        (Left (Kernel.CreateAddressKeystoreNotFound acc)) | acc == fixtureAccountId -> return ()
                        x -> fail (show (bimap STB STB x))

        prop "fails if the parent account doesn't exist" $ do
            monadicIO $ do
                withFixture @IO $ \keystore _ _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                    void $ liftIO $ update (fixturePw ^. wallets) (DeleteHdAccount hdAccountId)
                    res <- liftIO (Kernel.createAddress mempty fixtureAccountId fixturePw)
                    case res of
                        Left (Kernel.CreateAddressUnknownHdAccount _) -> return ()
                        x -> fail (show (bimap STB STB x))

    describe "Address creation (Servant)" $ do
        prop "works as expected in the happy path scenario" $ do
            monadicIO $
                withFixture $ \keystore layer _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (HdRootId hdRoot) = fixtureHdRootId
                        (AccountIdHdRnd myAccountId) = fixtureAccountId
                        wId = sformat build (view fromDb hdRoot)
                        accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                        req = V1.NewAddress Nothing accIdx (V1.WalletId wId)
                    res <- liftIO (runExceptT . runHandler' $ Handlers.newAddress layer req)
                    liftIO ((bimap identity STB res) `shouldSatisfy` isRight)

    describe "Address creation (wallet layer & kernel consistency)" $ do
        prop "layer & kernel agrees on the result" $ do
            monadicIO $ do
                res1 <- withFixture @IO $ \keystore _ _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    liftIO (Kernel.createAddress mempty fixtureAccountId fixturePw)
                res2 <- withFixture @IO $ \keystore layer _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (HdRootId hdRoot) = fixtureHdRootId
                        (AccountIdHdRnd myAccountId) = fixtureAccountId
                        wId = sformat build (view fromDb hdRoot)
                        accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                    liftIO ((WalletLayer._pwlCreateAddress layer) (V1.NewAddress Nothing accIdx (V1.WalletId wId)))
                case res2 of
                     Left (WalletLayer.CreateAddressError err) ->
                         return $ (bimap STB STB res1) `shouldBe` (bimap STB STB (Left err))
                     Left (WalletLayer.CreateAddressAddressDecodingFailed _) ->
                         fail "Layer & Kernel mismatch: impossible error, CreateAddressAddressDecodingFailed"
                     Left (WalletLayer.CreateAddressTimeLimitReached _) ->
                         fail "The layer request exceeded the allocated time quota."
                     Right _ -> do
                         -- If we get and 'Address', let's check that this is the case also for
                         -- the kernel run. Unfortunately we cannot compare the two addresses for equality
                         -- because the random index will be generated with a seed which changes every time
                         -- as we uses random, IO-based generation deep down the guts.
                         return $ (bimap STB STB res1) `shouldSatisfy` isRight
