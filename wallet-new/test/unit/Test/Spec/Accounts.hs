{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Test.Spec.Accounts (spec) where

import           Universum

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)
import qualified Test.Spec.CreateWallet as Wallets

import           Formatting (build, formatToString, (%))

import           Cardano.Wallet.Kernel.Accounts (CreateAccountError (..),
                     DeleteAccountError (..), UpdateAccountError (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as Kernel
import qualified Cardano.Wallet.Kernel.Internal as Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import           Cardano.Wallet.WalletLayer.Types (PassiveWalletLayer)

import qualified Cardano.Wallet.API.Request as API
import qualified Cardano.Wallet.API.Request.Pagination as API
import qualified Cardano.Wallet.API.Response as API
import           Cardano.Wallet.API.V1.Handlers.Accounts as Handlers
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets
import           Control.Monad.Except (runExceptT)
import           Servant.Server

import           Test.Spec.Fixture (GenPassiveWalletFixture,
                     genSpendingPassword, withLayer, withPassiveWalletFixture)
import           Util.Buildable (ShowThroughBuild (..))


data Fixture = Fixture {
      fixtureSpendingPassword :: Maybe V1.SpendingPassword
    , fixtureV1Wallet         :: V1.Wallet
    , fixtureNewAccountRq     :: V1.NewAccount
    }

genNewAccountRq :: Maybe V1.SpendingPassword -> PropertyM IO V1.NewAccount
genNewAccountRq spendingPassword = do
    name <- pick arbitrary
    return $ V1.NewAccount spendingPassword name

prepareFixtures :: GenPassiveWalletFixture Fixture
prepareFixtures = do
    spendingPassword <- genSpendingPassword
    newWalletRq <- Wallets.genNewWalletRq spendingPassword
    newAccountRq <- genNewAccountRq spendingPassword
    return $ \pw -> do
        res <- Wallets.createWallet pw newWalletRq
        case res of
             Left e         -> error (show e)
             Right v1Wallet -> return (Fixture spendingPassword v1Wallet newAccountRq)

withFixture :: MonadIO m
            => (  Keystore.Keystore
               -> PassiveWalletLayer m
               -> Internal.PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture cc = withPassiveWalletFixture prepareFixtures cc


spec :: Spec
spec = describe "Accounts" $ do
    describe "CreateAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    res <- (WalletLayer._pwlCreateAccount layer)
                           (V1.walId fixtureV1Wallet)
                           fixtureNewAccountRq
                    (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                wId <- pick arbitrary
                pwd <- genSpendingPassword
                request <- genNewAccountRq pwd
                withLayer $ \layer _ -> do
                    res <- (WalletLayer._pwlCreateAccount layer) wId request
                    case res of
                         Left (WalletLayer.CreateAccountError (CreateAccountKeystoreNotFound _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be created, but it was. WalletId "
                                        % build
                                        % " , NewAccount request "
                                        % build
                             in fail $ formatToString errMsg wId request

        prop "works when called from Servant" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let hdl = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    res <- runExceptT . runHandler' $ hdl
                    (bimap identity STB res) `shouldSatisfy` isRight

    describe "DeleteAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let wId = V1.walId fixtureV1Wallet
                    (Right V1.Account{..}) <-
                        (WalletLayer._pwlCreateAccount layer)
                            wId fixtureNewAccountRq
                    res <- (WalletLayer._pwlDeleteAccount layer) wId accIndex
                    (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the account doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                wId <- pick arbitrary
                withLayer $ \layer _ -> do
                    res <- (WalletLayer._pwlDeleteAccount layer) wId 100
                    case res of
                         Left (WalletLayer.DeleteAccountError (DeleteAccountUnknownHdRoot _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be deleted, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "works when called from Servant" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    (Right API.WalletResponse{..}) <- runExceptT . runHandler' $ create
                    let accountIndex = V1.accIndex wrData
                    let delete = Handlers.deleteAccount layer (V1.walId fixtureV1Wallet) accountIndex
                    res <- runExceptT . runHandler' $ delete
                    case res of
                         Left e  -> fail (show e)
                         -- There is no Buildable instance for 'NoContent', and
                         -- trying to make one would be overkill.
                         Right _ -> return ()

        prop "Servant handler fails if the parent root doesn't exist" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let delete = Handlers.deleteAccount layer (V1.walId fixtureV1Wallet) 100
                    res <- runExceptT . runHandler' $ delete
                    case res of
                         Left e  -> fail (show e)
                         -- There is no Buildable instance for 'NoContent', and
                         -- trying to make one would be overkill.
                         Right _ -> return ()

    describe "UpdateAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let wId = V1.walId fixtureV1Wallet
                    (Right V1.Account{..}) <-
                        (WalletLayer._pwlCreateAccount layer)
                            wId fixtureNewAccountRq
                    let updateAccountRq = V1.AccountUpdate "My nice account"
                    res <- (WalletLayer._pwlUpdateAccount layer) wId accIndex updateAccountRq
                    case res of
                         Left e -> fail (show e)
                         Right updatedAccount ->
                             V1.accName updatedAccount `shouldBe` "My nice account"

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                wId <- pick arbitrary
                withLayer $ \layer _ -> do
                    res <- (WalletLayer._pwlUpdateAccount layer) wId 100 (V1.AccountUpdate "new account")
                    case res of
                         Left (WalletLayer.UpdateAccountError (UpdateAccountUnknownHdRoot _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be updated, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "fails if the account doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let wId = V1.walId fixtureV1Wallet
                    res <- (WalletLayer._pwlUpdateAccount layer) wId 100 (V1.AccountUpdate "new account")
                    case res of
                         Left (WalletLayer.UpdateAccountError (UpdateAccountUnknownHdAccount _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be updated, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "works when called from Servant" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    (Right API.WalletResponse{..}) <- runExceptT . runHandler' $ create
                    let accountIndex = V1.accIndex wrData
                    let updateRq = V1.AccountUpdate "my new account"
                    let update = Handlers.updateAccount layer (V1.walId fixtureV1Wallet) accountIndex updateRq
                    res <- runExceptT . runHandler' $ update
                    case res of
                         Left e  -> fail (show e)
                         Right _ -> return ()

    describe "GetAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    (Right V1.Account{..}) <-
                        (WalletLayer._pwlCreateAccount layer) (V1.walId fixtureV1Wallet)
                                                              fixtureNewAccountRq
                    res <- (WalletLayer._pwlGetAccount layer) (V1.walId fixtureV1Wallet)
                                                              accIndex
                    case res of
                         Left e    -> fail (show e)
                         Right acc -> V1.accIndex acc `shouldBe` accIndex

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                wId <- pick arbitrary
                withLayer $ \layer _ -> do
                    res <- (WalletLayer._pwlGetAccount layer) wId 100
                    case res of
                         Left (WalletLayer.GetAccountError (Kernel.UnknownHdAccountRoot _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be retrieved, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "fails if the account doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    res <- (WalletLayer._pwlGetAccount layer) (V1.walId fixtureV1Wallet) 100
                    case res of
                         Left (WalletLayer.GetAccountError (Kernel.UnknownHdAccount _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be retrieved, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg (V1.walId fixtureV1Wallet)

        prop "works when called from Servant" $ withMaxSuccess 50 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    (Right API.WalletResponse{..}) <- runExceptT . runHandler' $ create
                    let accountIndex = V1.accIndex wrData
                    let fetch = Handlers.getAccount layer (V1.walId fixtureV1Wallet) accountIndex
                    res <- runExceptT . runHandler' $ fetch
                    case res of
                         Left e   -> fail (show e)
                         Right wr -> (API.wrData wr) `shouldBe` wrData

    describe "GetAccounts" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 25 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    forM_ [1..5] $ \(_i :: Int) ->
                        (WalletLayer._pwlCreateAccount layer) (V1.walId fixtureV1Wallet)
                                                              fixtureNewAccountRq
                    res <- (WalletLayer._pwlGetAccounts layer) (V1.walId fixtureV1Wallet)
                    case res of
                         Left e     -> fail (show e)
                         Right accs -> IxSet.size accs `shouldBe` 5

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 25 $ do
            monadicIO $ do
                wId <- pick arbitrary
                withLayer $ \layer _ -> do
                    res <- (WalletLayer._pwlGetAccounts layer) wId
                    case res of
                         Left (WalletLayer.GetAccountsError (Kernel.UnknownHdRoot _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting accounts not to be retrieved, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "works when called from Servant" $ withMaxSuccess 25 $ do
            monadicIO $ do
                withFixture $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    forM_ [1..5] $ \(_i :: Int) -> runExceptT . runHandler' $ create
                    let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let fetch = Handlers.listAccounts layer (V1.walId fixtureV1Wallet) params
                    res <- runExceptT . runHandler' $ fetch
                    case res of
                         Left e   -> fail (show e)
                         Right wr -> (length $ API.wrData wr) `shouldBe` 5
