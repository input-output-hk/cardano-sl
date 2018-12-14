{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Test.Spec.Accounts (spec) where

import           Universum

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)
import qualified Test.Spec.Wallets as Wallets

import           Formatting (build, formatToString, (%))

import qualified Cardano.Wallet.API.Request as API
import qualified Cardano.Wallet.API.Request.Pagination as API
import qualified Cardano.Wallet.API.Response as API
import           Cardano.Wallet.API.V1.Handlers.Accounts as Handlers
import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel.Accounts (CreateAccountError (..))
import qualified Cardano.Wallet.Kernel.DB.HdWallet as Kernel
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Internal as Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets
import           Control.Monad.Except (runExceptT)
import           Servant.Server

import           Pos.Core.Common (mkCoin)
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.HD (firstHardened)

import           Test.Spec.Fixture (GenPassiveWalletFixture,
                     genSpendingPassword, withLayer, withPassiveWalletFixture)
import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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
    newWalletRq <- WalletLayer.CreateWallet <$> Wallets.genNewWalletRq spendingPassword
    newAccountRq <- genNewAccountRq spendingPassword
    return $ \pw -> do
        res <- Wallets.createWallet pw newWalletRq
        case res of
             Left e         -> error (show e)
             Right v1Wallet -> return (Fixture spendingPassword v1Wallet newAccountRq)

withFixture :: MonadIO m
            => ProtocolMagic
            -> (  Keystore.Keystore
               -> PassiveWalletLayer m
               -> Internal.PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture pm cc = withPassiveWalletFixture pm prepareFixtures cc


spec :: Spec
spec = describe "Accounts" $ do
    describe "CreateAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    res <- WalletLayer.createAccount layer
                             (V1.walId fixtureV1Wallet)
                             fixtureNewAccountRq
                    (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                wId <- pick arbitrary
                pwd <- genSpendingPassword
                request <- genNewAccountRq pwd
                pm <- pick arbitrary
                withLayer pm $ \layer _ -> do
                    res <- WalletLayer.createAccount layer wId request
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

        prop "works when called from Servant" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let hdl = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    res <- runExceptT . runHandler' $ hdl
                    (bimap identity STB res) `shouldSatisfy` isRight

        prop "does NOT come with 1 address by default" $ withMaxSuccess 5 $ do
            -- We expect newly created accounts to @not@ have any associated
            -- addresses. Remember, it's only when we create a new HdRoot that
            -- we enforce this invariant.
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let hdl = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    res <- runExceptT . runHandler' $ hdl
                    case res of
                         Left e -> throwM e
                         Right API.APIResponse{..} ->
                             length (V1.accAddresses wrData) `shouldBe` 0

    describe "DeleteAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let wId = V1.walId fixtureV1Wallet
                    (Right V1.Account{..}) <-
                        WalletLayer.createAccount layer wId fixtureNewAccountRq
                    res <- WalletLayer.deleteAccount layer wId accIndex
                    (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                wId <- pick arbitrary
                pm  <- pick arbitrary
                withLayer pm $ \layer _ -> do
                    res <- WalletLayer.deleteAccount layer wId
                             (V1.unsafeMkAccountIndex firstHardened)
                    case res of
                         Left (WalletLayer.DeleteAccountError
                                  (Kernel.UnknownHdAccountRoot _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be deleted, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "fails if the account doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- Pick the first non-allocated index after 'firstHardened',
                    -- as by defaults each fixture's wallet is created with a
                    -- default account at index 'firstHardened'.
                    res <- WalletLayer.deleteAccount layer
                             (V1.walId fixtureV1Wallet)
                             (V1.unsafeMkAccountIndex (firstHardened + 1))
                    case res of
                         Left (WalletLayer.DeleteAccountError
                                  (Kernel.UnknownHdAccount _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be deleted, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg (V1.walId fixtureV1Wallet)

        prop "works when called from Servant" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    (Right API.APIResponse{..}) <- runExceptT . runHandler' $ create
                    let accountIndex = V1.accIndex wrData
                    let delete = Handlers.deleteAccount layer (V1.walId fixtureV1Wallet) accountIndex
                    res <- runExceptT . runHandler' $ delete
                    case res of
                         Left e  -> fail (show e)
                         -- There is no Buildable instance for 'NoContent', and
                         -- trying to make one would be overkill.
                         Right _ -> return ()

        prop "Servant handler fails if the parent wallet doesn't exist" $ withMaxSuccess 5 $ do
            monadicIO $ do
                wId <- pick arbitrary
                pm  <- pick arbitrary
                withLayer pm $ \layer _ -> do
                    let delete = Handlers.deleteAccount layer
                            wId
                            (V1.unsafeMkAccountIndex 2147483648)
                    res <- try . runExceptT . runHandler' $ delete
                    case res of
                         Left (_e :: WalletLayer.DeleteAccountError)  -> return ()
                         Right (Left e)  -> throwM e -- Unexpected Failure
                         Right (Right _) -> fail "Expecting a failure, but the handler succeeded."

        prop "Servant handler fails if the account doesn't exist" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- Pick the first non-allocated index after 'firstHardened',
                    -- as by defaults each fixture's wallet is created with a
                    -- default account at index 'firstHardened'.
                    let delete =
                            Handlers.deleteAccount layer
                                (V1.walId fixtureV1Wallet)
                                (V1.unsafeMkAccountIndex (firstHardened + 1))
                    res <- try . runExceptT . runHandler' $ delete
                    case res of
                         Left (_e :: WalletLayer.DeleteAccountError)  -> return ()
                         Right (Left e)  -> throwM e -- Unexpected Failure
                         Right (Right _) -> fail "Expecting a failure, but the handler succeeded."

    describe "UpdateAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let wId = V1.walId fixtureV1Wallet
                    (Right V1.Account{..}) <-
                        WalletLayer.createAccount layer wId fixtureNewAccountRq
                    let updateAccountRq = V1.AccountUpdate "My nice account"
                    res <- WalletLayer.updateAccount layer wId accIndex updateAccountRq
                    case res of
                         Left e -> fail (show e)
                         Right updatedAccount ->
                             V1.accName updatedAccount `shouldBe` "My nice account"

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                wId <- pick arbitrary
                pm  <- pick arbitrary
                withLayer pm $ \layer _ -> do
                    res <- WalletLayer.updateAccount layer
                             wId
                             (V1.unsafeMkAccountIndex 2147483648)
                             (V1.AccountUpdate "new account")
                    case res of
                         Left (WalletLayer.UpdateAccountError
                                (Kernel.UnknownHdAccountRoot _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be updated, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "fails if the account doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let wId = V1.walId fixtureV1Wallet
                    -- Pick the first non-allocated index after 'firstHardened',
                    -- as by defaults each fixture's wallet is created with a
                    -- default account at index 'firstHardened'.
                    res <- WalletLayer.updateAccount layer
                             wId
                             (V1.unsafeMkAccountIndex (firstHardened + 1))
                             (V1.AccountUpdate "new account")
                    case res of
                         Left (WalletLayer.UpdateAccountError
                                (Kernel.UnknownHdAccount _)) ->
                             return ()
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be updated, but it was. random WalletId "
                                        % build
                                        % " , V1.Wallet "
                             in fail $ formatToString errMsg wId

        prop "works when called from Servant" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    (Right API.APIResponse{..}) <- runExceptT . runHandler' $ create
                    let accountIndex = V1.accIndex wrData
                    let updateRq = V1.AccountUpdate "my new account"
                    let update = Handlers.updateAccount layer (V1.walId fixtureV1Wallet) accountIndex updateRq
                    res <- runExceptT . runHandler' $ update
                    case res of
                         Left e  -> fail (show e)
                         Right _ -> return ()

    describe "GetAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    (Right V1.Account{..}) <-
                        WalletLayer.createAccount layer (V1.walId fixtureV1Wallet)
                                                        fixtureNewAccountRq
                    res <- WalletLayer.getAccount layer (V1.walId fixtureV1Wallet)
                                                        accIndex
                    case res of
                         Left e    -> fail (show e)
                         Right acc -> V1.accIndex acc `shouldBe` accIndex

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                wId <- pick arbitrary
                pm  <- pick arbitrary
                withLayer pm $ \layer _ -> do
                    res <- WalletLayer.getAccount layer
                             wId
                             (V1.unsafeMkAccountIndex 2147483648)
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

        prop "fails if the account doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- Pick the first non-allocated index after 'firstHardened',
                    -- as by defaults each fixture's wallet is created with a
                    -- default account at index 'firstHardened'.
                    res <- WalletLayer.getAccount layer
                             (V1.walId fixtureV1Wallet)
                             (V1.unsafeMkAccountIndex (firstHardened + 1))
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

        prop "works when called from Servant" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    (Right API.APIResponse{..}) <- runExceptT . runHandler' $ create
                    let accountIndex = V1.accIndex wrData
                    let fetch = Handlers.getAccount layer (V1.walId fixtureV1Wallet) accountIndex
                    res <- runExceptT . runHandler' $ fetch
                    case res of
                         Left e   -> fail (show e)
                         Right wr -> (API.wrData wr) `shouldBe` wrData

    describe "GetAccounts" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- We create 4 accounts, plus one is created automatically
                    -- by the 'createWallet' endpoint, for a total of 5.
                    forM_ [1..4] $ \(_i :: Int) ->
                        WalletLayer.createAccount layer (V1.walId fixtureV1Wallet)
                                                        fixtureNewAccountRq
                    res <- WalletLayer.getAccounts layer (V1.walId fixtureV1Wallet)
                    case res of
                         Left e     -> fail (show e)
                         Right accs -> IxSet.size accs `shouldBe` 5

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                wId <- pick arbitrary
                pm  <- pick arbitrary
                withLayer pm $ \layer _ -> do
                    res <- WalletLayer.getAccounts layer wId
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

        prop "works when called from Servant" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    -- We create 4 accounts, plus one is created automatically
                    -- by the 'createWallet' endpoint, for a total of 5.
                    forM_ [1..4] $ \(_i :: Int) -> runExceptT . runHandler' $ create
                    let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let fetch = Handlers.listAccounts layer (V1.walId fixtureV1Wallet) params
                    res <- runExceptT . runHandler' $ fetch
                    case res of
                         Left e   -> fail (show e)
                         Right wr -> (length $ API.wrData wr) `shouldBe` 5


    describe "GetAccountAddresses" $ do

        prop "fails if the account doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let filters = API.NoFilters
                    -- Pick the first non-allocated index after 'firstHardened',
                    -- as by defaults each fixture's wallet is created with a
                    -- default account at index 'firstHardened'.
                    res <- WalletLayer.getAccountAddresses layer
                             (V1.walId fixtureV1Wallet)
                             (V1.unsafeMkAccountIndex (firstHardened + 1))
                             params
                             filters
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


        prop "applied to each newly created accounts gives addresses as obtained from GetAccounts" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- We create 4 accounts, plus one is created automatically
                    -- by the 'createWallet' endpoint, for a total of 5.
                    forM_ [1..4] $ \(_i :: Int) ->
                        WalletLayer.createAccount layer (V1.walId fixtureV1Wallet)
                                                        fixtureNewAccountRq
                    accounts <- WalletLayer.getAccounts layer (V1.walId fixtureV1Wallet)
                    let accountIndices =
                            case accounts of
                                Left _     -> []
                                Right accs -> map V1.accIndex $ IxSet.toList accs
                    let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let filters = API.NoFilters
                    partialAddresses <- forM accountIndices $ \(ind :: V1.AccountIndex) ->
                        WalletLayer.getAccountAddresses layer (V1.walId fixtureV1Wallet) ind params filters
                    case accounts of
                        Right accs -> (map V1.accAddresses $ IxSet.toList accs)
                                      `shouldBe`
                                      (map (\(Right addr) -> API.wrData addr) partialAddresses)
                        Left err   -> fail (show err)


        prop "and this also works when called from Servant" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    -- We create 4 accounts, plus one is created automatically
                    -- by the 'createWallet' endpoint, for a total of 5.
                    forM_ [1..4] $ \(_i :: Int) -> runExceptT . runHandler' $ create
                    let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let fetchForAccounts = Handlers.listAccounts layer (V1.walId fixtureV1Wallet) params
                    accounts <- runExceptT . runHandler' $ fetchForAccounts
                    let accountIndices =
                            case accounts of
                                Left _     -> []
                                Right accs -> map V1.accIndex $ API.wrData accs
                    let reqParams = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let filters = API.NoFilters
                    let fetchForAccountAddresses ind =
                            Handlers.getAccountAddresses layer (V1.walId fixtureV1Wallet)
                                                         ind reqParams filters
                    partialAddresses <- forM accountIndices $ \(ind :: V1.AccountIndex) ->
                        runExceptT . runHandler' $ fetchForAccountAddresses ind
                    case accounts of
                        Right accs  -> (map V1.accAddresses $ API.wrData accs)
                                       `shouldBe`
                                       (map (\(Right bal) -> (V1.acaAddresses . API.wrData) bal) partialAddresses)
                        Left err   -> fail (show err)


        prop "applied to accounts that were just updated via address creation is the same as obtained from GetAccounts" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- We create 4 accounts, plus one is created automatically
                    -- by the 'createWallet' endpoint, for a total of 5.
                    forM_ [1..4] $ \(_i :: Int) ->
                        WalletLayer.createAccount layer (V1.walId fixtureV1Wallet)
                                                        fixtureNewAccountRq
                    accountsBefore <- WalletLayer.getAccounts layer (V1.walId fixtureV1Wallet)
                    let accountIndices =
                            case accountsBefore of
                                Left _     -> []
                                Right accs -> map V1.accIndex $ IxSet.toList accs
                    forM_ accountIndices $ \(accIdx :: V1.AccountIndex) ->
                                WalletLayer.createAddress layer (V1.NewAddress Nothing accIdx (V1.walId fixtureV1Wallet))
                    accountsUpdated <- WalletLayer.getAccounts layer (V1.walId fixtureV1Wallet)
                    let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let filters = API.NoFilters
                    partialAddresses <- forM accountIndices $ \(ind :: V1.AccountIndex) ->
                        WalletLayer.getAccountAddresses layer (V1.walId fixtureV1Wallet) ind params filters
                    case accountsUpdated of
                        Right accs -> (map V1.accAddresses $ IxSet.toList accs)
                                      `shouldBe`
                                      (map (\(Right addr) -> API.wrData addr) partialAddresses)
                        Left err   -> fail (show err)


    describe "GetAccountBalance" $ do

        prop "gives zero balance for newly created account" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let zero = V1 (mkCoin 0)
                    (Right V1.Account{..}) <-
                        WalletLayer.createAccount layer (V1.walId fixtureV1Wallet)
                                                        fixtureNewAccountRq
                    res <- WalletLayer.getAccountBalance layer (V1.walId fixtureV1Wallet)
                                                               accIndex
                    case res of
                         Left e    -> fail (show e)
                         Right balance -> balance `shouldBe` V1.AccountBalance zero

        prop "fails if the account doesn't exists" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- Pick the first non-allocated index after 'firstHardened',
                    -- as by defaults each fixture's wallet is created with a
                    -- default account at index 'firstHardened'.
                    res <- WalletLayer.getAccountBalance layer
                             (V1.walId fixtureV1Wallet)
                             (V1.unsafeMkAccountIndex (firstHardened + 1))
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



        prop "applied to each newly created account gives balances as obtained from GetAccounts" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    -- We create 4 accounts, plus one is created automatically
                    -- by the 'createWallet' endpoint, for a total of 5.
                    forM_ [1..4] $ \(_i :: Int) ->
                        WalletLayer.createAccount layer (V1.walId fixtureV1Wallet)
                                                        fixtureNewAccountRq
                    accounts <- WalletLayer.getAccounts layer (V1.walId fixtureV1Wallet)
                    let accountIndices =
                            case accounts of
                                Left _     -> []
                                Right accs -> map V1.accIndex $ IxSet.toList accs
                    partialBalances <- forM accountIndices $ \(ind :: V1.AccountIndex) ->
                        WalletLayer.getAccountBalance layer (V1.walId fixtureV1Wallet) ind
                    case (accounts, length partialBalances /= 5) of
                        (Right accs, False) -> (map (V1.AccountBalance . V1.accAmount) $ IxSet.toList accs)
                                               `shouldBe`
                                               (map (\(Right bal) -> bal) partialBalances)
                        _                   -> fail "expecting to get 5 balances from partial getters"


        prop "and this also works when called from Servant" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let create = Handlers.newAccount layer (V1.walId fixtureV1Wallet) fixtureNewAccountRq
                    -- We create 4 accounts, plus one is created automatically
                    -- by the 'createWallet' endpoint, for a total of 5.
                    forM_ [1..4] $ \(_i :: Int) -> runExceptT . runHandler' $ create
                    let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                    let fetchForAccounts = Handlers.listAccounts layer (V1.walId fixtureV1Wallet) params
                    accounts <- runExceptT . runHandler' $ fetchForAccounts
                    let accountIndices =
                            case accounts of
                                Left _     -> []
                                Right accs -> map V1.accIndex $ API.wrData accs
                    let fetchForAccountBalance = Handlers.getAccountBalance layer (V1.walId fixtureV1Wallet)
                    partialBalances <- forM accountIndices $ \(ind :: V1.AccountIndex) ->
                        runExceptT . runHandler' $ fetchForAccountBalance ind
                    case (accounts, length partialBalances /= 5) of
                        (Right accs, False) -> (map (V1.AccountBalance . V1.accAmount) $ API.wrData accs)
                                               `shouldBe`
                                               (map (\(Right bal) -> API.wrData bal) partialBalances)
                        _                   -> fail "expecting to get 5 balances from partial getters"
