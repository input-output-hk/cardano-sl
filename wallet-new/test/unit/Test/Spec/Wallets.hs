{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Test.Spec.Wallets (
      spec
    , genNewWalletRq
    ) where

import           Universum

import           Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, suchThat, vectorOf, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import           Data.Coerce (coerce)
import           Formatting (build, formatToString)

import           Pos.Core (decodeTextAddress)
import           Pos.Crypto (emptyPassphrase, hash)

import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HdRootId (..), UnknownHdRoot (..), WalletName (..),
                     hdRootId)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create
                     (CreateHdRootError (..))
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Internal as Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import           Cardano.Wallet.Kernel.Wallets (CreateWalletError (..))
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets

import qualified Cardano.Wallet.API.Request as API
import qualified Cardano.Wallet.API.Request.Pagination as API
import qualified Cardano.Wallet.API.Response as V1
import           Cardano.Wallet.API.V1.Handlers.Wallets as Handlers
import           Cardano.Wallet.API.V1.Types (V1 (..), unV1)
import qualified Cardano.Wallet.API.V1.Types as V1
import           Control.Monad.Except (runExceptT)
import           Servant.Server

import           Test.Spec.Fixture (GenPassiveWalletFixture,
                     genSpendingPassword, withLayer, withPassiveWalletFixture)
import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureSpendingPassword :: V1.SpendingPassword
    , fixtureV1Wallet         :: V1.Wallet
    , fixtureHdRootId         :: HdRootId
    }

oppositeLevel :: V1.AssuranceLevel -> V1.AssuranceLevel
oppositeLevel V1.StrictAssurance = V1.NormalAssurance
oppositeLevel V1.NormalAssurance = V1.StrictAssurance

genNewWalletRq :: Maybe V1.SpendingPassword -> PropertyM IO V1.NewWallet
genNewWalletRq spendingPassword = do
    assuranceLevel   <- pick arbitrary
    walletName       <- pick arbitrary
    mnemonic <- BIP39.entropyToMnemonic <$> liftIO (BIP39.genEntropy @(BIP39.EntropySize 12))
    return $ V1.NewWallet (V1.BackupPhrase mnemonic)
                          spendingPassword
                          assuranceLevel
                          walletName
                          V1.CreateWallet

prepareFixtures :: GenPassiveWalletFixture Fixture
prepareFixtures = do
    spendingPassword <- pick (arbitrary `suchThat` ((/=) mempty))
    newWalletRq <- genNewWalletRq (Just spendingPassword)
    return $ \pw -> do
        res <- Wallets.createWallet pw newWalletRq
        case res of
             Left e         -> error (show e)
             Right v1Wallet -> do
                 let (V1.WalletId wId) = V1.walId v1Wallet
                 case decodeTextAddress wId of
                      Left e -> error $  "Error decoding the input Address "
                                      <> show wId
                                      <> ": "
                                      <> show e
                      Right rootAddr -> do
                          let rootId = HdRootId . InDb $ rootAddr
                          return (Fixture spendingPassword v1Wallet rootId)

-- | A 'Fixture' where we already have a new 'Wallet' in scope.
withNewWalletFixture :: (  Keystore.Keystore
                        -> WalletLayer.PassiveWalletLayer IO
                        -> Internal.PassiveWallet
                        -> Fixture
                        -> IO a
                        )
                     -> PropertyM IO a
withNewWalletFixture cc = withPassiveWalletFixture prepareFixtures cc

spec :: Spec
spec = describe "Wallets" $ do
    describe "CreateWallet" $ do
        describe "Wallet creation (wallet layer)" $ do

            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    pwd     <- genSpendingPassword
                    request <- genNewWalletRq pwd
                    withLayer $ \layer _ -> do
                        liftIO $ do
                            res <- (WalletLayer._pwlCreateWallet layer) request
                            (bimap STB STB res) `shouldSatisfy` isRight

            prop "fails if the wallet already exists" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    pwd     <- genSpendingPassword
                    request <- genNewWalletRq pwd
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
                    pwd     <- genSpendingPassword
                    request <- genNewWalletRq pwd
                    withLayer $ \layer _ -> do
                        let w' = request { V1.newwalName = "İıÀļƒȑĕďŏŨƞįťŢęșťıİ 日本" }
                        liftIO $ do
                            res <- (WalletLayer._pwlCreateWallet layer) w'
                            (bimap STB STB res) `shouldSatisfy` isRight


        describe "Wallet creation (kernel)" $ do
            prop "correctly persists the ESK in the keystore" $ withMaxSuccess 50 $
                monadicIO $ do
                    pwd     <- genSpendingPassword
                    V1.NewWallet{..} <- genNewWalletRq pwd
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
            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    pwd <- genSpendingPassword
                    rq  <- genNewWalletRq pwd
                    withLayer $ \layer _ -> do
                        liftIO $ do
                            res <- runExceptT . runHandler' $ Handlers.newWallet layer rq
                            (bimap identity STB res) `shouldSatisfy` isRight

    describe "DeleteWallet" $ do
        describe "Wallet deletion (wallet layer)" $ do

            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    withNewWalletFixture $ \_ layer _ Fixture{..} -> do
                        let wId = V1.walId fixtureV1Wallet
                        liftIO $ do
                            res1 <- WalletLayer.deleteWallet layer wId
                            (bimap STB STB res1) `shouldSatisfy` isRight
                            -- Check that the wallet is not there anymore
                            res2 <- WalletLayer.getWallet layer wId
                            (bimap STB STB res2) `shouldSatisfy` isLeft

            prop "cascade-deletes all the associated accounts" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    withNewWalletFixture $ \_ layer _ Fixture{..} -> do
                        let wId = V1.walId fixtureV1Wallet
                        liftIO $ do
                            let check predicate _ V1.Account{..} = do
                                    acc <- WalletLayer.getAccount layer wId accIndex
                                    (bimap STB STB acc) `shouldSatisfy` predicate

                            Right allAccounts <- WalletLayer.getAccounts layer wId

                            -- We should have 1 account, and fetching it must
                            -- succeed.
                            IxSet.size allAccounts `shouldBe` 1
                            foldM_ (check isRight) () allAccounts

                            -- Deletion should still return 'Right'.
                            res <- (WalletLayer.deleteWallet layer wId)
                            (bimap STB STB res) `shouldSatisfy` isRight

                            -- Fetching the old, not-existing-anymore accounts
                            -- should fail.
                            foldM_ (check isLeft) () allAccounts

            prop "fails if the wallet doesn't exists" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    wId  <- pick arbitrary
                    withLayer $ \layer _ -> do
                        liftIO $ do
                            res <- WalletLayer.deleteWallet layer wId
                            case res of
                                 Left (WalletLayer.DeleteWalletError (V1 (UnknownHdRoot _))) ->
                                     return ()
                                 Left unexpectedErr ->
                                     fail $ "expecting different failure than " <> show unexpectedErr
                                 Right _ -> fail "expecting wallet not to be created, but it was."


        describe "Wallet deletion (kernel)" $ do
            prop "correctly deletes the ESK in the keystore" $ withMaxSuccess 50 $
                monadicIO $ do
                    withNewWalletFixture $ \ks _ wallet Fixture{..} -> do
                        liftIO $ do
                            let wId = WalletIdHdRnd fixtureHdRootId

                            mbKey <- Keystore.lookup wId ks
                            mbKey `shouldSatisfy` isJust

                            res <- Kernel.deleteHdWallet wallet fixtureHdRootId

                            case res of
                                 Left e -> fail (formatToString build e)
                                 Right () -> do
                                     --  Check that the key is not in the keystore anymore
                                     mbKey' <- Keystore.lookup wId ks
                                     mbKey' `shouldSatisfy` isNothing

    describe "UpdateWalletPassword" $ do

        describe "Wallet update password (wallet layer)" $ do

            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    newPwd  <- pick arbitrary
                    withNewWalletFixture $ \ _ layer _ Fixture{..} -> do
                            let request = V1.PasswordUpdate fixtureSpendingPassword newPwd
                            let wId     = V1.walId fixtureV1Wallet
                            res <- (WalletLayer._pwlUpdateWalletPassword layer) wId request
                            (bimap STB STB res) `shouldSatisfy` isRight

            prop "fails if the old password doesn't match" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    wrongPwd  <- pick (arbitrary `suchThat` ((/=) mempty))
                    newPwd    <- pick arbitrary
                    withNewWalletFixture $ \ _ layer _ Fixture{..} -> do
                            let request = V1.PasswordUpdate wrongPwd newPwd
                            let wId     = V1.walId fixtureV1Wallet
                            res <- (WalletLayer._pwlUpdateWalletPassword layer) wId request
                            case res of
                                 Left (WalletLayer.UpdateWalletPasswordError (Kernel.UpdateWalletPasswordOldPasswordMismatch _)) ->
                                     return ()
                                 Left unexpectedErr ->
                                     fail $ "expecting different failure than " <> show unexpectedErr
                                 Right _ -> fail "expecting password not to be updated, but it was."

        describe "Wallet update password (kernel)" $ do
            prop "correctly replaces the ESK in the keystore" $ withMaxSuccess 50 $
                monadicIO $ do
                    newPwd <- pick arbitrary
                    withNewWalletFixture $ \ keystore _ wallet Fixture{..} -> do
                        let wid = WalletIdHdRnd fixtureHdRootId
                        oldKey <- Keystore.lookup wid keystore
                        res <- Kernel.updatePassword wallet
                                                     fixtureHdRootId
                                                     (unV1 fixtureSpendingPassword)
                                                     newPwd
                        case res of
                             Left e -> throwM e
                             Right (_db, _newRoot) -> do
                                 --  Check that the key was replaced in the keystore correctly.
                                 newKey <- Keystore.lookup wid keystore
                                 newKey `shouldSatisfy` isJust
                                 (fmap hash newKey) `shouldSatisfy` (not . (==) (fmap hash oldKey))

        describe "Wallet update password (Servant)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    newPwd <- pick arbitrary
                    withNewWalletFixture $ \ _ layer _ Fixture{..} -> do
                        liftIO $ do
                            let wId = V1.walId fixtureV1Wallet
                            let rq  = V1.PasswordUpdate fixtureSpendingPassword newPwd
                            res <- runExceptT . runHandler' $ Handlers.updatePassword layer wId rq
                            (bimap identity STB res) `shouldSatisfy` isRight

    describe "GetWallet" $ do

        -- There is no formal \"kernel function\" for this one, so we are
        -- testing only the WalletLayer and the Servant handler.
        describe "Get a specific wallet (wallet layer)" $ do

            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    withNewWalletFixture $ \ _ layer _ Fixture{..} -> do
                            let wId     = V1.walId fixtureV1Wallet
                            res <- WalletLayer.getWallet layer wId
                            (bimap STB STB res) `shouldBe` (Right (STB fixtureV1Wallet))

            prop "fails if the wallet doesn't exist" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    wId <- pick arbitrary
                    withLayer $ \ layer _ -> do
                            res <- WalletLayer.getWallet layer wId
                            case res of
                                 Left (WalletLayer.GetWalletError (V1 (UnknownHdRoot _))) ->
                                     return ()
                                 Left unexpectedErr ->
                                     fail $ "expecting different failure than " <> show unexpectedErr
                                 Right _ -> fail "expecting wallet not to be fetched, but it was."

        describe "Get a specific wallet (Servant)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    withNewWalletFixture $ \ _ layer _ Fixture{..} -> do
                        liftIO $ do
                            let wId = V1.walId fixtureV1Wallet
                            res <- runExceptT . runHandler' $ Handlers.getWallet layer wId
                            (bimap identity STB res) `shouldSatisfy` isRight

            prop "fails if the wallet doesn't exist" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    wId <- pick arbitrary
                    withLayer $ \layer _ -> do
                        let getW = Handlers.getWallet layer wId
                        res <- try . runExceptT . runHandler' $ getW
                        case res of
                             Left (_e :: WalletLayer.GetWalletError)  -> return ()
                             Right (Left e)  -> throwM e -- Unexpected Failure
                             Right (Right _) -> fail "Expecting a failure, but the handler succeeded."

    describe "UpdateWallet" $ do

        -- There is no formal \"kernel function\" for this one, so we are
        -- testing only the WalletLayer and the Servant handler.
        describe "Update a wallet (wallet layer)" $ do

            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    withNewWalletFixture $ \ _ layer _ Fixture{..} -> do
                            let wId     = V1.walId fixtureV1Wallet
                            let newLevel = oppositeLevel (V1.walAssuranceLevel fixtureV1Wallet)
                            res <- WalletLayer.updateWallet layer wId (V1.WalletUpdate newLevel "FooBar")
                            case res of
                                 Left e  -> fail (formatToString build e)
                                 Right w -> do
                                     V1.walAssuranceLevel w `shouldBe` newLevel
                                     V1.walName w `shouldBe` "FooBar"

            prop "fails if the wallet doesn't exist" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    wId  <- pick arbitrary
                    lvl  <- pick arbitrary
                    name <- pick arbitrary
                    withLayer $ \ layer _ -> do
                            res <- WalletLayer.updateWallet layer wId  (V1.WalletUpdate lvl name)
                            case res of
                                 Left (WalletLayer.UpdateWalletError (V1 (UnknownHdRoot _))) ->
                                     return ()
                                 Left unexpectedErr ->
                                     fail $ "expecting different failure than " <> show unexpectedErr
                                 Right _ -> fail "expecting wallet not to be updated, but it was."

        describe "Update a wallet (Servant)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    withNewWalletFixture $ \ _ layer _ Fixture{..} -> do
                        liftIO $ do
                            let wId = V1.walId fixtureV1Wallet
                            let newLevel = oppositeLevel (V1.walAssuranceLevel fixtureV1Wallet)
                            let updt     = V1.WalletUpdate newLevel "FooBar"
                            res <- runExceptT . runHandler' $ Handlers.updateWallet layer wId updt
                            case res of
                                 Left e  -> fail (show e)
                                 Right (V1.WalletResponse{..}) -> do
                                     V1.walAssuranceLevel wrData `shouldBe` newLevel
                                     V1.walName wrData `shouldBe` "FooBar"

            prop "fails if the wallet doesn't exist" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    wId <- pick arbitrary
                    lvl  <- pick arbitrary
                    name <- pick arbitrary
                    withLayer $ \layer _ -> do
                        let updateW = Handlers.updateWallet layer wId
                        res <- try . runExceptT . runHandler' $ updateW (V1.WalletUpdate lvl name)
                        case res of
                             Left (_e :: WalletLayer.UpdateWalletError)  -> return ()
                             Right (Left e)  -> throwM e -- Unexpected Failure
                             Right (Right _) -> fail "Expecting a failure, but the handler succeeded."

    describe "GetWallets" $ do

        -- There is no formal \"kernel function\" for this one, so we are
        -- testing only the WalletLayer and the Servant handler.
        describe "Gets a list of wallets (wallet layer)" $ do

            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    rqs <- map (\rq -> rq { V1.newwalOperation = V1.CreateWallet })
                               <$> pick (vectorOf 5 arbitrary)
                    withLayer $ \layer _ -> do
                        forM_ rqs (WalletLayer.createWallet layer)
                        res <- WalletLayer.getWallets layer
                        (IxSet.size res) `shouldBe` 5

        describe "Gets a list of wallets (Servant)" $ do
            prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
                monadicIO $ do
                    rqs <- map (\rq -> rq { V1.newwalOperation = V1.CreateWallet })
                               <$> pick (vectorOf 5 arbitrary)
                    withLayer $ \ layer _ -> do
                        liftIO $ do
                            forM_ rqs (runExceptT . runHandler' . Handlers.newWallet layer)
                            let params = API.RequestParams (API.PaginationParams (API.Page 1) (API.PerPage 10))
                            res <- runExceptT . runHandler' $ Handlers.listWallets layer params API.NoFilters API.NoSorts
                            case res of
                                 Left e -> throwM e
                                 Right (V1.WalletResponse{..}) -> do
                                     length wrData `shouldBe` 5
