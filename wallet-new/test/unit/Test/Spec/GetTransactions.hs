{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Spec.GetTransactions (spec) where


import           Universum

import           Control.Lens (to)
import           Control.Monad.Except (runExceptT)
import           Data.Acid (update)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as M
import           Formatting (build, sformat)
import           Servant.Server
import           Util.Buildable (ShowThroughBuild (..))

import           Test.Hspec (Spec, describe, expectationFailure, shouldBe,
                     shouldMatchList, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import           Pos.Chain.Txp (TxOut (..), TxOutAux (..))
import qualified Pos.Chain.Txp as Core
import           Pos.Core as Core
import           Pos.Core (Coin (..), IsBootstrapEraAddr (..),
                     deriveLvl2KeyPair, mkCoin)
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, ProtocolMagic,
                     ShouldCheckPassphrase (..), emptyPassphrase,
                     safeDeterministicKeyGen)
import           Pos.Crypto.HD (firstHardened)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Request.Pagination
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Handlers
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation (..),
                     InputGrouping (..), newOptions)
import           Cardano.Wallet.Kernel.DB.AcidState
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdAddressIx (..), HdRoot (..),
                     HdRootId (..), WalletName (..), eskToHdRootId,
                     hdAccountIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.TxMeta
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.PrefilterTx as Kernel
import qualified Cardano.Wallet.Kernel.Read as Kernel
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..),
                     walletPassiveLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Cardano.Wallet.WalletLayer.Kernel.Conv as Kernel.Conv
import           Cardano.Wallet.WalletLayer.Kernel.Transactions (toTransaction)

import qualified Test.Spec.Addresses as Addresses
import           Test.Spec.CoinSelection.Generators (InitialBalance (..),
                     Pay (..), genUtxoWithAtLeast)
import qualified Test.Spec.Fixture as Fixture
import qualified Test.Spec.NewPayment as NewPayment
import           TxMetaStorageSpecs (Isomorphic (..), genMeta)


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fix = Fix {
      fixtureHdRootId  :: HdRootId
    , fixtureHdRoot    :: HdRoot
    , fixtureESK       :: EncryptedSecretKey
    , fixtureAccountId :: AccountId
    , fixtureUtxo      :: Core.Utxo
    }

data Fixture = Fixture {
      fixture   :: [Fix]
    , fixturePw :: PassiveWallet
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: NetworkMagic
                -> InitialBalance
                -> Fixture.GenActiveWalletFixture Fixture
prepareFixtures nm initialBalance = do
    fixt <- forM [0x11, 0x22] $ \b -> do
        let (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 b) mempty
        let newRootId = eskToHdRootId nm esk
        newRoot <- initHdRoot <$> pure newRootId
                            <*> pure (WalletName "A wallet")
                            <*> pure NoSpendingPassword
                            <*> pure AssuranceLevelNormal
                            <*> (InDb <$> pick arbitrary)
        newAccountId <- HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
        utxo   <- pick (genUtxoWithAtLeast initialBalance)
        -- Override all the addresses of the random Utxo with something meaningful,
        -- i.e. with 'Address'(es) generated in a principled way, and not random.
        utxo' <- foldlM (\acc (txIn, (TxOutAux (TxOut _ coin))) -> do
                            newIndex <- deriveIndex (pick . choose) HdAddressIx HardDerivation

                            let Just (addr, _) = deriveLvl2KeyPair nm
                                                                (IsBootstrapEraAddr True)
                                                                (ShouldCheckPassphrase True)
                                                                mempty
                                                                esk
                                                                (newAccountId ^. hdAccountIdIx . to getHdAccountIx)
                                                                (getHdAddressIx newIndex)
                            return $ M.insert txIn (TxOutAux (TxOut addr coin)) acc
                        ) M.empty (M.toList utxo)
        return $ Fix {
              fixtureHdRootId = newRootId
            , fixtureHdRoot = newRoot
            , fixtureAccountId = AccountIdHdRnd newAccountId
            , fixtureESK = esk
            , fixtureUtxo = utxo'
            }

    return $ \keystore aw -> do
        let pw = Kernel.walletPassive aw
        forM_ fixt $ \Fix{..} -> do
            liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore

            let accounts         = Kernel.prefilterUtxo nm fixtureHdRootId fixtureESK fixtureUtxo
                hdAccountId      = Kernel.defaultHdAccountId fixtureHdRootId
                (Just hdAddress) = Kernel.defaultHdAddress nm fixtureESK emptyPassphrase fixtureHdRootId

            void $ liftIO $ update (pw ^. wallets) (CreateHdWallet fixtureHdRoot hdAccountId hdAddress accounts)
        return $ Fixture {
              fixture = fixt
            , fixturePw = pw
        }

withFixture :: MonadIO m
            => ProtocolMagic
            -> InitialBalance
            -> (  Keystore.Keystore
               -> WalletLayer.ActiveWalletLayer m
               -> Kernel.ActiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture pm initialBalance cc =
    Fixture.withActiveWalletFixture pm (prepareFixtures nm initialBalance) cc
  where
    nm = makeNetworkMagic pm

-- | Returns the address that is automatically created with the wallet.
getFixedAddress :: WalletLayer.ActiveWalletLayer IO -> Fix -> IO Core.Address
getFixedAddress layer Fix{..} = do
    let params = RequestParams (PaginationParams (Page 1) (PerPage 10))
    let filters = NoFilters
    Right wr <- WalletLayer.getAccountAddresses (walletPassiveLayer layer)
            (Kernel.Conv.toRootId fixtureHdRootId)
            (V1.unsafeMkAccountIndex firstHardened)
            params
            filters
    -- the defaut account of the wallet should have a unique address.
    let [address] = wrData wr
    return $ V1.unV1 . V1.addrId $ address

-- | Returns an address from the account we explicitely create.
getNonFixedAddress :: WalletLayer.ActiveWalletLayer IO -> Fix -> IO Core.Address
getNonFixedAddress layer Fix{..} = do
    let params = RequestParams (PaginationParams (Page 1) (PerPage 10))
    let filters = NoFilters
    let (AccountIdHdRnd hdAccountId) = fixtureAccountId
    let index = getHdAccountIx $ hdAccountId ^. hdAccountIdIx
    Right wr <- WalletLayer.getAccountAddresses (walletPassiveLayer layer)
            (Kernel.Conv.toRootId fixtureHdRootId)
            (V1.unsafeMkAccountIndex index)
            params
            filters
    -- the account we create in the fixture should also have an address
    let (address : _) = wrData wr
    return $ V1.unV1 . V1.addrId $ address

getAccountBalanceNow :: Kernel.PassiveWallet -> Fix -> IO Word64
getAccountBalanceNow pw Fix{..} = do
    let (AccountIdHdRnd hdAccountId) = fixtureAccountId
    let index = getHdAccountIx $ hdAccountId ^. hdAccountIdIx
    db <- Kernel.getWalletSnapshot pw
    let res =
            Accounts.getAccountBalance
            (Kernel.Conv.toRootId fixtureHdRootId)
            (V1.unsafeMkAccountIndex index)
            db
    bimap STB STB res `shouldSatisfy` isRight
    let Right (V1.AccountBalance (V1.V1 (Coin coins))) = res
    return coins

-- | A constant fee calculation.
constantFee :: Word64 -> Int -> NonEmpty Coin -> Coin
constantFee c _ _ = mkCoin c

constantFeeCheck :: Word64 -> Coin -> Bool
constantFeeCheck c c' = mkCoin c == c'

spec :: Spec
spec = do
    describe "GetTransactions" $ do
        prop "scenario: Layer.CreateAddress -> TxMeta.putTxMeta -> Layer.getTransactions works properly." $ withMaxSuccess 5 $
            monadicIO $ do
                testMetaSTB <- pick genMeta
                pm          <- pick arbitrary
                Addresses.withFixture pm $ \keystore layer pwallet Addresses.Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (HdRootId hdRoot) = fixtureHdRootId
                        (AccountIdHdRnd myAccountId) = fixtureAccountId
                        wId = sformat build (view fromDb hdRoot)
                        accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                        hdl = (pwallet ^. Kernel.walletMeta)
                        testMeta = unSTB testMetaSTB
                    case decodeTextAddress wId of
                        Left _         -> expectationFailure "decodeTextAddress failed"
                        Right rootAddr -> do
                            let meta = testMeta {_txMetaWalletId = rootAddr, _txMetaAccountIx = accIdx}
                            _ <- liftIO $ WalletLayer.createAddress layer
                                    (V1.NewAddress
                                        Nothing
                                        (V1.unsafeMkAccountIndex accIdx)
                                        (V1.WalletId wId)
                                    )
                            putTxMeta (pwallet ^. Kernel.walletMeta) meta
                            (result, mbCount) <- (getTxMetas hdl) (Offset 0) (Limit 10) Everything Nothing NoFilterOp NoFilterOp Nothing
                            map Isomorphic result `shouldMatchList` [Isomorphic meta]
                            let check WalletResponse{..} = do
                                    let PaginationMetadata{..} = metaPagination wrMeta
                                    wrStatus `shouldBe` SuccessStatus
                                    length wrData `shouldBe` 1
                                    metaTotalPages `shouldBe` 1
                                    metaTotalEntries `shouldBe` 1
                                    metaPage `shouldBe` (Page 1)
                                    metaPerPage `shouldBe` (PerPage 10)
                                    case wrData of
                                        [tx] -> V1.txStatus tx `shouldBe` V1.WontApply
                                        ls   -> expectationFailure $ "Tx list returned has wrong size "
                                            <> show (length ls) <> "instead of 1: ls = " <> show ls

                            eiResp <- WalletLayer.getTransactions
                                        layer
                                        Nothing
                                        Nothing
                                        Nothing
                                        (RequestParams $ PaginationParams (Page 1) (PerPage 10))
                                        NoFilters
                                        NoSorts
                            mbCount `shouldBe` (Just 1)
                            case eiResp of
                                Left l -> expectationFailure $ "returned " <> show l
                                Right resp -> check resp

        prop "scenario: Layer.pay -> Layer.getTransactions works properly. Tx status should be Applying " $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                NewPayment.withFixture @IO pm (InitialADA 10000) (PayLovelace 25) $ \keystore activeLayer aw NewPayment.Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (AccountIdHdRnd hdAccountId)  = fixtureAccountId
                    let (HdRootId (InDb rootAddress)) = fixtureHdRootId
                    let sourceWallet = V1.WalletId (sformat build rootAddress)
                    let accountIndex = Kernel.Conv.toAccountId hdAccountId
                    let destinations =
                            fmap (\(addr, coin) -> V1.PaymentDistribution (V1.V1 addr) (V1.V1 coin)
                                ) fixturePayees
                    let newPayment = V1.Payment {
                                    pmtSource          = V1.PaymentSource sourceWallet accountIndex
                                , pmtDestinations     = destinations
                                , pmtGroupingPolicy   = Nothing
                                , pmtSpendingPassword = Nothing
                                }
                    res <- liftIO ((WalletLayer.pay activeLayer) mempty
                                                                 IgnoreGrouping
                                                                 SenderPaysFee
                                                                 newPayment
                                  )
                    case res of
                        Left _   -> expectationFailure "Kernel.newTransaction failed"
                        Right (_, meta) -> do
                            let txid = _txMetaId meta
                                pw = Kernel.walletPassive aw
                                layer = walletPassiveLayer activeLayer
                                (HdRootId hdRoot) = fixtureHdRootId
                                wId = sformat build (view fromDb hdRoot)
                                accIdx = Kernel.Conv.toAccountId hdAccountId
                                hdl = (pw ^. Kernel.walletMeta)
                            db <- Kernel.getWalletSnapshot pw
                            let isPending = Kernel.currentTxIsPending db txid hdAccountId
                            _ <- case isPending of
                                Left _err -> expectationFailure "hdAccountId not found in Acid State from Kernel"
                                Right False -> expectationFailure "txid not found in Acid State from Kernel"
                                Right True -> pure ()
                            _ <- liftIO (WalletLayer.createAddress layer (V1.NewAddress Nothing accIdx (V1.WalletId wId)))
                            (result, mbCount) <- (getTxMetas hdl) (Offset 0) (Limit 10) Everything Nothing NoFilterOp NoFilterOp Nothing
                            map Isomorphic result `shouldMatchList` [Isomorphic meta]
                            let check WalletResponse{..} = do
                                    let PaginationMetadata{..} = metaPagination wrMeta
                                    wrStatus `shouldBe` SuccessStatus
                                    length wrData `shouldBe` 1
                                    metaTotalPages `shouldBe` 1
                                    metaTotalEntries `shouldBe` 1
                                    metaPage `shouldBe` (Page 1)
                                    metaPerPage `shouldBe` (PerPage 10)
                                    case wrData of
                                        [tx1] -> do
                                            V1.txStatus tx1 `shouldBe` V1.Applying
                                        ls   -> expectationFailure $ "Tx list returned has wrong size "
                                            <> show (length ls) <> "instead of 1: ls = " <> show ls

                            eiResp <- WalletLayer.getTransactions
                                        layer
                                        Nothing
                                        Nothing
                                        Nothing
                                        (RequestParams $ PaginationParams (Page 1) (PerPage 10))
                                        NoFilters
                                        NoSorts
                            mbCount `shouldBe` (Just 1)
                            case eiResp of
                                Left l -> expectationFailure $ "returned " <> show l
                                Right resp -> check resp

        prop "newTransaction and getTransactions return the same result" $ withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                NewPayment.withPayment pm (InitialADA 10000) (PayLovelace 100) $ \activeLayer newPayment -> do
                    payRes <- liftIO (runExceptT . runHandler' $ Handlers.newTransaction activeLayer newPayment)
                    getTxRes <- WalletLayer.getTransactions
                        (walletPassiveLayer activeLayer)
                        Nothing
                        Nothing
                        Nothing
                        (RequestParams $ PaginationParams (Page 1) (PerPage 10))
                        NoFilters
                        NoSorts
                    case (payRes, getTxRes) of
                        (Right txMetaPay, Right txMetaGet) ->
                            wrData txMetaGet `shouldBe` wrData  ((\x -> [x]) <$> txMetaPay)
                        _ -> expectationFailure "WalletLayer.getTransactions or Handlers.newTransaction failed"

        prop "TxMeta from pay has the correct txAmount" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                NewPayment.withFixture @IO pm (InitialADA 10000) (PayLovelace 100) $ \_ _ aw NewPayment.Fixture{..} -> do
                    -- we use constant fees here, to have predictable txAmount.
                    let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                    (_tx, txMeta) <- payAux aw hdAccountId fixturePayees 200
                    txMeta ^. txMetaAmount `shouldBe` Coin 300

    describe "Transactions with multiple wallets" $ do
        prop "test fixture has all the wanted properies" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) $ \_ layer aw (Fixture [w1, w2] _) -> do
                    db <- Kernel.getWalletSnapshot (Kernel.walletPassive aw)
                    let Right accs1 = Accounts.getAccounts (Kernel.Conv.toRootId $ fixtureHdRootId w1) db
                    length (IxSet.toList accs1) `shouldBe` 2
                    let Right accs2 = Accounts.getAccounts (Kernel.Conv.toRootId $ fixtureHdRootId w2) db
                    length (IxSet.toList accs2) `shouldBe` 2
                    _ <- getFixedAddress layer w1
                    _ <- getFixedAddress layer w2
                    _ <- getNonFixedAddress layer w1
                    _ <- getNonFixedAddress layer w2
                    return ()

        prop "TxMeta from pay between two wallets has the correct txAmount" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) $ \_ layer aw (Fixture [w1, w2] _) -> do
                    let pw = Kernel.walletPassive aw
                    address <- getFixedAddress layer w2
                    let (AccountIdHdRnd hdAccountId1) = fixtureAccountId w1
                    let payees = (NonEmpty.fromList [(address, Coin 100)])
                    (_tx, txMeta) <- payAux aw hdAccountId1 payees 200
                    txMeta ^. txMetaAmount `shouldBe` Coin 300
                    txMeta ^. txMetaIsOutgoing `shouldBe` True
                    txMeta ^. txMetaIsLocal `shouldBe` False
                    res <- toTransaction pw txMeta
                    bimap STB STB res `shouldSatisfy` isRight
                    let Right tx = res
                    V1.txStatus tx `shouldBe` V1.Applying
                    V1.txConfirmations tx `shouldBe` 0

        prop "as above but now we pay to the explicitely created account" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) $ \_ layer aw (Fixture [w1, w2] _) -> do
                    address <- getNonFixedAddress layer w2
                    let (AccountIdHdRnd hdAccountId1) = fixtureAccountId w1
                    let payees = (NonEmpty.fromList [(address, Coin 100)])
                    (_tx, txMeta) <- payAux aw hdAccountId1 payees 200
                    txMeta ^. txMetaAmount `shouldBe` Coin 300

        prop "payment to different wallet changes the balance the same as txAmount" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) $ \_ layer aw (Fixture [w1, w2] _) -> do
                    let pw = Kernel.walletPassive aw
                    -- get the balance before the payment
                    coinsBefore <- getAccountBalanceNow pw w1
                    -- do the payment
                    let (AccountIdHdRnd hdAccountId1) = fixtureAccountId w1
                    address <- getFixedAddress layer w2
                    let payees = (NonEmpty.fromList [(address, Coin 100)])
                    (_tx, txMeta) <- payAux aw hdAccountId1 payees 200
                    txMeta ^. txMetaAmount `shouldBe` Coin 300
                    -- get the balance after the payment
                    coinsAfter <- getAccountBalanceNow pw w1
                    coinsBefore - coinsAfter `shouldBe` 300

        prop "as above but now we pay to the explicitely created account" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) $ \_ layer aw (Fixture [w1, w2] _) -> do
                    let pw = Kernel.walletPassive aw
                    let (AccountIdHdRnd hdAccountId1) = fixtureAccountId w1
                    -- get the balance before the payment
                    coinsBefore <- getAccountBalanceNow pw w1
                    -- do the payment
                    address <- getNonFixedAddress layer w2
                    let payees = (NonEmpty.fromList [(address, Coin 100)])
                    (_tx, txMeta) <- payAux aw hdAccountId1 payees 200
                    txMeta ^. txMetaAmount `shouldBe` Coin 300
                    -- get the balance after the payment
                    coinsAfter <- getAccountBalanceNow pw w1
                    coinsBefore - coinsAfter `shouldBe` 300

        prop "2 consecutive payments" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) $ \_ layer aw (Fixture [w1, w2] _) -> do
                    let pw = Kernel.walletPassive aw
                    -- get the balance before the payment
                    coinsBefore <- getAccountBalanceNow pw w1
                    -- do the payment
                    let (AccountIdHdRnd hdAccountId1) = fixtureAccountId w1
                    address1 <- getFixedAddress layer w2
                    address2 <- getNonFixedAddress layer w2
                    let payees1 = (NonEmpty.fromList [(address1, Coin 100)])
                    (_, txMeta1) <- payAux aw hdAccountId1 payees1 200
                    txMeta1 ^. txMetaAmount `shouldBe` Coin 300
                    -- do the second payment
                    let payees2 = (NonEmpty.fromList [(address2, Coin 400)])
                    (_, txMeta2) <- payAux aw hdAccountId1 payees2 800
                    txMeta2 ^. txMetaAmount `shouldBe` Coin 1200
                    -- get the balance after the payment
                    coinsAfter <- getAccountBalanceNow pw w1
                    coinsBefore - coinsAfter `shouldBe` 1500

    describe "Transactions with multiple accounts" $ do
        prop "TxMeta from pay between two accounts of the same wallet has the correct txAmount" $ withMaxSuccess 5 $
            monadicIO $ do
                pm <- pick arbitrary
                withFixture @IO pm (InitialADA 10000) $ \_ layer aw (Fixture [w1, _] _) -> do
                    let pw = Kernel.walletPassive aw
                    -- get the balance before the payment
                    coinsBefore <- getAccountBalanceNow pw w1
                    -- do the payment
                    address <- getFixedAddress layer w1
                    let (AccountIdHdRnd hdAccountId1) = fixtureAccountId w1
                    let payees = (NonEmpty.fromList [(address, Coin 100)])
                    (_, txMeta) <- payAux aw hdAccountId1 payees 200
                    -- this is 200 because the outputs is at the same wallet.
                    txMeta ^. txMetaAmount `shouldBe` Coin 200
                    txMeta ^. txMetaIsOutgoing `shouldBe` True
                    txMeta ^. txMetaIsLocal `shouldBe` True
                    -- get the balance after the payment
                    coinsAfter <- getAccountBalanceNow pw w1
                    coinsBefore - coinsAfter `shouldBe` 300

payAux :: Kernel.ActiveWallet -> HdAccountId -> NonEmpty (Address, Coin) -> Word64 -> IO (Core.Tx, TxMeta)
payAux aw hdAccountId payees fees = do
    let opts = (newOptions (constantFee fees) (constantFeeCheck fees)) {
                          csoExpenseRegulation = SenderPaysFee
                        , csoInputGrouping = IgnoreGrouping
                        }
    payRes <- (Kernel.pay aw
                        mempty
                        opts
                        hdAccountId
                        payees
                        )
    bimap STB STB payRes `shouldSatisfy` isRight
    let Right t = payRes
    return t
