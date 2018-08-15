{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Spec.GetTransactions (spec) where


import           Universum

import           Test.Hspec (Spec, describe, expectationFailure, shouldBe,
                     shouldMatchList)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (withMaxSuccess)
import           Test.QuickCheck.Monadic (monadicIO, pick)

import           Control.Lens (to)
import           Formatting (build, sformat)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Request.Pagination
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation (..), InputGrouping (..))
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountIx (..),
                     HdRootId (..), hdAccountIdIx)

-- import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.TxMeta
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
-- import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
-- import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import           Cardano.Wallet.WalletLayer (walletPassiveLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Types as V1

import           Pos.Core as Core
-- import           Pos.Core.Txp
-- import           Pos.Crypto.Hashing
import qualified Test.Spec.Addresses as Addresses hiding (spec)
-- import qualified Test.Spec.Fixture as Fixture
import           Test.Spec.CoinSelection.Generators (InitialBalance (..),
                     Pay (..))
import qualified Test.Spec.NewPayment as NewPayment
import           TxMetaStorageSpecs (Isomorphic (..), genMeta)
import           Util.Buildable (ShowThroughBuild (..))


{-# ANN module ("HLint: ignore Unnecessary hiding" :: Text) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

spec :: Spec
spec =
    describe "GetTransactions" $ do

        prop "scenario: Layer.CreateAddress -> TxMeta.putTxMeta -> Layer.getTransactions works properly." $ withMaxSuccess 50 $
            monadicIO $ do
                testMetaSTB <- pick genMeta
                Addresses.withFixture $ \keystore layer pwallet Addresses.Fixture{..} -> do
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
                            _ <- liftIO ((WalletLayer._pwlCreateAddress layer) (V1.NewAddress Nothing accIdx (V1.WalletId wId)))
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

                            eiResp <- WalletLayer._pwlGetTransactions
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

        prop "scenario: Layer.pay -> Layer.getTransactions works properly. Tx status should be Applying " $ withMaxSuccess 50 $
            monadicIO $ do
                NewPayment.withFixture @IO (InitialADA 10000) (PayLovelace 10) $ \keystore activeLayer aw NewPayment.Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (AccountIdHdRnd hdAccountId)  = fixtureAccountId
                    let (HdRootId (InDb rootAddress)) = fixtureHdRootId
                    let sourceWallet = V1.WalletId (sformat build rootAddress)
                    let accountIndex = hdAccountId ^. hdAccountIdIx . to getHdAccountIx
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
                                accIdx = hdAccountId ^. hdAccountIdIx . to getHdAccountIx
                                hdl = (pw ^. Kernel.walletMeta)
                            db <- Kernel.getWalletSnapshot pw
                            let isPending = Kernel.accountIsTxPending db hdAccountId txid
                            _ <- case isPending of
                                False -> expectationFailure "txid not found in Acid State from Kernel"
                                True -> pure ()
                            _ <- liftIO ((WalletLayer._pwlCreateAddress layer) (V1.NewAddress Nothing accIdx (V1.WalletId wId)))
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
                                        [tx1] -> V1.txStatus tx1 `shouldBe` V1.Applying
                                        ls   -> expectationFailure $ "Tx list returned has wrong size "
                                            <> show (length ls) <> "instead of 1: ls = " <> show ls

                            eiResp <- WalletLayer._pwlGetTransactions
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


