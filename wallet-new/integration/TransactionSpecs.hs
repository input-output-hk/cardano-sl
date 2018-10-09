{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module TransactionSpecs (transactionSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           Functions (randomTest)
import           Test.Hspec
import           Test.QuickCheck.Monadic (PropertyM, run)
import           Text.Show.Pretty (ppShow)

import           Util

import qualified Data.Map.Strict as Map
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core
import           Pos.Util.Log.LoggerConfig (defaultTestConfiguration)
import           Pos.Util.Wlog (Severity (Debug), setupLogging)


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

log :: MonadIO m => Text -> m ()
log = putStrLn . mappend "[TEST-LOG] "

ppShowT :: Show a => a -> Text
ppShowT = fromString . ppShow

transactionSpecs :: WalletRef -> WalletClient IO -> Spec
transactionSpecs wRef wc = beforeAll_ (setupLogging "wallet-new_transactionSpecs" (defaultTestConfiguration Debug)) $
    describe "Transactions" $ do

        randomTest "posted transactions appear in the index" 1 $ do
            genesis <- run $ genesisWallet wc
            (fromAcct, _) <- run $ firstAccountAndId wc genesis

            log $ show fromAcct

            wallet <- run $ sampleWallet wRef wc
            (toAcct, toAddr) <- run $ firstAccountAndId wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = V1 (Core.mkCoin 10)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

            etxn <- run $ postTransaction wc payment

            txn <- run $ fmap wrData etxn `shouldPrism` _Right

            eresp <- run $ getTransactionIndex wc Nothing Nothing Nothing
            resp <- run $ fmap wrData eresp `shouldPrism` _Right

            liftIO $ map txId resp `shouldContain` [txId txn]

            -- have to wait until it is accomodated in order not to interfere with other tests
            run $ pollTransactions wc (walId wallet) (accIndex toAcct) (txId txn)

        randomTest ( "asset-locked wallets can receive funds and transactions are "
           <> "confirmed in index") 1 $ do
            genesis <- run $ genesisWallet wc
            (fromAcct, _) <- run $ firstAccountAndId wc genesis

            wallet <- run $ genesisAssetLockedWallet wc
            (toAcct, toAddr) <- run $ firstAccountAndId wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = V1 (Core.mkCoin 11)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

            etxn <- run $ postTransaction wc payment

            txn <- run $ fmap wrData etxn `shouldPrism` _Right

            run $ pollTransactions wc (walId wallet) (accIndex toAcct) (txId txn)

            eresp <- run $ getTransactionIndex wc Nothing Nothing Nothing
            resp <- run $ fmap wrData eresp `shouldPrism` _Right

            liftIO $ map txId resp `shouldContain` [txId txn]
            let txnEntry: _ = filter ( \x -> (txId x) == (txId txn)) resp
            log $ "Resp   : " <> ppShowT txnEntry
            liftIO $ txConfirmations txnEntry `shouldNotBe` 0

        randomTest "sending from asset-locked address gets 0 confirmations from core nodes" 1 $ do
            genesis <- run $ genesisAssetLockedWallet wc
            (fromAcct, _) <- run $ firstAccountAndId wc genesis

            wallet <- run $ sampleWallet wRef wc
            (_, toAddr) <- run $ firstAccountAndId wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = V1 (Core.mkCoin 12)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

            etxn <- run $ postTransaction wc payment

            txn <- run $ fmap wrData etxn `shouldPrism` _Right

            eresp <- run $ getTransactionIndex wc Nothing Nothing Nothing
            resp <- run $ fmap wrData eresp `shouldPrism` _Right :: PropertyM IO [Transaction]

            let txnEntry : _ = filter ( \x -> (txId x) == (txId txn)) resp
            log $ "Resp   : " <> ppShowT txnEntry
            liftIO $ txConfirmations txnEntry `shouldBe` 0

        randomTest "estimate fees of a well-formed transaction" 1 $ do
            ws <- run $ (,)
                <$> (randomCreateWallet >>= createWalletCheck wc)
                <*> (randomCreateWallet >>= createWalletCheck wc)

            ((fromAcct, _), (_toAcct, toAddr)) <- run $ (,)
                <$> firstAccountAndId wc (fst ws)
                <*> firstAccountAndId wc (snd ws)

            let amount = V1 (Core.mkCoin 13)

            let payment = Payment
                    { pmtSource = PaymentSource
                        { psWalletId = walId (fst ws)
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = amount
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

            efee <- run $ getTransactionFee wc payment
            case efee of
                Right fee ->
                    liftIO $ feeEstimatedAmount (wrData fee)
                        `shouldSatisfy`
                            (> amount)
                Left (ClientWalletError (NotEnoughMoney _)) ->
                    liftIO $ pure ()
                Left err ->
                    liftIO $ expectationFailure $
                        "Expected either a successful fee or a NotEnoughMoney "
                        <> " error, got: "
                        <> show err

        randomTest "fails if you don't have any money" 1 $ run $ do
            (wallet, account) <- fixtureWallet Nothing
            resp <- makePayment (Core.mkCoin 14) (wallet, account) =<< getRandomAddress
            let err = NotEnoughMoney (ErrAvailableBalanceIsInsufficient 0)
            expectFailure (ClientWalletError err) resp

        randomTest "fails if you spend more money than your available balance" 1 $ run $ do
            (wallet, account) <- fixtureWallet (Just $ Core.mkCoin 42)
            resp <- makePayment (Core.mkCoin 10000) (wallet, account) =<< getRandomAddress
            let err = NotEnoughMoney (ErrAvailableBalanceIsInsufficient 42)
            expectFailure (ClientWalletError err) resp

        randomTest "fails if you can't cover fee with a transaction" 1 $ run $ do
            (wallet, account) <- fixtureWallet (Just $ Core.mkCoin 42)
            resp <- makePayment (Core.mkCoin 42) (wallet, account) =<< getRandomAddress
            let err = NotEnoughMoney ErrCannotCoverFee
            expectFailure (ClientWalletError err) resp

        randomTest "posted transactions gives rise to nonempty Utxo histogram" 1 $ do
            genesis <- run $ genesisWallet wc
            (fromAcct, _) <- run $ firstAccountAndId wc genesis

            newWallet <- run $ randomWallet CreateWallet
            wallet <- run $ createWalletCheck wc newWallet
            (toAcct, toAddr) <- run $ firstAccountAndId wc wallet

            let payment val = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = V1 (Core.mkCoin val)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

            eresp0 <- run $ getUtxoStatistics wc (walId wallet)
            utxoStatistics0 <- run $ fmap wrData eresp0 `shouldPrism` _Right
            let utxoStatistics0Expected = computeUtxoStatistics log10 []
            liftIO $ utxoStatistics0 `shouldBe` utxoStatistics0Expected

            etxn <- run $ postTransaction wc (payment 1)
            txn <- run $ fmap wrData etxn `shouldPrism` _Right

            run $ pollTransactions wc (walId wallet) (accIndex toAcct) (txId txn)

            let txIn  = Txp.TxInUnknown 0 "test"
            let txOut = Txp.TxOutAux Txp.TxOut
                    { Txp.txOutAddress = unV1 (addrId toAddr)
                    , Txp.txOutValue = Core.mkCoin 1
                    }
            let utxos = [Map.fromList [(txIn, txOut)]]

            eresp <- run $ getUtxoStatistics wc (walId wallet)
            utxoStatistics <- run $ fmap wrData eresp `shouldPrism` _Right
            let utxoStatisticsExpected = computeUtxoStatistics log10 utxos
            liftIO $ utxoStatistics `shouldBe` utxoStatisticsExpected

  where
    makePayment
        :: Core.Coin
        -> (Wallet, Account)
        -> Core.Address
        -> IO (Either ClientError Transaction)
    makePayment amount (sourceW, sourceA) destination = do
        let payment = Payment
                { pmtSource = PaymentSource
                    { psWalletId     = walId sourceW
                    , psAccountIndex = accIndex sourceA
                    }
                , pmtDestinations = pure PaymentDistribution
                    { pdAddress = V1 destination
                    , pdAmount  = V1 amount
                    }
                , pmtGroupingPolicy   = Nothing
                , pmtSpendingPassword = Nothing
                }
        fmap (fmap wrData) $ postTransaction wc payment

    getRandomAddress
        :: IO Core.Address
    getRandomAddress = do
        wallet <- randomWallet CreateWallet >>= createWalletCheck wc
        (_, toAddr) <- firstAccountAndId wc wallet
        return (unV1 $ addrId toAddr)

    fixtureWallet
        :: Maybe Core.Coin
        -> IO (Wallet, Account)
    fixtureWallet mcoin = do
        genesis <- genesisWallet wc
        (genesisAccount, _) <- firstAccountAndId wc genesis
        wallet <- randomWallet CreateWallet >>= createWalletCheck wc
        (account, address) <- firstAccountAndId wc wallet
        case mcoin of
            Nothing   -> return ()
            Just coin -> do
                txn <- makePayment coin (genesis, genesisAccount) (unV1 $ addrId address) >>= shouldPrismFlipped _Right
                pollTransactions wc (walId wallet) (accIndex account) (txId txn)
        return (wallet, account)

    expectFailure
        :: Show a
        => ClientError
        -> Either ClientError a
        -> IO ()
    expectFailure want eresp = do
        resp <- eresp `shouldPrism` _Left
        resp `shouldBe` want
