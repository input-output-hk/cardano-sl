{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module TransactionSpecs (transactionSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           Test.Hspec
import           Test.QuickCheck.Monadic (PropertyM, run)

import           Util hiding (makePayment)
import qualified Util

import qualified Data.Map.Strict as Map
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core
import           Pos.Util.Log.LoggerConfig (defaultTestConfiguration)
import           Pos.Util.Wlog (Severity (Debug), setupLogging)


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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

            liftIO $ txn `shouldBeConfirmed` resp

        randomTest "sending from asset-locked address should not be confirmed" 1 $ do
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

            liftIO $ txn `shouldNotBeConfirmed` resp

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
            (wallet, account) <- fixtureWallet []
            resp <- makePayment (Core.mkCoin 14) (wallet, account) =<< getRandomAddress
            let err = NotEnoughMoney (ErrAvailableBalanceIsInsufficient 0)
            expectFailure (ClientWalletError err) resp

        randomTest "fails if you spend more money than your available balance" 1 $ run $ do
            (wallet, account) <- fixtureWallet (Core.mkCoin <$> [42])
            resp <- makePayment (Core.mkCoin 10000) (wallet, account) =<< getRandomAddress
            let err = NotEnoughMoney (ErrAvailableBalanceIsInsufficient 42)
            expectFailure (ClientWalletError err) resp

        randomTest "fails if you can't cover fee with a transaction" 1 $ run $ do
            (wallet, account) <- fixtureWallet (Core.mkCoin <$> [42])
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

        -- NOTE:
        -- Cases where we have to increase the number of change outputs are hard
        -- to test in practice. We either need:
        --
        -- - A BIG change to cause an overflow (but even with all the genesis
        --   wallets, we don't have enough funds)
        --
        -- - A selection that will have no change such that a new one will be
        --   created for the change. However, the coin selection tends to always
        --   generate a change output.


        -- Initial Selection:      Final Selection:
        --   inputs : [200000]       inputs : [200000]
        --   outputs: [1]            outputs: [1]
        --   changes: [199999]       changes: [28094]
        --   fee+   : 171905         fee+   : 171817
        --
        --           Actual fee: 171905 (+88)
        randomTest "fee calculation: no extra inputs, no extra change" 1 $ run $ do
            source <- fixtureWallet (Core.mkCoin <$> [200000])
            resp <- makePayment (Core.mkCoin 1) source =<< getRandomAddress
            expectConfirmation source resp

        -- Initial Selection:      Final Selection:
        --   inputs : [171906]       inputs : [171906]
        --   outputs: [1]            outputs: [1]
        --   changes: [171905]       changes: []
        --   fee+   : 171905         fee+   : 167862
        --
        --           Actual fee: 167862 (+4043)
        randomTest "fee calculation: empty a wallet" 1 $ run $ do
            source <- fixtureWallet (Core.mkCoin <$> [171906])
            resp <- makePayment (Core.mkCoin 1) source =<< getRandomAddress
            expectConfirmation source resp

        -- Initial Selection:      Final Selection:
        --   inputs : [100000]       inputs : [100000, 100000]
        --   outputs: [1]            outputs: [1]
        --   changes: [99999]        changes: [19964]
        --   fee+   : 171905         fee+   : 179947
        --
        --           Actual fee: 180035 (+88)
        randomTest "fee calculation: needs extra input, no extra change" 1 $ run $ do
            source <- fixtureWallet (Core.mkCoin <$> [100000, 100000])
            resp <- makePayment (Core.mkCoin 1) source =<< getRandomAddress
            expectConfirmation source resp

        -- Initial Selection:      Final Selection:
        --   inputs : [30000]        inputs : [30000, 30000, 30000, 30000,
        --                                     30000, 30000, 30000, 30000]
        --   outputs: [42]           outputs: [42]
        --   changes: [29958]        changes: [11055]
        --   fee+   : 171905         fee+   : 228815
        --
        --           Actual fee: 228903 (+88)
        randomTest "fee calculation: needs many extra inputs" 1 $ run $ do
            source <- fixtureWallet (replicate 8 (Core.mkCoin 30000))
            resp <- makePayment (Core.mkCoin 42) source =<< getRandomAddress
            expectConfirmation source resp
  where
    makePayment amount (sourceW, sourceA) destination
       = fmap (fmap wrData) $ Util.makePayment wc amount (sourceW, sourceA) destination

    getRandomAddress
        :: IO Core.Address
    getRandomAddress = do
        wallet <- randomWallet CreateWallet >>= createWalletCheck wc
        (_, toAddr) <- firstAccountAndId wc wallet
        return (unV1 $ addrId toAddr)

    fixtureWallet
        :: [Core.Coin]
        -> IO (Wallet, Account)
    fixtureWallet coins = do
        genesis <- genesisWallet wc
        (genesisAccount, _) <- firstAccountAndId wc genesis
        wallet <- randomWallet CreateWallet >>= createWalletCheck wc
        (account, _) <- firstAccountAndId wc wallet
        forM_ coins $ \coin -> do
            -- Make transaction to different addresses to cope with input selection grouping.
            addr <- createAddress wc (wallet, account)
            txn <- makePayment coin (genesis, genesisAccount) (unV1 $ addrId addr) >>= shouldPrismFlipped _Right
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

    expectConfirmation
        :: (Wallet, Account)
        -> Either ClientError Transaction
        -> IO ()
    expectConfirmation (wallet, account) = \case
        Left err ->
            fail $ "Expected transcation confirmation, but got a ClientError: " <> show err
        Right txn ->
            pollTransactions wc (walId wallet) (accIndex account) (txId txn)

    shouldBeConfirmed
        :: Transaction
        -> [Transaction]
        -> IO ()
    shouldBeConfirmed txn transactions = do
        let txnEntry: _ = filter ( \x -> (txId x) == (txId txn)) transactions
        log $ "Resp   : " <> ppShowT txnEntry
        [InNewestBlocks, Persisted] `shouldContain` [txStatus txnEntry]
    infixr 8 `shouldBeConfirmed`

    shouldNotBeConfirmed
        :: Transaction
        -> [Transaction]
        -> IO ()
    shouldNotBeConfirmed txn transactions = do
        let txnEntry: _ = filter ( \x -> (txId x) == (txId txn)) transactions
        log $ "Resp   : " <> ppShowT txnEntry
        [InNewestBlocks, Persisted] `shouldNotContain` [txStatus txnEntry]
    infixr 8 `shouldNotBeConfirmed`
