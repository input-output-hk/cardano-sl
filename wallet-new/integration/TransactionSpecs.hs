{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module TransactionSpecs (transactionSpecs) where

import           Universum
import qualified Serokell.Util.Base16 as B16

import qualified Cardano.Crypto.Wallet as CC
import           Cardano.Wallet.API.V1.Errors hiding (describe)
import           Cardano.Wallet.Client.Http
import           Pos.Binary.Class (decodeFull')
import qualified Pos.Core as Core
import           Pos.Crypto (SecretKey, SignTag (..), Signature (..), emptyPassphrase,
                             encToPublic, encodeBase58PublicKey, hash, noPassEncrypt, sign)
import           Pos.Crypto.HD (ShouldCheckPassphrase (..),
                                deriveHDPassphrase, deriveHDSecretKey)

import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           Control.Concurrent (threadDelay)
import           Text.Show.Pretty (ppShow)
import           Util

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

log :: MonadIO m => Text -> m ()
log = putStrLn . mappend "[TEST-LOG] "

ppShowT :: Show a => a -> Text
ppShowT = fromString . ppShow

transactionSpecs :: WalletRef -> WalletClient IO -> Spec
transactionSpecs wRef wc = do
    describe "Transactions" $ do
        it "posted transactions appear in the index" $ do
            genesis <- makeGenesisWallet wc
            (fromAcct, _) <- getFirstAccountAndAddress wc genesis

            wallet <- sampleWallet wRef wc
            (toAcct, toAddr) <- getFirstAccountAndAddress wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = tenthOf (accAmount fromAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                tenthOf (V1 c) =
                    V1 (Core.mkCoin (max 1 (Core.getCoin c `div` 10)))

            etxn <- postTransaction wc payment

            txn <- fmap wrData etxn `shouldPrism` _Right

            eresp <- getTransactionIndex
                wc
                (Just (walId wallet))
                (Just (accIndex toAcct))
                Nothing
            resp <- fmap wrData eresp `shouldPrism` _Right

            map txId resp `shouldContain` [txId txn]

        it ( "asset-locked wallets can receive funds and transactions are "
           <> "confirmed in index"
           ) $ do
            genesis <- genesisWallet wc
            (fromAcct, _) <- firstAccountAndId wc genesis

            wallet <- genesisAssetLockedWallet wc
            (toAcct, toAddr) <- firstAccountAndId wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = tenthOf (accAmount fromAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                tenthOf (V1 c) = V1 (Core.mkCoin (Core.getCoin c `div` 10))

            etxn <- postTransaction wc payment

            txn <- fmap wrData etxn `mustBe` _OK

            threadDelay 120000000
            eresp <- getTransactionIndex wc (Just (walId wallet)) (Just (accIndex toAcct)) Nothing
            resp <- fmap wrData eresp `mustBe` _OK

            map txId resp `shouldContain` [txId txn]
            let txnEntry: _ = filter ( \x -> (txId x) == (txId txn)) resp
            log $ "Resp   : " <> ppShowT txnEntry
            txConfirmations txnEntry `shouldNotBe` 0

        it "sending from asset-locked address in wallet with no ther addresses gets 0 confirmations from core nodes" $ do
            genesis <- genesisAssetLockedWallet wc
            (fromAcct, _) <- firstAccountAndId wc genesis

            wallet <- sampleWallet wRef wc
            (toAcct, toAddr) <- firstAccountAndId wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = tenthOf (accAmount fromAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                tenthOf (V1 c) = V1 (Core.mkCoin (Core.getCoin c `div` 10))

            etxn <- postTransaction wc payment

            txn <- fmap wrData etxn `shouldPrism` _Right

            threadDelay 120000000
            eresp <- getTransactionIndex wc (Just (walId wallet)) (Just (accIndex toAcct)) Nothing
            resp <- fmap wrData eresp `shouldPrism` _Right :: IO [ Transaction]
            let txnEntry : _ = filter ( \x -> (txId x) == (txId txn)) resp
            log $ "Resp   : " <> ppShowT txnEntry
            txConfirmations txnEntry `shouldBe` 0

        it "estimate fees of a well-formed transaction" $ do
            ws <- (,)
                <$> (randomCreateWallet >>= createWalletCheck wc)
                <*> (randomCreateWallet >>= createWalletCheck wc)

            ((fromAcct, _), (_toAcct, toAddr)) <- (,)
                <$> getFirstAccountAndAddress wc (fst ws)
                <*> getFirstAccountAndAddress wc (snd ws)

            let amount = V1 (Core.mkCoin 42)

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

            efee <- getTransactionFee wc payment
            case efee of
                Right fee ->
                    feeEstimatedAmount (wrData fee)
                        `shouldSatisfy`
                            (> amount)
                Left (ClientWalletError (NotEnoughMoney _)) ->
                    pure ()
                Left err ->
                    expectationFailure $
                        "Expected either a successful fee or a NotEnoughMoney "
                        <> " error, got: "
                        <> show err

        it "fails if you spend too much money" $ do
            wallet <- sampleWallet wRef wc
            (toAcct, toAddr) <- getFirstAccountAndAddress wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId wallet
                        , psAccountIndex = accIndex toAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = tooMuchCash (accAmount toAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                tooMuchCash (V1 c) = V1 (Core.mkCoin (Core.getCoin c * 2))
            etxn <- postTransaction wc payment

            void $ etxn `mustBe` _Failed

        it "create unsigned transaction and submit it to the blockchain" $ do
            -- create genesis wallet, it is initial source of money,
            -- we will use it to send money to the source wallet before test payment
            genesisWallet <- makeGenesisWallet wc
            (genesisAccount, _) <- getFirstAccountAndAddress wc genesisWallet

            -- create a keys for the source wallet
            (srcWalletSecretKey, srcWalletEncSecretKey, srcWalletPublicKey) <- makeWalletKeys
            -- create external wallet, the source of test payment
            (srcExtWallet, defaultSrcAccount) <- makeExternalWalletBasedOn srcWalletPublicKey

            -- create and store new address for source wallet,
            -- we need it to send money from genesis wallet, before test payment
            srcWalletAddress <- makeAddressAndStoreIt srcWalletPublicKey
                                                      srcWalletEncSecretKey
                                                      srcExtWallet
                                                      defaultSrcAccount
            -- send some money to source wallet
            let initAmountInLovelaces = 1000000000
                initPayment = makePayment genesisWallet
                                          genesisAccount
                                          srcWalletAddress
                                          initAmountInLovelaces
            txResponse <- postTransaction wc initPayment
            void $ txResponse `mustBe` _OK

            -- now source wallet contains some money
            --srcExtWalletBalance <- getWalletBalanceInLovelaces wc srcExtWallet
            --srcExtWalletBalance `shouldBe` initAmountInLovelaces

            -- create another external wallet, the destination of test payment
            (_dstWalletSecretKey, dstWalletEncSecretKey, dstWalletPublicKey) <- makeWalletKeys
            (dstExtWallet, defaultDstAccount) <- makeExternalWalletBasedOn dstWalletPublicKey

            -- create and store new address for destination wallet,
            -- we need it to send money from source wallet
            dstWalletAddress <- makeAddressAndStoreIt dstWalletPublicKey
                                                      dstWalletEncSecretKey
                                                      dstExtWallet
                                                      defaultDstAccount

            --
            srcWalletChangeAddress <- makeAddressAndStoreIt srcWalletPublicKey
                                                            srcWalletEncSecretKey
                                                            srcExtWallet
                                                            defaultSrcAccount
            -- test payment
            let testAmountInLovelaces = 100000000
                testPayment = makePayment srcExtWallet
                                          defaultSrcAccount
                                          dstWalletAddress
                                          testAmountInLovelaces
                testPaymentWithChangeAddress = PaymentWithChangeAddress testPayment
                                                                        (Core.addrToBase58Text srcWalletChangeAddress)

            rawTxResponse <- postUnsignedTransaction wc testPaymentWithChangeAddress
            rawTx <- rawTxResponse `mustBe` _OK

            -- now we have a raw transaction, but it wasn't piblished yet,
            -- let's sign it (as if Ledger device did it)
            let (RawTransaction txInHexFormat) = wrData rawTx
                (Right txSerialized) = B16.decode txInHexFormat
                (Right (tx :: Core.Tx)) = decodeFull' txSerialized
                txHash = hash tx
                protocolMagic = Core.ProtocolMagic 125 -- Some random value, it's just for test cluster.
                (Signature txSignature) = sign protocolMagic SignTx srcWalletSecretKey txHash
                rawSignature = CC.unXSignature txSignature
                txSignatureInHexFormat = B16.encode rawSignature
                srcWalletPublicKeyAsBase58 = encodeBase58PublicKey srcWalletPublicKey
                signedTx = SignedTransaction srcWalletPublicKeyAsBase58
                                             txInHexFormat
                                             txSignatureInHexFormat

            -- now we have signed transaction, let's publish it in the blockchain
            signedTxResponse <- postSignedTransaction wc signedTx
            void $ signedTxResponse `mustBe` _OK

            -- check current balance of destination wallet
            --dstExtWalletBalance <- getWalletBalanceInLovelaces wc dstExtWallet
            --dstExtWalletBalance `shouldBe` testAmountInLovelaces
  where
    makePayment srcWallet srcAccount dstAddress amount = Payment
        { pmtSource = PaymentSource
            { psWalletId = walId srcWallet
            , psAccountIndex = accIndex srcAccount
            }
        , pmtDestinations = pure PaymentDistribution
            { pdAddress = V1 dstAddress
            , pdAmount = V1 (Core.mkCoin amount)
            }
        , pmtGroupingPolicy = Nothing
        , pmtSpendingPassword = Nothing
        }

    makeAddressAndStoreIt _publicKey encSecretKey wallet anAccount = do
        -- we have to create HD address because we will sync this wallet
        -- with the blockchain to see its actual balance

        -- create HD secret key based on the 'encSecretKey'
        let passCheck = ShouldCheckPassphrase True
            accountIndex = accIndex anAccount
            (Just hdSecretKey) = deriveHDSecretKey passCheck
                                                   emptyPassphrase
                                                   encSecretKey
                                                   accountIndex
        -- derive public key from HD secret key
        let hdPublicKey = encToPublic hdSecretKey

        -- we need HD passphrase to read HD derivation path later,
        -- during synchronization with the blockchain
        addrIndex <- randomAddressIndex
        let hdPassphrase = deriveHDPassphrase hdPublicKey
            forBootstrapEra = Core.IsBootstrapEraAddr True
            (Just (anAddress, _)) = Core.createHDAddressH forBootstrapEra
                                                          passCheck
                                                          emptyPassphrase
                                                          hdPassphrase
                                                          hdSecretKey
                                                          [accountIndex]
                                                          addrIndex
            anAddressAsBase58 = Core.addrToBase58Text anAddress

        let addressCreatedFromThisPK = Core.checkPubKeyAddress hdPublicKey anAddress
        addressCreatedFromThisPK `shouldBe` True

        -- store this HD-address in the wallet's account
        storeResponse <- postStoreAddress wc
                                          (walId wallet)
                                          (accIndex anAccount)
                                          anAddressAsBase58
        void $ storeResponse `mustBe` _OK
        pure anAddress

    makeExternalWalletBasedOn publicKey = do
        newExtWallet <- randomExternalWalletWithPublicKey CreateWallet publicKey
        extWallet <- createExternalWalletCheck wc newExtWallet
        defaultAccount <- firstAccountInExtWallet wc extWallet
        pure (extWallet, defaultAccount)

    makeWalletKeys = do
        secretKey <- randomSecretKey
        let encSecretKey = noPassEncrypt secretKey
            publicKey    = encToPublic encSecretKey
        pure (secretKey, encSecretKey, publicKey)

    randomSecretKey :: IO SecretKey
    randomSecretKey = generate arbitrary

    randomAddressIndex :: IO Word32
    randomAddressIndex = generate arbitrary
