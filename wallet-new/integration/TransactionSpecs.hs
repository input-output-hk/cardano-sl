module TransactionSpecs (transactionSpecs) where

import           Universum
import qualified Serokell.Util.Base16 as B16

import qualified Cardano.Crypto.Wallet as CC
import           Cardano.Wallet.API.V1.Errors hiding (describe)
import           Cardano.Wallet.Client.Http
import qualified Pos.Core as Core
import           Pos.Crypto (SecretKey, SignTag (..), Signature (..), emptyPassphrase,
                             encToPublic, encToSecret, encodeBase58PublicKey,
                             noPassEncrypt, signEncoded, checkSigRaw)
import           Pos.Crypto.HD (ShouldCheckPassphrase (..))

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
        it "Posted transactions appear in the index" $ do
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

            txn <- fmap wrData $ shouldReturnRight $ postTransaction wc payment

            threadDelay 120000000
            resp <- fmap wrData $ shouldReturnRight $
                         getTransactionIndex wc (Just (walId wallet)) (Just (accIndex toAcct)) Nothing

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

        it "Estimate fees of a well-formed transaction" $ do
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

        it "Fails if you spend too much money" $ do
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
            shouldFail $ postTransaction wc payment

        it "Create unsigned transaction and submit it to the blockchain" $ do
            -- Create genesis wallet, it is initial source of money,
            -- we will use it to send money to the source wallet before test payment.
            genesisWallet <- makeGenesisWallet wc
            (genesisAccount, _) <- getFirstAccountAndAddress wc genesisWallet

            -- Create a keys for the source wallet.
            (srcWalletEncRootSK, srcWalletRootPK) <- makeWalletRootKeys

            -- Create and store new address for source wallet,
            -- we need it to send money from genesis wallet, before test payment.
            ( srcWalletAddress
              , srcWalletAddressDerivedSK
              , srcWalletAddressDerivedPK ) <- makeFirstAddress srcWalletEncRootSK
            -- Create external wallet, the source of test payment.
            (srcExtWallet, defaultSrcAccount) <- makeExternalWalletBasedOn srcWalletRootPK
            storeAddressInWalletAccount srcExtWallet defaultSrcAccount srcWalletAddress

            -- Most likely that we'll have some change after test payment
            -- (if test payment's amount is smaller that 'srcExtWallet' balance),
            -- so we must provide change address for it.
            srcWalletChangeAddress <- makeAnotherAddress srcWalletEncRootSK defaultSrcAccount
            storeAddressInWalletAccount srcExtWallet defaultSrcAccount srcWalletChangeAddress

            -- Send some money to source wallet.
            let initAmountInLovelaces = 1000000000
                initPayment = makePayment genesisWallet
                                          genesisAccount
                                          srcWalletAddress
                                          initAmountInLovelaces
            void $ shouldReturnRight $ postTransaction wc initPayment

            -- Now source wallet contains some money.
            srcExtWalletBalance <- getWalletBalanceInLovelaces wc srcExtWallet
            srcExtWalletBalance `shouldSatisfy` (> 0)

            -- Create another external wallet, the destination of test payment.
            (dstWalletEncRootSK, dstWalletRootPK) <- makeWalletRootKeys

            -- Create and store new address for destination wallet,
            -- we need it to send money from source wallet.
            (dstWalletAddress, _, _) <- makeFirstAddress dstWalletEncRootSK
            (dstExtWallet, defaultDstAccount) <- makeExternalWalletBasedOn dstWalletRootPK
            storeAddressInWalletAccount dstExtWallet defaultDstAccount dstWalletAddress

            -- Test payment.
            let testAmountInLovelaces = 100000000
                testPayment = makePayment srcExtWallet
                                          defaultSrcAccount
                                          dstWalletAddress
                                          testAmountInLovelaces
                changeAddressAsBase58 = Core.addrToBase58Text srcWalletChangeAddress
                testPaymentWithChangeAddress = PaymentWithChangeAddress testPayment changeAddressAsBase58

            rawTx <- shouldReturnRight $ postUnsignedTransaction wc testPaymentWithChangeAddress

            -- Now we have a raw transaction, but it wasn't piblished yet,
            -- let's sign it (as if Ledger device did it).
            -- There's only one input for this transaction, so we should provide
            -- only one proof for this input.
            let RawTransaction txInHexFormat txSigDataInHexFormat _ = wrData rawTx
                -- txSigDataInHexFormat is a hash data of this transaction, this
                -- hash data should be signed by derivedSK.
                Right txSigDataAsBytes = B16.decode txSigDataInHexFormat
                protocolMagic = Core.ProtocolMagic 55550001
                Signature txSignature = signEncoded protocolMagic
                                                    SignTx
                                                    srcWalletAddressDerivedSK
                                                    txSigDataAsBytes
                rawSignature = CC.unXSignature txSignature
                txSignatureInHexFormat = B16.encode rawSignature
                srcWalletRootPKAsBase58 = encodeBase58PublicKey srcWalletRootPK
                derivedPKAsBase58 = encodeBase58PublicKey srcWalletAddressDerivedPK
                srcWalletAddressAsBase58 = Core.addrToBase58Text srcWalletAddress
                inputProof = AddressWithProof srcWalletAddressAsBase58
                                              txSignatureInHexFormat
                                              derivedPKAsBase58
                signedTx = SignedTransaction srcWalletRootPKAsBase58
                                             txInHexFormat
                                             [inputProof]
                addressCreatedFromThisPK = Core.checkPubKeyAddress srcWalletAddressDerivedPK
                                                                   srcWalletAddress
                txSignatureIsValid = checkSigRaw protocolMagic
                                                 (Just SignTx)
                                                 srcWalletAddressDerivedPK
                                                 txSigDataAsBytes
                                                 (Signature txSignature)

            addressCreatedFromThisPK `shouldBe` True
            txSignatureIsValid `shouldBe` True

            -- Now we have signed transaction, let's publish it in the blockchain.
            void $ shouldReturnRight $ postSignedTransaction wc signedTx

            -- Check current balance of destination wallet.
            dstExtWalletBalance <- getWalletBalanceInLovelaces wc dstExtWallet
            dstExtWalletBalance `shouldSatisfy` (> 0)
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

    makeFirstAddress encSecretKey = do
        -- We have to create HD address because we will sync this wallet
        -- with the blockchain to see its actual balance.
        let forBootstrapEra = Core.IsBootstrapEraAddr True
            Just (anAddress, derivedEncSK) =
                Core.deriveFirstHDAddress forBootstrapEra
                                          emptyPassphrase
                                          encSecretKey
            derivedPK = encToPublic derivedEncSK
            addressCreatedFromThisPK = Core.checkPubKeyAddress derivedPK anAddress

        addressCreatedFromThisPK `shouldBe` True

        pure (anAddress, encToSecret derivedEncSK, derivedPK)

    makeAnotherAddress encSecretKey anAccount = do
        addrIndex <- randomAddressIndex
        let forBootstrapEra = Core.IsBootstrapEraAddr True
            passCheck = ShouldCheckPassphrase True
            Just (anAddress, derivedEncSecretKey) =
                Core.deriveLvl2KeyPair forBootstrapEra
                                       passCheck
                                       emptyPassphrase
                                       encSecretKey
                                       (accIndex anAccount)
                                       addrIndex
            derivedPublicKey = encToPublic derivedEncSecretKey
            addressCreatedFromThisPK = Core.checkPubKeyAddress derivedPublicKey anAddress

        addressCreatedFromThisPK `shouldBe` True

        pure anAddress

    storeAddressInWalletAccount wallet anAccount anAddress = do
        -- Store this HD-address in the wallet's account.
        let anAddressAsBase58 = Core.addrToBase58Text anAddress
        void $ shouldReturnRight $ postStoreAddress wc
                                                    (walId wallet)
                                                    (accIndex anAccount)
                                                    anAddressAsBase58

    makeExternalWalletBasedOn publicKey = do
        newExtWallet <- randomExternalWalletWithPublicKey CreateWallet publicKey
        extWallet <- createExternalWalletCheck wc newExtWallet
        defaultAccount <- firstAccountInExtWallet wc extWallet
        pure (extWallet, defaultAccount)

    makeWalletRootKeys = do
        rootSK <- randomSK
        let encRootSK = noPassEncrypt rootSK
            rootPK    = encToPublic encRootSK
        pure (encRootSK, rootPK)

    randomSK :: IO SecretKey
    randomSK = generate arbitrary

    randomAddressIndex :: IO Word32
    randomAddressIndex = generate arbitrary
