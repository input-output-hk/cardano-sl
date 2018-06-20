{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module TransactionSpecs (transactionSpecs) where

import           Universum
-- import qualified Serokell.Util.Base16 as B16

import           Cardano.Wallet.API.V1.Errors hiding (describe)
import           Cardano.Wallet.Client.Http
-- import           Pos.Binary.Class (decodeFull', serialize')
import qualified Pos.Core as Core
import           Pos.Crypto (SecretKey, {-SignTag (..),-} emptyPassphrase, encToPublic,
                             {-encodeBase58PublicKey,-} {-hash,-} noPassEncrypt, {-sign-})
import           Pos.Crypto.HD (ShouldCheckPassphrase (..),
                                deriveHDPassphrase, deriveHDSecretKey)

import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           Util


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
                        , pdAmount = halfOf (accAmount fromAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                halfOf (V1 c) = V1 (Core.mkCoin (Core.getCoin c `div` 2))

            etxn <- postTransaction wc payment

            txn <- fmap wrData etxn `mustBe` _OK

            eresp <- getTransactionIndex wc (Just (walId wallet)) (Just (accIndex toAcct)) Nothing
            resp <- fmap wrData eresp `mustBe` _OK

            map txId resp `shouldContain` [txId txn]

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
            (_srcWalletSecretKey, srcWalletEncSecretKey, srcWalletPublicKey) <- makeWalletKeys
            -- create external wallet, the source of test payment
            (srcExtWallet, defaultSrcAccount) <- makeExternalWalletBasedOn srcWalletPublicKey

            -- create and store new address for source wallet,
            -- we need it to send money from genesis wallet, before test payment
            srcWalletAddress <- makeAddressAndStoreIt srcWalletPublicKey
                                                      srcWalletEncSecretKey
                                                      srcExtWallet
                                                      defaultSrcAccount
            -- send some money to source wallet
            let initAmountInLovelaces = 1000000000 -- Don't worry, genesis wallet has much more money.
                initPayment = makePayment genesisWallet
                                          genesisAccount
                                          srcWalletAddress
                                          initAmountInLovelaces
            txResponse <- postTransaction wc initPayment
            void $ txResponse `mustBe` _OK

            -- now source wallet contains some money
            srcExtWalletBalance <- getWalletBalanceInLovelaces wc srcExtWallet
            srcExtWalletBalance `shouldBe` initAmountInLovelaces

            -- create another external wallet, the destination of test payment
            (_dstWalletSecretKey, dstWalletEncSecretKey, dstWalletPublicKey) <- makeWalletKeys
            (dstExtWallet, defaultDstAccount) <- makeExternalWalletBasedOn dstWalletPublicKey

            -- create and store new address for destination wallet,
            -- we need it to send money from source wallet
            dstWalletAddress <- makeAddressAndStoreIt dstWalletPublicKey
                                                      dstWalletEncSecretKey
                                                      dstExtWallet
                                                      defaultDstAccount
            -- test payment
            let testAmountInLovelaces =  100000000
                testPayment = makePayment srcExtWallet
                                          defaultSrcAccount
                                          dstWalletAddress
                                          testAmountInLovelaces
            rawTxResponse <- postUnsignedTransaction wc testPayment
            void $ rawTxResponse `mustBe` _OK
            --rawTx <- rawTxResponse `mustBe` _OK
            {-
            -- now we have a raw transaction, but it wasn't piblished yet,
            -- let's sign it (as if Ledger device did it)
            let (RawTransaction txInHexFormat) = wrData rawTx
                (Right txSerialized) = B16.decode txInHexFormat
                (Right (tx :: Core.Tx)) = decodeFull' txSerialized
                txHash = hash tx
                protocolMagic = Core.ProtocolMagic 125 -- Some random value, it's just for test cluster.
                txSignature = sign protocolMagic SignTx srcWalletSecretKey txHash
                txSignatureInHexFormat = B16.encode $ serialize' txSignature
                srcWalletPublicKeyAsBase58 = encodeBase58PublicKey srcWalletPublicKey
                signedTx = SignedTransaction srcWalletPublicKeyAsBase58
                                             txInHexFormat
                                             txSignatureInHexFormat

            -- now we have signed transaction, let's publish it in the blockchain
            signedTxResponse <- postSignedTransaction wc signedTx
            void $ signedTxResponse `mustBe` _OK

            -- check current balance of destination wallet
            dstExtWalletBalance <- getWalletBalanceInLovelaces wc dstExtWallet
            dstExtWalletBalance `shouldBe` testAmountInLovelaces
            -}
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

    makeAddressAndStoreIt publicKey encSecretKey wallet anAccount = do
        -- we have to create HD address because we will sync this wallet
        -- with the blockchain to see its actual balance

        -- create HD secret key based on the 'encSecretKey'.
        let noPassCheck = ShouldCheckPassphrase False
            accountIndex = accIndex anAccount
            (Just hdSecretKey) = deriveHDSecretKey noPassCheck
                                                   emptyPassphrase
                                                   encSecretKey
                                                   accountIndex

        -- we need HD passphrase to read HD derivation path later,
        -- during synchronization.
        let hdPassphrase = deriveHDPassphrase publicKey
            forBootstrapEra = Core.IsBootstrapEraAddr True
            accountDerivationIndex = accountIndex
            addressDerivationIndex = 0
            (Just (anAddress, _)) = Core.createHDAddressH forBootstrapEra
                                                          noPassCheck
                                                          emptyPassphrase
                                                          hdPassphrase
                                                          hdSecretKey
                                                          [accountDerivationIndex]
                                                          addressDerivationIndex
            anAddressAsBase58 = Core.addrToBase58Text anAddress

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
