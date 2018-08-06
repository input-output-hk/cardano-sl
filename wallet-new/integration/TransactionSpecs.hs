module TransactionSpecs (transactionSpecs) where

import qualified Serokell.Util.Base16 as B16
import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Cardano.Wallet.API.V1.Errors hiding (describe)
import           Cardano.Wallet.Client.Http
import qualified Pos.Core as Core
import           Pos.Crypto (EncryptedSecretKey, PublicKey, SecretKey, SignTag (..), Signature (..),
                             checkSigRaw, emptyPassphrase, encToPublic, encToSecret,
                             encodeBase58PublicKey, signEncoded)
import           Pos.Crypto.HD (ShouldCheckPassphrase (..))

import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           Util


transactionSpecs
     :: HasCallStack
     => (WalletClient IO, WalletClient IO, WalletClient IO, WalletClient IO)
     -> Spec
transactionSpecs (wc0, wc1, wc2, _wc3) = describe "Transactions" $ do
    it "Estimate fees of a well-formed transaction" $ estimateTransactionFees wc0
    before (createRandomSampleWallet wc0) $
        it "Fails if you spend too much money" $ spendTooMuchMoney wc0
    before ((,) <$> lookupGenesisWallet wc0 <*> createRandomSampleWallet wc0) $
        it "Posted transactions appear in the index" $ testPostTransaction wc0
    before (lookupGenesisWallet wc0) $
        it "Payment: genesis wallet -> external wallet -> external wallet" $ \genesisWallet ->
            makeExternalDestWallet wc0 FirstSK
              >>= payFromExternalWalletToExternal wc0 wc0 genesisWallet SecondSK
    before (lookupGenesisWallet wc0) $
        it "Payment: genesis wallet -> external wallet -> regular wallet" $ \genesisWallet ->
            makeRegularDestWallet wc0
              >>= payFromExternalWalletToExternal wc0 wc0 genesisWallet ThirdSK

    before (lookupGenesisWallet wc0) $
        xit "Payment: genesis wallet node0 -> external wallet node1 -> regular wallet node2"
            $ \genesisWallet ->
                makeRegularDestWallet wc2
                  >>= payFromExternalWalletToExternal wc0 wc1 genesisWallet FourthSK


testPostTransaction :: HasCallStack => WalletClient IO -> (Wallet, Wallet) -> IO ()
testPostTransaction wc (genesisWallet, wallet) = do
    (fromAcct, _) <- getFirstAccountAndAddress wc genesisWallet
    (toAcct, toAddr) <- getFirstAccountAndAddress wc wallet

    let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesisWallet
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

    txn <- fmap wrData $ shouldReturnRight $ postTransaction wc payment
    resp <- fmap wrData $ shouldReturnRight $
                         getTransactionIndex wc (Just (walId wallet)) (Just (accIndex toAcct)) Nothing

    map txId resp `shouldContain` [txId txn]

estimateTransactionFees :: HasCallStack => WalletClient IO -> IO ()
estimateTransactionFees wc = do
    wallet1 <- randomCreateWallet >>= createWalletCheck wc
    (fromAcct, _) <- getFirstAccountAndAddress wc wallet1
    (_toAcct, toAddr) <- randomCreateWallet >>= createWalletCheck wc >>= getFirstAccountAndAddress wc
    let amount = V1 (Core.mkCoin 42)

    let payment = Payment
                    { pmtSource = PaymentSource
                        { psWalletId = walId wallet1
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
        Left (ClientWalletError (NotEnoughMoney _)) -> pure ()

        Left err ->
                    expectationFailure $
                        "Expected either a successful fee or a NotEnoughMoney "
                        <> " error, got: "
                        <> show err

spendTooMuchMoney :: HasCallStack => WalletClient IO -> Wallet -> IO ()
spendTooMuchMoney wc wallet = do
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

payFromExternalWalletToExternal
  :: HasCallStack
  => WalletClient IO
  -> WalletClient IO
  -> Wallet
  -> WhichSK
  -> (Wallet, Address)
  -> IO ()
payFromExternalWalletToExternal clientA clientB genesisWallet key (dstExtWallet, dstWalletAddress) = do
    -- Create genesis wallet, it is initial source of money,
    -- we will use it to send money to the source wallet before test payment.
    (genesisAccount, _) <- getFirstAccountAndAddress clientA genesisWallet
     -- Create a key for the source wallet.
    let (srcWalletEncRootSK, srcWalletRootPK) = makeWalletRootKeys key

    -- Create and store new address for source wallet,
    -- we need it to send money from genesis wallet, before test payment.
    ( srcWalletAddress
      , srcWalletAddressDerivedSK
      , srcWalletAddressDerivedPK ) <- makeFirstAddress srcWalletEncRootSK
    -- Create external wallet, the source of test payment.
    (srcExtWallet, defaultSrcAccount) <- makeExternalWalletBasedOn clientA srcWalletRootPK
    storeAddressInWalletAccount clientA srcExtWallet defaultSrcAccount srcWalletAddress

    -- Most likely that we'll have some change after test payment
    -- (if test payment's amount is smaller that 'srcExtWallet' balance),
    -- so we must provide change address for it.
    srcWalletChangeAddress <- makeAnotherAddress srcWalletEncRootSK defaultSrcAccount
    storeAddressInWalletAccount clientA srcExtWallet defaultSrcAccount srcWalletChangeAddress

    -- Send some money to source wallet.
    let initAmountInLovelaces = 1000000
        initPayment = makePayment genesisWallet
                                  genesisAccount
                                  srcWalletAddress
                                  initAmountInLovelaces
    void $ shouldReturnRight $ postTransaction clientA initPayment

    -- Now source wallet contains some money.
    srcExtWalletBalance <- getWalletBalanceInLovelaces clientB srcExtWallet
    srcExtWalletBalance `shouldSatisfy` (> 0)

    -- Test payment from one external wallet to another wallet.
    makePaymentFromExternalWallet clientB
                                  10000
                                  srcExtWallet
                                  defaultSrcAccount
                                  dstExtWallet
                                  dstWalletAddress
                                  srcWalletChangeAddress
                                  srcWalletAddress
                                  srcWalletAddressDerivedSK
                                  srcWalletAddressDerivedPK
                                  srcWalletRootPK

makeExternalDestWallet :: HasCallStack => WalletClient IO -> WhichSK -> IO (Wallet, Address)
makeExternalDestWallet wc key = do
    -- Create another external wallet, the destination of test payment.
    let (dstWalletEncRootSK, dstWalletRootPK) = makeWalletRootKeys key

    -- Create and store new address for destination wallet,
    -- we need it to send money from source wallet.
    (dstWalletAddress, _, _) <- makeFirstAddress dstWalletEncRootSK
    (dstExtWallet, defaultDstAccount) <- makeExternalWalletBasedOn wc dstWalletRootPK
    storeAddressInWalletAccount wc dstExtWallet defaultDstAccount dstWalletAddress
    return (dstExtWallet, dstWalletAddress)

makeRegularDestWallet :: HasCallStack => WalletClient IO -> IO (Wallet, Address)
makeRegularDestWallet wc = do
    -- Create destination regular wallet.
    newDstRegularWallet <- randomCreateWallet
    dstRegularWallet <- createWalletCheck wc newDstRegularWallet
    (_, dstRegularWalletAddress) <- getFirstAccountAndAddress wc dstRegularWallet
    return (dstRegularWallet, unV1 . addrId $ dstRegularWalletAddress)

makePayment :: Wallet -> Account -> Address -> Word64 -> Payment
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


makeFirstAddress :: HasCallStack => EncryptedSecretKey -> IO (Address, SecretKey, PublicKey)
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


makeAnotherAddress :: HasCallStack => EncryptedSecretKey -> Account -> IO Address
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


storeAddressInWalletAccount :: HasCallStack => WalletClient IO -> Wallet -> Account -> Address -> IO ()
storeAddressInWalletAccount wc wallet anAccount anAddress = do
    -- Store this HD-address in the wallet's account.
    let anAddressAsBase58 = Core.addrToBase58Text anAddress
    void $ shouldReturnRight $ postStoreAddress wc
                                                (walId wallet)
                                                (accIndex anAccount)
                                                anAddressAsBase58

randomAddressIndex :: IO Word32
randomAddressIndex = generate arbitrary


makePaymentFromExternalWallet
     :: HasCallStack
     => WalletClient IO
     -> Word64
     -> Wallet
     -> Account
     -> Wallet
     -> Address
     -> Address
     -> Address
     -> SecretKey
     -> PublicKey
     -> PublicKey
     -> IO ()

makePaymentFromExternalWallet
        wc
        amountInLovelaces
        srcWallet
        srcAccount
        dstWallet
        dstAddress
        changeAddress
        fromAddress
        derivedSK
        derivedPK
        rootPK = do
    let testPayment = makePayment srcWallet
                                  srcAccount
                                  dstAddress
                                  amountInLovelaces
        changeAddressAsBase58 = Core.addrToBase58Text changeAddress
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
        -- Value of protocolMagic is taken from lib/configuration.yaml.
        protocolMagic = Core.ProtocolMagic 55550001
        Signature txSignature = signEncoded protocolMagic
                                            SignTx
                                            derivedSK
                                            txSigDataAsBytes
        rawSignature = CC.unXSignature txSignature
        txSignatureInHexFormat = B16.encode rawSignature
        srcWalletRootPKAsBase58 = encodeBase58PublicKey rootPK
        derivedPKAsBase58 = encodeBase58PublicKey derivedPK
        srcWalletAddressAsBase58 = Core.addrToBase58Text fromAddress
        inputProof = AddressWithProof srcWalletAddressAsBase58
                                      txSignatureInHexFormat
                                      derivedPKAsBase58
        signedTx = SignedTransaction srcWalletRootPKAsBase58
                                     txInHexFormat
                                     [inputProof]
        addressCreatedFromThisPK = Core.checkPubKeyAddress derivedPK
                                                           fromAddress
        txSignatureIsValid = checkSigRaw protocolMagic
                                         (Just SignTx)
                                         derivedPK
                                         txSigDataAsBytes
                                         (Signature txSignature)

    addressCreatedFromThisPK `shouldBe` True
    txSignatureIsValid `shouldBe` True

    -- Now we have signed transaction, let's publish it in the blockchain.
    void $ shouldReturnRight $ postSignedTransaction wc signedTx
     -- Check current balance of destination wallet.
    dstBalance <- getWalletBalanceInLovelaces wc dstWallet
    -- Test payment.
    dstBalance `shouldSatisfy` (> 0)
