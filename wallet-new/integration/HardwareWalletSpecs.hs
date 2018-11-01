{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module HardwareWalletSpecs
   ( hardwareWalletSpecs
   , postUnsignedTransactionTest
   , signAndPostTransactionTest
   , uniqueNewWallet
   , Seed
   )
where
import qualified Data.ByteArray (convert)
import qualified Data.ByteString as BS
import           Test.Hspec
import           Universum


import           Cardano.Wallet.API.V1.Types (AssuranceLevel (..),
                     BackupPhrase (..), NewWallet (..), WalletOperation (..),
                     unBackupPhrase)
import           Cardano.Wallet.Client.Http (AddressWithProof (..),
                     Payment (..), PaymentDistribution (..),
                     PaymentSource (..), SignedTransaction (..), Transaction,
                     UnsignedTransaction (..), V1 (V1), WalletClient, accIndex,
                     addrId, mkAddressAsBase58, mkPublicKeyAsBase58,
                     mkTransactionSignatureAsBase16, pdAddress, pdAmount,
                     pmtDestinations, pmtGroupingPolicy, pmtSource,
                     pmtSpendingPassword, psAccountIndex, psWalletId,
                     rawTransactionAsBase16, walId)
import qualified Cardano.Wallet.Client.Http as Http (postSignedTransaction,
                     postUnsignedTransaction, postWallet)

import qualified Cardano.Wallet.Kernel.BIP39 as BIP39

import           Pos.Core as Core
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Crypto (PublicKey, SecretKey, ShouldCheckPassphrase (..),
                     SignTag (SignTx), Signature (..), checkSigRaw,
                     emptyPassphrase, encToSecret, firstHardened, hash,
                     safeDeterministicKeyGen, signEncoded, toPublic)
import           Pos.Crypto.Configuration (ProtocolMagic)
import qualified Pos.Crypto.Hashing (hash)
import           Pos.Util.Log.LoggerConfig (defaultTestConfiguration)
import           Pos.Util.Mnemonic (entropyToMnemonic, mkEntropy)
import           Pos.Util.Wlog (Severity (Debug), setupLogging)
import qualified Serokell.Util.Base16 (decode)

import           Util (chargeWallet, firstAccountAndId, shouldReturnData)

type Seed = Integer

hardwareWalletSpecs :: ProtocolMagic -> Seed -> WalletClient IO -> Spec
hardwareWalletSpecs protocolMagic seed wc
    = beforeAll_ (setupLogging "wallet-newHardwareWalletSpecs" (defaultTestConfiguration Debug)) $
    describe "HardwareWallets (External Wallets)" $ do
        it "create a new unsigned transaction"
            $ void $ postUnsignedTransactionTest seed wc "srcWallet1" "dstWallet1"
        it "submit an externally signed transaction to the blockchain" $ void
            ( postUnsignedTransactionTest seed wc "srcWallet2" "dstWallet2"
                        >>= signAndPostTransactionTest protocolMagic wc)

postUnsignedTransactionTest
    :: Seed
    -> WalletClient IO
    -> Text
    -> Text
    -> IO (NewWallet, UnsignedTransaction)
postUnsignedTransactionTest seed wc sourceName destName = do
    let
        newSourceWallet :: NewWallet
        newSourceWallet = uniqueNewWallet seed sourceName
    sourceWallet <- shouldReturnData $ Http.postWallet wc newSourceWallet
    (sourceAccount, _sourceAddr) <- chargeWallet wc (Core.mkCoin 2000000) sourceWallet
    destWallet <- shouldReturnData $ Http.postWallet wc $ uniqueNewWallet seed destName

    (_toAcct, toAddr) <- firstAccountAndId wc destWallet

    let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId sourceWallet
                        , psAccountIndex = accIndex sourceAccount
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = V1 (Core.mkCoin 1000000)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

    unsignedTx <- shouldReturnData $ Http.postUnsignedTransaction wc payment
    return (newSourceWallet, unsignedTx)

signAndPostTransactionTest
    :: ProtocolMagic
    -> WalletClient IO
    -> (NewWallet, UnsignedTransaction)
    -> IO Transaction
signAndPostTransactionTest
    protocolMagic wc (fromWallet, UnsignedTransaction txInHexFormat _txSigDataInHexFormat)
    = do
    let
        fromAddress :: Address
        secretKey :: SecretKey
        (fromAddress, secretKey) = firstAddressSecretKey protocolMagic fromWallet
        publicKey :: PublicKey
        publicKey = toPublic secretKey

        transactionData :: ByteString
        transactionData = Data.ByteArray.convert $ Pos.Crypto.hash
                             $ fromRight (error "cannot decode transaction")
                             $ Serokell.Util.Base16.decode $ rawTransactionAsBase16 txInHexFormat
        signature :: Signature a
        signature = Pos.Crypto.signEncoded
                           protocolMagic
                           SignTx
                           secretKey
                           transactionData

        inputProof = AddressWithProof (mkAddressAsBase58 fromAddress)
                                      (mkTransactionSignatureAsBase16 signature)
                                      (mkPublicKeyAsBase58 publicKey)

        signedTx = SignedTransaction txInHexFormat [inputProof]

-- We have to check address and signature.
    Core.checkPubKeyAddress publicKey fromAddress `shouldBe` True
    let
        isTxSignatureValid = checkSigRaw protocolMagic
                                                 (Just SignTx)
                                                 publicKey
                                                 transactionData
                                                 signature

    isTxSignatureValid `shouldBe` True

            -- Now we have signed transaction, let's publish it in the blockchain.
            --void $ shouldReturnRight $ postSignedTransaction wc signedTx
    shouldReturnData $ Http.postSignedTransaction wc signedTx

uniqueNewWallet :: Seed -> Text -> NewWallet
uniqueNewWallet seed name
    = NewWallet {
      newwalBackupPhrase     = backupPhrase
    , newwalSpendingPassword = Nothing
    , newwalAssuranceLevel   = NormalAssurance
    , newwalName             = name
    , newwalOperation        = CreateWallet
    }
    where
        (Right backupPhrase) = (BackupPhrase . entropyToMnemonic)
                                <$> (mkEntropy $ BS.take 16
                                      $ Data.ByteArray.convert $ Pos.Crypto.Hashing.hash (name,seed))

firstAddressSecretKey :: ProtocolMagic -> NewWallet -> (Address, SecretKey)
firstAddressSecretKey protocolMagic wallet = case derivedAddr of
    Nothing        -> error "cannot derive addr"
    Just (addr,sk) -> (addr, encToSecret sk)
    where
        accountESK = snd $ safeDeterministicKeyGen
                      (BIP39.mnemonicToSeed $ unBackupPhrase $ newwalBackupPhrase wallet) spendingPassword
        spendingPassword = case newwalSpendingPassword wallet of
                             Nothing -> emptyPassphrase
                             Just _  -> error "todo: spendingPassword not supported"
        derivedAddr = deriveLvl2KeyPair
                        (Pos.Core.NetworkMagic.makeNetworkMagic protocolMagic)
                        (IsBootstrapEraAddr True)
                        (ShouldCheckPassphrase True)
                        spendingPassword
                        accountESK
                        firstHardened
                        firstHardened
