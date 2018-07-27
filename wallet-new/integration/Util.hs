{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Util where

import qualified Serokell.Util.Base16 as B16
import           Universum

import           Cardano.Wallet.Client.Http
import           Test.Hspec (expectationFailure, shouldSatisfy)
import           Test.QuickCheck (Gen, arbitrary, generate)

import           Pos.Binary.Class (decodeFull')
import qualified Pos.Core as Core
import           Pos.Crypto.Signing (EncryptedSecretKey, PublicKey, encToPublic,
                                     encodeBase58PublicKey, noPassEncrypt)
import           Pos.Util.BackupPhrase (safeKeysFromPhrase)

type WalletRef = MVar Wallet

randomWallet :: WalletOperation -> IO NewWallet
randomWallet walletOp =
    generate $
        NewWallet
            <$> arbitrary
            <*> pure Nothing
            <*> arbitrary
            <*> pure "Wallet"
            <*> pure walletOp

randomCreateWallet :: IO NewWallet
randomCreateWallet = randomWallet CreateWallet

randomRestoreWallet :: IO NewWallet
randomRestoreWallet = randomWallet RestoreWallet

randomExternalWallet :: HasCallStack => WalletOperation -> IO NewExternalWallet
randomExternalWallet walletOp =
    generate $
        NewExternalWallet
            <$> (encodeBase58PublicKey <$> arbitraryExtPubKey)
            <*> arbitrary
            <*> pure "External Wallet"
            <*> pure walletOp
  where
    arbitraryExtPubKey :: Gen PublicKey
    arbitraryExtPubKey =
        (encToPublic . fst . orFail . safeKeysFromPhrase mempty) <$> arbitrary

    orFail :: (Show e) => Either e a -> a
    orFail =
        either (error . show) identity

randomExternalWalletWithPublicKey :: WalletOperation -> PublicKey -> IO NewExternalWallet
randomExternalWalletWithPublicKey walletOp publicKey =
    generate $
        NewExternalWallet
            <$> pure (encodeBase58PublicKey publicKey)
            <*> arbitrary
            <*> pure "External Wallet"
            <*> pure walletOp

createWalletCheck :: HasCallStack => WalletClient IO -> NewWallet -> IO Wallet
createWalletCheck wc newWallet
  = shouldReturnRight $ fmap wrData <$> postWallet wc newWallet

createExternalWalletCheck :: HasCallStack => WalletClient IO -> NewExternalWallet -> IO Wallet
createExternalWalletCheck wc newExtWallet
    = shouldReturnRight $ fmap wrData <$> postExternalWallet wc newExtWallet

getFirstAccountAndAddress :: HasCallStack => WalletClient IO -> Wallet -> IO (Account, WalletAddress)
getFirstAccountAndAddress wc wallet = do
    accounts <- getAccountsInWallet wc wallet
    let (fstAccount : _) = accounts
        (fstAddress : _) = accAddresses fstAccount
    pure (fstAccount, fstAddress)

firstAccountInExtWallet :: HasCallStack => WalletClient IO -> Wallet -> IO Account
firstAccountInExtWallet wc wallet = do
    accounts <- getAccountsInWallet wc wallet
    let (fstAccount : _) = accounts
    pure fstAccount

getAccountsInWallet :: HasCallStack => WalletClient IO -> Wallet -> IO [Account]
getAccountsInWallet wc wallet = do
    accounts <- fmap wrData $ shouldReturnRight $ getAccounts wc (walId wallet)
    accounts `shouldSatisfy` (not . null)
    return accounts

-- | We need just a few random keys, but they must be different.
data WhichSK
    = FirstSK
    | SecondSK
    | ThirdSK
    | FourthSK
    | FifthSK

makeWalletRootKeys :: WhichSK -> (EncryptedSecretKey, PublicKey)
makeWalletRootKeys skNumber =
    let Right skSerialized = B16.decode $ hexEncodedSKs !! toIndex skNumber
        Right rootSK = decodeFull' skSerialized
        encRootSK = noPassEncrypt rootSK
        rootPK = encToPublic encRootSK
    in (encRootSK, rootPK)
  where
    toIndex FirstSK  = 0
    toIndex SecondSK = 1
    toIndex ThirdSK  = 2
    toIndex FourthSK = 3
    toIndex FifthSK  = 4

    hexEncodedSKs :: [Text]
    hexEncodedSKs =
        [ "588050d9b69d2ccb4c61fabbd1e5cb9831320e884a13bbdb9cb48de39a04a748e64edef294b6ca10e97e2be10891b62de80852c81689b7e645b530333ffdaf16ce14edc1d053732591b8d0fda116adf4c299a5c8675eeb6cd51e27219c7c62b488fec3df4c8b129e0b04358b60066595351fb47a46cc2aa7ab3caf24ee5a597cd6a7"
        , "588070ed286e16e63e1ddaf8f62c7bb90a78fa63240f382d2854f98632413b47fc47538bea23d5f1fcae82d59e16c02bf3f8919a3eff60051ae33d7c370c76e562c4d4e410ea3acef31ccfcb26e02c4939bee51ca02087e1f76cde4e6baa7c44ae09516fb12219029aef84cff63c740131c2a94883f2e2ceea93fccf7d7e737f7797"
        , "5880b821a903476b248919392d09214c131e7682c925ce5ca59305b3f37a803cfa5ee4ff29da485d24456df0f8ed652ea547a4bd07b4e0f9634240819e12987ed2522981b1dd1a385c0929221b7f80309a9d90c9c04ff990cba7f78392e1edf9854539ca28313941322329d9bdede7000d5c5da018c390c3c45ec06db5dce19b5e2b"
        , "588098ecf760f607ea429d159b56047a7ff2bb1c68506e878327767ec87ab14ccb501814c580a8a77a828bc1ab9c30b8b4cc365c744c963b0f2e752970797e939baff062ffce3855aa071615f591512e01a0722f6955631c1f7fff752cf30266236576099d8357877c38a4eb0e608f9aa2e3e3ec71fb3d40cd42d4e72df8d3f7f3a7"
        , "5880d84181870133801d2447cbe53ab73ffb993402f9955fd4640e896a200c3a5b431494b739a81020dff394dbe327a3b6888cc452d716e7d3dcdeeb854b267f390e22c39a59b25a7fc4c9ab1697e8dc184d2cab98b469501dc0debf91999126469afb51e8d0dd4556dbc3f2423452bcc969f92f3484d32c875b50c467b4b1151f75"
        ]

makeExternalWalletBasedOn :: WalletClient IO -> PublicKey -> IO (Wallet, Account)
makeExternalWalletBasedOn wc publicKey = do
    newExtWallet <- randomExternalWalletWithPublicKey CreateWallet publicKey
    extWallet <- createExternalWalletCheck wc newExtWallet
    defaultAccount <- firstAccountInExtWallet wc extWallet
    pure (extWallet, defaultAccount)

newWalletRef :: IO WalletRef
newWalletRef = newEmptyMVar

sampleWallet :: WalletRef -> WalletClient IO -> IO Wallet
sampleWallet wRef wc = do
    mwallet <- tryTakeMVar wRef
    case mwallet of
        Just wallet -> do
            putMVar wRef wallet
            pure wallet
        Nothing -> do
            w <- randomWallet CreateWallet
            w' <- createWalletCheck wc w
            didWrite <- tryPutMVar wRef w'
            if didWrite
                then pure w'
                else readMVar wRef

genesisWallet :: WalletClient IO -> IO Wallet
genesisWallet wc = do
    Right allWallets <- fmap wrData <$> getWallets wc
    maybe
        (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
        return
        (find ((lockedWallet /=) . walId) allWallets)

-- Hard code the genesis wallet that's asset locked
genesisAssetLockedWallet :: WalletClient IO -> IO Wallet
genesisAssetLockedWallet wc = do
    Right allWallets <- fmap wrData <$> getWallets wc
    maybe
        (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
        return
        (find ((lockedWallet ==) . walId) allWallets)

lockedWallet :: WalletId
lockedWallet =
    WalletId "Ae2tdPwUPEZ5YjF9WuDoWfCZLPQ56MdQC6CZa2VKwMVRVqBBfTLPNcPvET4"

getWalletBalanceInLovelaces :: WalletClient IO -> Wallet -> IO Word64
getWalletBalanceInLovelaces wc wallet = do
    sameWallet <- shouldReturnRight $ getWallet wc (walId wallet)
    pure . Core.getCoin . unV1 . walBalance . wrData $ sameWallet

shouldReturnRight :: (HasCallStack, Show err) => IO (Either err a) -> IO a
shouldReturnRight a = a >>= \case
    Right x  -> return x
    Left err -> do
      expectationFailure ("expecting Right (..) got :" ++ show err)
      error "unreachable"

shouldFailWith
    :: HasCallStack
    => IO (Either ClientError a)
    -> ClientError
    -> IO ()
shouldFailWith action wantErr = action >>= \case
    Right _  -> expectationFailure errMsg
    Left err -> do
      if err == wantErr
        then return ()
        else expectationFailure errMsg
   where
     errMsg = "expecting Left ("++ show wantErr ++ ")"

shouldFail :: HasCallStack => IO (Either ClientError a) -> IO ()
shouldFail action = action >>= \case
    Right _  -> expectationFailure "expecting Left (..)"
    Left _   -> return ()

-- | We need just a few random keys, but they must be different.
data WhichSK
    = FirstSK
    | SecondSK
    | ThirdSK
    | FourthSK
    | FifthSK

makeWalletRootKeys :: WhichSK -> (EncryptedSecretKey, PublicKey)
makeWalletRootKeys skNumber =
    let Right skSerialized = B16.decode $ tohexEncodedSK skNumber
        Right rootSK = decodeFull' skSerialized
        encRootSK = noPassEncrypt rootSK
        rootPK = encToPublic encRootSK
    in (encRootSK, rootPK)
  where
    tohexEncodedSK FirstSK  = "588050d9b69d2ccb4c61fabbd1e5cb9831320e884a13bbdb9cb48de39a04a748e64edef294b6ca10e97e2be10891b62de80852c81689b7e645b530333ffdaf16ce14edc1d053732591b8d0fda116adf4c299a5c8675eeb6cd51e27219c7c62b488fec3df4c8b129e0b04358b60066595351fb47a46cc2aa7ab3caf24ee5a597cd6a7"
    tohexEncodedSK SecondSK = "588070ed286e16e63e1ddaf8f62c7bb90a78fa63240f382d2854f98632413b47fc47538bea23d5f1fcae82d59e16c02bf3f8919a3eff60051ae33d7c370c76e562c4d4e410ea3acef31ccfcb26e02c4939bee51ca02087e1f76cde4e6baa7c44ae09516fb12219029aef84cff63c740131c2a94883f2e2ceea93fccf7d7e737f7797"
    tohexEncodedSK ThirdSK  = "5880b821a903476b248919392d09214c131e7682c925ce5ca59305b3f37a803cfa5ee4ff29da485d24456df0f8ed652ea547a4bd07b4e0f9634240819e12987ed2522981b1dd1a385c0929221b7f80309a9d90c9c04ff990cba7f78392e1edf9854539ca28313941322329d9bdede7000d5c5da018c390c3c45ec06db5dce19b5e2b"
    tohexEncodedSK FourthSK = "588098ecf760f607ea429d159b56047a7ff2bb1c68506e878327767ec87ab14ccb501814c580a8a77a828bc1ab9c30b8b4cc365c744c963b0f2e752970797e939baff062ffce3855aa071615f591512e01a0722f6955631c1f7fff752cf30266236576099d8357877c38a4eb0e608f9aa2e3e3ec71fb3d40cd42d4e72df8d3f7f3a7"
    tohexEncodedSK FifthSK  = "5880d84181870133801d2447cbe53ab73ffb993402f9955fd4640e896a200c3a5b431494b739a81020dff394dbe327a3b6888cc452d716e7d3dcdeeb854b267f390e22c39a59b25a7fc4c9ab1697e8dc184d2cab98b469501dc0debf91999126469afb51e8d0dd4556dbc3f2423452bcc969f92f3484d32c875b50c467b4b1151f75"

makeExternalWalletBasedOn :: WalletClient IO -> PublicKey -> IO (Wallet, Account)
makeExternalWalletBasedOn wc publicKey = do
    newExtWallet <- randomExternalWalletWithPublicKey CreateWallet publicKey
    extWallet <- createExternalWalletCheck wc newExtWallet
    defaultAccount <- firstAccountInExtWallet wc extWallet
    pure (extWallet, defaultAccount)
