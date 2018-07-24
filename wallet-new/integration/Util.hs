{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Util where

import           Universum hiding ((^?))

import           Cardano.Wallet.Client.Http
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (shouldSatisfy, expectationFailure)
import           Test.QuickCheck (Gen, arbitrary, generate)


import qualified Pos.Core as Core
import           Pos.Crypto.Signing (PublicKey, encToPublic, encodeBase58PublicKey)
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

genesisRef :: WalletRef
genesisRef = unsafePerformIO newEmptyMVar
{-# NOINLINE genesisRef #-}

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
      print err
      if err == wantErr
        then return ()
        else expectationFailure errMsg
   where
     errMsg = "expecting Left ("++ show wantErr ++ ")"

shouldFail :: HasCallStack => IO (Either ClientError a) -> IO ()
shouldFail action = action >>= \case
    Right _  -> expectationFailure "expecting Left (..)"
    Left _   -> return ()
