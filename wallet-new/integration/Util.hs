{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Util where

import qualified Serokell.Util.Base16 as B16
import           Universum

import           Cardano.Wallet.Client.Http
import           Test.Hspec (expectationFailure, shouldSatisfy)
import           Test.QuickCheck (arbitrary, generate)

import           Pos.Binary.Class (decodeFull')
import qualified Pos.Core as Core
import           Pos.Crypto.Signing (EncryptedSecretKey, PublicKey, encToPublic,
                                     encodeBase58PublicKey, noPassEncrypt)

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

randomExternalWalletWithPublicKey :: WalletOperation -> PublicKey -> IO NewExternalWallet
randomExternalWalletWithPublicKey walletOp publicKey =
    generate $
        NewExternalWallet
            <$> pure (encodeBase58PublicKey publicKey)
            <*> arbitrary
            <*> pure "External Wallet"
            <*> pure walletOp

createWalletCheck :: HasCallStack => WalletClient IO -> NewWallet -> IO Wallet
createWalletCheck wc newWallet =
    shouldReturnRight $ fmap wrData <$> postWallet wc newWallet

createExternalWalletCheck :: HasCallStack => WalletClient IO -> NewExternalWallet -> IO Wallet
createExternalWalletCheck wc newExtWallet =
    shouldReturnRight $ fmap wrData <$> postExternalWallet wc newExtWallet

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

createRandomSampleWallet :: HasCallStack => WalletClient IO -> IO Wallet
createRandomSampleWallet wc
     = randomWallet CreateWallet >>= createWalletCheck wc

lookupGenesisWallet :: HasCallStack => WalletClient IO -> IO Wallet
lookupGenesisWallet wc = do
    allWallets <- fmap wrData $ shouldReturnRight $ getWallets wc
    case find (("Genesis wallet" ==) . walName) allWallets of
      Just gen -> return gen
      Nothing -> do
        expectationFailure $ "Genesis wallet is missing;"
                                     ++ "did you import it prior to executing the test-suite?"
        error "unreachable"

getWalletBalanceInLovelaces :: HasCallStack => WalletClient IO -> Wallet -> IO Word64
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
    | SixthSK
    | SeventhSK
    | EighthSK
    | NinthSK
    | TenthSK

makeWalletRootKeys :: WhichSK -> (EncryptedSecretKey, PublicKey)
makeWalletRootKeys skNumber =
    let Right skSerialized = B16.decode $ tohexEncodedSK skNumber
        Right rootSK = decodeFull' skSerialized
        encRootSK = noPassEncrypt rootSK
        rootPK = encToPublic encRootSK
    in (encRootSK, rootPK)
  where
    tohexEncodedSK FirstSK   = "588050d9b69d2ccb4c61fabbd1e5cb9831320e884a13bbdb9cb48de39a04a748e64edef294b6ca10e97e2be10891b62de80852c81689b7e645b530333ffdaf16ce14edc1d053732591b8d0fda116adf4c299a5c8675eeb6cd51e27219c7c62b488fec3df4c8b129e0b04358b60066595351fb47a46cc2aa7ab3caf24ee5a597cd6a7"
    tohexEncodedSK SecondSK  = "588070ed286e16e63e1ddaf8f62c7bb90a78fa63240f382d2854f98632413b47fc47538bea23d5f1fcae82d59e16c02bf3f8919a3eff60051ae33d7c370c76e562c4d4e410ea3acef31ccfcb26e02c4939bee51ca02087e1f76cde4e6baa7c44ae09516fb12219029aef84cff63c740131c2a94883f2e2ceea93fccf7d7e737f7797"
    tohexEncodedSK ThirdSK   = "5880b821a903476b248919392d09214c131e7682c925ce5ca59305b3f37a803cfa5ee4ff29da485d24456df0f8ed652ea547a4bd07b4e0f9634240819e12987ed2522981b1dd1a385c0929221b7f80309a9d90c9c04ff990cba7f78392e1edf9854539ca28313941322329d9bdede7000d5c5da018c390c3c45ec06db5dce19b5e2b"
    tohexEncodedSK FourthSK  = "588098ecf760f607ea429d159b56047a7ff2bb1c68506e878327767ec87ab14ccb501814c580a8a77a828bc1ab9c30b8b4cc365c744c963b0f2e752970797e939baff062ffce3855aa071615f591512e01a0722f6955631c1f7fff752cf30266236576099d8357877c38a4eb0e608f9aa2e3e3ec71fb3d40cd42d4e72df8d3f7f3a7"
    tohexEncodedSK FifthSK   = "5880d84181870133801d2447cbe53ab73ffb993402f9955fd4640e896a200c3a5b431494b739a81020dff394dbe327a3b6888cc452d716e7d3dcdeeb854b267f390e22c39a59b25a7fc4c9ab1697e8dc184d2cab98b469501dc0debf91999126469afb51e8d0dd4556dbc3f2423452bcc969f92f3484d32c875b50c467b4b1151f75"
    tohexEncodedSK SixthSK   = "588070ced14b0cfd06f797f7350599f52fa4ba99097e640113ddd0b75116e1cb59481ad04c750e404c0938ad81aefa89a7b7fab1f0615f4a9ff8be8cc925ca0a6a766941e9916f617acec3c78753ed8f251cc5a7389c0d8fad2050986b34c69113c5a511b569c50e2b325c54d87e246b412820d3f5e576da4ac14633d9da54b6d452"
    tohexEncodedSK SeventhSK = "588020be87a48e332051140f387b1ef8d1871c624762c155c3a305f516f4f5011f4fb26250e765c662b847981498dd60db93042dc6f46aa4de808b9ec12d97795c701d76e596e93d99519c0cec720fa9086c93495ee996b77dc7958ae8aaf5f6169ce2e06684df827bf7bd2a0d9492264dec64a5427b741e0b3f44497bb0dd75c98a"
    tohexEncodedSK EighthSK  = "5880785e65465e75085d4d6ee18338364bcacc2fa04f7d0c4d41eed9e4e962fce151e9cbd232a603700e6cbdddbf5db756d45a4e57fe4b776bcdd3dc7091e7c2124d90a7f9e828e5504a17e6fd4880f60d2d482116b78cc1785d3dca23c6fb26f224afd4c790a4732d3aad1c5f652147706308caecdbbfe4d5a6cf42089ce0bce4a9"
    tohexEncodedSK NinthSK   = "5880e0f810ca1aac5423410b37b7218245250882bee78d0bf7c8e32a901a50860e4b1331e2a2557b8ae0bc723bc927b291120a4f71cd681d8e41f2b388e6945be3533e5e7a53eb12f050d9519e5a82c33f1e0f919ed9c1ff5103f4a2c4d791bb07a7fdb989fa667aa91ff27741edbda34141005658f58efc5f5e82901f72578fafbb"
    tohexEncodedSK TenthSK   = "588048285acfa538df38ad592d3595579b6ed617e24bf309f302525353811a1b394bdae02b5a7ed4d6c05771c0e85ac5b65a5cfe0684a411be8bba1319fb76ac0dcc89ecfea812f4f2124a23e8f5ee3d3caceaef289ebbdac2ad1e4cf2c00d7fcfef454dc3a056eb2c8329a1d4ccf347cb736cb1bf8e25c40d5966fa9cd1c319d7a6"

makeExternalWalletBasedOn :: WalletClient IO -> PublicKey -> IO (Wallet, Account)
makeExternalWalletBasedOn wc publicKey = do
    newExtWallet <- randomExternalWalletWithPublicKey CreateWallet publicKey
    extWallet <- createExternalWalletCheck wc newExtWallet
    defaultAccount <- firstAccountInExtWallet wc extWallet
    pure (extWallet, defaultAccount)
