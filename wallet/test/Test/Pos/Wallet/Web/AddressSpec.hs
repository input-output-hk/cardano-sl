module Test.Pos.Wallet.Web.AddressSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Formatting (sformat, (%))
import           Serokell.Data.Memory.Units (memory)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

import           Pos.Binary (biSize)
import           Pos.Client.Txp.Addresses (getFakeChangeAddress, getNewAddress)
import           Pos.Core.Address (Address)
import           Pos.Crypto (PassPhrase)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)

import           Pos.Wallet.Web.Account (nextAddress)
import           Pos.Wallet.Web.ClientTypes (AccountId, CAccountInit (..), caId, cwamId)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.Logic (newAccount)
import           Pos.Wallet.Web.State (getWalletAddresses)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)
import           Test.Pos.Util (assertProperty, expectedOne, withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)
import           Test.Pos.Wallet.Web.Util (importSingleWallet, mostlyEmptyPassphrases)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
    describe "Fake address has maximal possible size" $
    modifyMaxSuccess (const 10) $ do
        prop "getNewAddress" $
            fakeAddressHasMaxSizeTest changeAddressGenerator
        prop "nextAddress" $
            fakeAddressHasMaxSizeTest commonAddressGenerator

type AddressGenerator = AccountId -> PassPhrase -> WalletProperty Address

fakeAddressHasMaxSizeTest
    :: (HasConfigurations, HasCompileInfo)
    => AddressGenerator -> Word32 -> WalletProperty ()
fakeAddressHasMaxSizeTest generator _accSeed = do
    passphrase <- importSingleWallet mostlyEmptyPassphrases

    wid <- expectedOne "wallet addresses" =<< getWalletAddresses

    accId <- lift $ decodeCTypeOrFail . caId
         =<< newAccount passphrase (CAccountInit def wid)

    address <- generator accId passphrase

    largeAddress <- lift getFakeChangeAddress

    assertProperty
        (biSize largeAddress >= biSize address)
        (sformat (memory%" < "%memory) (biSize largeAddress) (biSize address))

-- | Addresses generator used in 'MonadAddresses' to create change addresses.
-- Unfortunatelly, its randomness doesn't depend on QuickCheck seed,
-- so another proper generator is helpful.
changeAddressGenerator :: HasConfigurations => AddressGenerator
changeAddressGenerator accId passphrase = lift $ getNewAddress (accId, passphrase)

-- | Generator which is directly used in endpoints.
commonAddressGenerator :: HasConfigurations => AddressGenerator
commonAddressGenerator accId passphrase = do
    addressE <- lift $ nextAddress accId passphrase
    case addressE of
        Right address -> do
            lift $ decodeCTypeOrFail (cwamId address)
        Left err ->
            lift $ throwM $ InternalError err
