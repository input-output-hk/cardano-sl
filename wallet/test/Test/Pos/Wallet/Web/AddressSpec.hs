{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.AddressSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Formatting (sformat, (%))
import           Serokell.Data.Memory.Units (memory)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Discard (..), arbitrary)
import           Test.QuickCheck.Monadic (pick, stop)

import           Pos.Binary (biSize)
import           Pos.Client.Txp.Addresses (getFakeChangeAddress, getNewAddress)
import           Pos.Core.Common (Address)
import           Pos.Crypto (PassPhrase)

import           Pos.Wallet.Web.Account (GenSeed (..), genUniqueAddress)
import           Pos.Wallet.Web.ClientTypes (AccountId, CAccountInit (..), caId)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.Logic (newAccount)
import           Pos.Wallet.Web.State (askWalletSnapshot, getWalletAddresses,
                     wamAddress)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.Pos.Core.Dummy (dummyEpochSlots)
import           Test.Pos.Util.QuickCheck.Property (assertProperty, expectedOne)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)
import           Test.Pos.Wallet.Web.Util (importSingleWallet,
                     mostlyEmptyPassphrases)

spec :: Spec
spec = withDefConfigurations $ \_ _ _ ->
    describe "Fake address has maximal possible size" $
    modifyMaxSuccess (const 10) $ do
        prop "getNewAddress" $
            fakeAddressHasMaxSizeTest changeAddressGenerator
        prop "genUniqueAddress" $
            fakeAddressHasMaxSizeTest commonAddressGenerator

type AddressGenerator = AccountId -> PassPhrase -> WalletProperty Address

fakeAddressHasMaxSizeTest :: AddressGenerator -> Word32 -> WalletProperty ()
fakeAddressHasMaxSizeTest generator accSeed = do
    passphrase <- importSingleWallet mostlyEmptyPassphrases
    ws <- askWalletSnapshot
    wid <- expectedOne "wallet addresses" $ getWalletAddresses ws
    accId <- lift $ decodeCTypeOrFail . caId
         =<< newAccount (DeterminedSeed accSeed) passphrase (CAccountInit def wid)
    address <- generator accId passphrase

    largeAddress <- lift $ getFakeChangeAddress dummyEpochSlots

    assertProperty
        (biSize largeAddress >= biSize address)
        (sformat (memory%" < "%memory) (biSize largeAddress) (biSize address))

-- | Addresses generator used in 'MonadAddresses' to create change addresses.
-- Unfortunatelly, its randomness doesn't depend on QuickCheck seed,
-- so another proper generator is helpful.
changeAddressGenerator :: AddressGenerator
changeAddressGenerator accId passphrase =
    lift $ getNewAddress dummyEpochSlots (accId, passphrase)

-- | Generator which is directly used in endpoints.
commonAddressGenerator :: AddressGenerator
commonAddressGenerator accId passphrase = do
    ws <- askWalletSnapshot
    addrSeed <- pick arbitrary
    let genAddress = genUniqueAddress ws (DeterminedSeed addrSeed) passphrase accId
    -- can't catch under 'PropertyM', workarounding
    maddr <- lift $ (Just <$> genAddress) `catch` seedBusyHandler
    addr <- maybe (stop Discard) pure maddr
    return $ addr ^. wamAddress
  where
    seedBusyHandler (InternalError "address generation: this index is already taken")
                      = pure Nothing
    seedBusyHandler e = throwM e
