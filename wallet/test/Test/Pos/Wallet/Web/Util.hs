-- | Useful functions for testing scenarios.

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Pos.Wallet.Web.Util
       (
       -- * Block utils
         wpGenBlocks
       , wpGenBlock

       -- * Wallet utils
       , importWallets
       , importSomeWallets
       , importSingleWallet
       , mostlyEmptyPassphrases
       , deriveRandomAddress
       , genWalletLvl2KeyPair
       , genWalletAddress
       , genWalletUtxo
       , expectedAddrBalance
       ) where

import           Universum

import           Control.Concurrent.STM (writeTVar)
import           Control.Monad.Random.Strict (evalRandT)
import           Data.List (head, (!!))
import qualified Data.Map as M
import           Formatting (build, sformat, (%))
import           Test.QuickCheck (Arbitrary (..), choose, frequency, sublistOf,
                     suchThat, vectorOf)
import           Test.QuickCheck.Gen (Gen (MkGen))
import           Test.QuickCheck.Monadic (assert, pick)

import           Pos.Chain.Block (Blund, LastKnownHeaderTag, blockHeader,
                     headerHashG)
import           Pos.Chain.Txp (TxIn, TxOut (..), TxOutAux (..),
                     TxpConfiguration, Utxo)
import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Client.Txp.Balances (getBalance)
import           Pos.Core (Address, BlockCount, Coin)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Common (IsBootstrapEraAddr (..), deriveLvl2KeyPair)
import           Pos.Core.Genesis (poorSecretToEncKey)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase,
                     ShouldCheckPassphrase (..), emptyPassphrase,
                     firstHardened)
import           Pos.Generator.Block (genBlocks)
import           Pos.Infra.StateLock (Priority (..), modifyStateLock)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util (HasLens (..), _neLast)

import           Pos.Util.Servant (encodeCType)
import           Pos.Util.UserSecret (mkGenesisWalletUserSecret)
import           Pos.Wallet.Web.ClientTypes (Addr, CId, Wal, encToCId)
import           Pos.Wallet.Web.Methods.Restore (importWalletDo)

import           Pos.Infra.Util.JsonLog.Events
                     (MemPoolModifyReason (ApplyBlock))
import           Test.Pos.Block.Logic.Util (EnableTxPayload, InplaceDB,
                     genBlockGenParams)
import           Test.Pos.Chain.Txp.Arbitrary ()
import           Test.Pos.Core.Dummy (dummyConfig, dummyGenesisData,
                     dummyGenesisSecretsPoor)
import           Test.Pos.Util.QuickCheck.Property (assertProperty,
                     maybeStopProperty)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)

----------------------------------------------------------------------------
-- Block utils
----------------------------------------------------------------------------

-- | Gen blocks in WalletProperty
wpGenBlocks
    :: HasConfigurations
    => TxpConfiguration
    -> Maybe BlockCount
    -> EnableTxPayload
    -> InplaceDB
    -> WalletProperty (OldestFirst [] Blund)
wpGenBlocks txpConfig blkCnt enTxPayload inplaceDB = do
    params <- genBlockGenParams dummyConfig blkCnt enTxPayload inplaceDB
    g <- pick $ MkGen $ \qc _ -> qc
    lift $ modifyStateLock HighPriority ApplyBlock $ \prevTip -> do -- FIXME is ApplyBlock the right one?
        blunds <- OldestFirst <$> evalRandT (genBlocks dummyConfig txpConfig params maybeToList) g
        case nonEmpty $ getOldestFirst blunds of
            Just nonEmptyBlunds -> do
                let tipBlockHeader = nonEmptyBlunds ^. _neLast . _1 . blockHeader
                lastKnownHeader <- view (lensOf @LastKnownHeaderTag)
                atomically $ writeTVar lastKnownHeader (Just tipBlockHeader)
                pure (tipBlockHeader ^. headerHashG, blunds)
            Nothing -> pure (prevTip, blunds)

wpGenBlock
    :: HasConfigurations
    => TxpConfiguration
    -> EnableTxPayload
    -> InplaceDB
    -> WalletProperty Blund
wpGenBlock txpConfig = fmap (Data.List.head . toList) ... wpGenBlocks txpConfig (Just 1)

----------------------------------------------------------------------------
-- Wallet test helpers
----------------------------------------------------------------------------

-- | Import some nonempty set, but not bigger than given number of elements, of genesis secrets.
-- Returns corresponding passphrases.
importWallets :: Int -> Gen PassPhrase -> WalletProperty [PassPhrase]
importWallets numLimit passGen = do
    let secrets = map poorSecretToEncKey dummyGenesisSecretsPoor
    (encSecrets, passphrases) <- pick $ do
        seks <- take numLimit <$> sublistOf secrets `suchThat` (not . null)
        let l = length seks
        passwds <- vectorOf l passGen
        pure (seks, passwds)
    let wuses = map mkGenesisWalletUserSecret encSecrets
    lift $ mapM_ (uncurry $ importWalletDo dummyConfig) (zip passphrases wuses)
    skeys <- lift getSecretKeysPlain
    assertProperty (not (null skeys)) "Empty set of imported keys"
    pure passphrases

importSomeWallets :: Gen PassPhrase -> WalletProperty [PassPhrase]
importSomeWallets = importWallets 10

importSingleWallet :: Gen PassPhrase -> WalletProperty PassPhrase
importSingleWallet passGen =
    fromMaybe (error "No wallets imported") . (fmap fst . uncons) <$> importWallets 1 passGen

mostlyEmptyPassphrases :: Gen PassPhrase
mostlyEmptyPassphrases =
    frequency
        [ (5, pure emptyPassphrase)
        , (1, arbitrary)
        ]

-- | Take passphrases of our wallets
-- and return some address from one of our wallets and id of this wallet.
-- BE CAREFUL: this functions might take long time b/c it uses @deriveLvl2KeyPair@
deriveRandomAddress :: [PassPhrase] -> WalletProperty (CId Addr, CId Wal)
deriveRandomAddress passphrases = do
    skeys <- lift getSecretKeysPlain
    let l = length skeys
    assert (l > 0)
    walletIdx <- pick $ choose (0, l - 1)
    let sk = skeys !! walletIdx
    let walId = encToCId sk
    let psw = passphrases !! walletIdx
    addressMB <- pick $ genWalletAddress sk psw
    address <- maybeStopProperty "deriveRandomAddress: couldn't derive HD address" addressMB
    pure (encodeCType address, walId)

----------------------------------------------------------------------------
-- Wallet addresses generation
----------------------------------------------------------------------------

-- | Take root secret key of wallet and a passphrase
-- and generate arbitrary wallet address with corresponding lvl 2
-- secret key
-- BE CAREFUL: this functions might take long time b/c it uses @deriveLvl2KeyPair@
genWalletLvl2KeyPair
    :: EncryptedSecretKey
    -> PassPhrase
    -> Gen (Maybe (Address, EncryptedSecretKey))
genWalletLvl2KeyPair sk psw = do
    accountIdx <- getDerivingIndex <$> arbitrary
    addressIdx <- getDerivingIndex <$> arbitrary
    pure $ deriveLvl2KeyPair
        (IsBootstrapEraAddr True)
        (ShouldCheckPassphrase False)
        psw sk accountIdx addressIdx

-- | Take root secret key of wallet and a passphrase
-- and generate arbitrary wallet address
-- BE CAREFUL: this functions might take long time b/c it uses @deriveLvl2KeyPair@
genWalletAddress
    :: EncryptedSecretKey
    -> PassPhrase
    -> Gen (Maybe Address)
genWalletAddress sk psw = fst <<$>> genWalletLvl2KeyPair sk psw

-- | Generate utxo which contains only addresses from given wallet
-- BE CAREFUL: @deriveLvl2KeyPair@ is called `size` times here -
-- generating large utxos will take a long time
genWalletUtxo
    :: EncryptedSecretKey
    -> PassPhrase
    -> Int                -- Size of Utxo
    -> Gen (Maybe Utxo)
genWalletUtxo sk psw size =
    fmap M.fromList . sequence <$> replicateM size genOutput
  where
    genOutput :: Gen (Maybe (TxIn, TxOutAux))
    genOutput = do
        txIn <- arbitrary
        coin <- arbitrary
        (\address -> (txIn, TxOutAux $ TxOut address coin)) <<$>>
            genWalletAddress sk psw

----------------------------------------------------------------------------
-- Wallet properties
----------------------------------------------------------------------------

-- Useful properties

-- | Checks that balance of address is positive and returns it.
expectedAddrBalance :: Address -> Coin -> WalletProperty ()
expectedAddrBalance addr expected = do
    balance <- lift $ getBalance dummyGenesisData addr
    assertProperty (balance == expected) $
        sformat ("balance for address "%build
                    %" mismatched, expected: "%build
                    %", actual balance: "%build)
                addr expected balance

----------------------------------------------------------------------------
-- Deriving index
----------------------------------------------------------------------------

-- | Index of account or address in acceptable range.
newtype DerivingIndex = DerivingIndex
    { getDerivingIndex :: Word32
    } deriving (Eq, Num, Ord)

instance Arbitrary DerivingIndex where
    arbitrary = DerivingIndex <$> choose (firstHardened, firstHardened + (firstHardened - 1))
