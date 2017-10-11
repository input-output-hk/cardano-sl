-- | Useful functions for testing scenarios.

module Test.Pos.Wallet.Web.Util
       (
       -- * Block utils
         wpGenBlocks
       , wpGenBlock

       -- * Wallet utils
       , importSomeWallets
       , deriveRandomAddress
       , expectedAddrBalance
       ) where

import           Universum
import           Unsafe                         (unsafeHead)

import           Control.Concurrent.STM         (putTMVar, tryTakeTMVar, writeTVar)
import           Control.Monad.Random.Strict    (evalRandT)
import           Data.List                      ((!!))
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, (%))
import           Test.QuickCheck                (Arbitrary (..), choose, sublistOf,
                                                 vectorOf)
import           Test.QuickCheck.Gen            (Gen (MkGen))
import           Test.QuickCheck.Monadic        (assert, pick)

import           Pos.Block.Core                 (blockHeader)
import           Pos.Block.Types                (Blund)
import           Pos.Client.KeyStorage          (getSecretKeysPlain)
import           Pos.Client.Txp.Balances        (getBalance)
import           Pos.Context                    (LastKnownHeaderTag, ProgressHeaderTag)
import           Pos.Core                       (Address, BlockCount, Coin,
                                                 HasConfiguration, generatedSecrets,
                                                 gsSecretKeysPoor, headerHashG)
import           Pos.Core.Address               (IsBootstrapEraAddr (..),
                                                 deriveLvl2KeyPair, deriveLvl2KeyPair)
import           Pos.Crypto                     (PassPhrase, ShouldCheckPassphrase (..),
                                                 firstHardened)
import           Pos.Generator.Block            (genBlocks)
import           Pos.Launcher                   (HasConfigurations)
import           Pos.StateLock                  (Priority (..), modifyStateLock)
import           Pos.Util.Chrono                (OldestFirst (..))
import           Pos.Util.CompileInfo           (HasCompileInfo)
import           Pos.Util.Servant               (encodeCType)
import           Pos.Util.UserSecret            (mkGenesisWalletUserSecret)
import           Pos.Util.Util                  (HasLens (..), _neLast)
import           Pos.Wallet.Web.ClientTypes     (Addr, CId)
import           Pos.Wallet.Web.Methods.Restore (importWalletDo)

import           Test.Pos.Block.Logic.Util      (EnableTxPayload, InplaceDB,
                                                 genBlockGenParams)
import           Test.Pos.Util                  (assertProperty, maybeStopProperty)
import           Test.Pos.Wallet.Web.Mode       (WalletProperty)

----------------------------------------------------------------------------
-- Block utils
----------------------------------------------------------------------------

-- | Gen blocks in WalletProperty
wpGenBlocks
    :: (HasCompileInfo, HasConfigurations)
    => Maybe BlockCount
    -> EnableTxPayload
    -> InplaceDB
    -> WalletProperty (OldestFirst [] Blund)
wpGenBlocks blkCnt enTxPayload inplaceDB = do
    params <- genBlockGenParams blkCnt enTxPayload inplaceDB
    g <- pick $ MkGen $ \qc _ -> qc
    lift $ modifyStateLock HighPriority "wpGenBlocks" $ \prevTip -> do
        blunds <- evalRandT (genBlocks params) g
        case NE.nonEmpty $ getOldestFirst blunds of
            Just nonEmptyBlunds -> do
                let tipBlockHeader = nonEmptyBlunds ^. _neLast . _1 . blockHeader
                lastKnownHeader <- view (lensOf @LastKnownHeaderTag)
                atomically $ writeTVar lastKnownHeader (Just tipBlockHeader)
                progressHeader <- view (lensOf @ProgressHeaderTag)
                atomically $ do
                    void $ tryTakeTMVar progressHeader
                    putTMVar progressHeader tipBlockHeader
                pure (tipBlockHeader ^. headerHashG, blunds)
            Nothing -> pure (prevTip, blunds)

wpGenBlock
    :: (HasCompileInfo, HasConfigurations)
    => EnableTxPayload
    -> InplaceDB
    -> WalletProperty Blund
wpGenBlock = fmap (unsafeHead . toList) ... wpGenBlocks (Just 1)

----------------------------------------------------------------------------
-- Wallet test helpers
----------------------------------------------------------------------------

-- | Import some nonempty set, but not bigger than 10 elements, of genesis secrets.
-- Returns corresponding passphrases.
importSomeWallets :: (HasConfigurations, HasCompileInfo)  => WalletProperty [PassPhrase]
importSomeWallets = do
    let secrets =
            map (view _2) .
            gsSecretKeysPoor .
            fromMaybe (error "Generated secrets are unknown") $ generatedSecrets
    -- pva701: does sublistOf generate non-empty list?
    (encSecrets, passphrases) <- pick $ do
        seks <- take 10 <$> sublistOf secrets
        let l = length seks
        passwds <- vectorOf l arbitrary
        pure (seks, passwds)
    let wuses = map mkGenesisWalletUserSecret encSecrets
    lift $ mapM_ (uncurry importWalletDo) (zip passphrases wuses)
    skeys <- lift getSecretKeysPlain
    assert (length skeys > 0)
    pure passphrases

-- | Take passphrases of our wallets
-- and return some address from one of our wallets
-- BE CAREFUL: this functions might take long time b/c it uses @deriveLvl2KeyPair@
deriveRandomAddress :: [PassPhrase] -> WalletProperty (CId Addr)
deriveRandomAddress passphrases = do
    skeys <- lift getSecretKeysPlain
    let l = length skeys
    assert (l > 0)
    walletIdx <- pick $ choose (0, l - 1)
    accountIdx <- pick $ getDerivingIndex <$> arbitrary
    addressIdx <- pick $ getDerivingIndex <$> arbitrary
    let sk = skeys !! walletIdx
    let psw = passphrases !! walletIdx
    let !addressMB = fst <$>
            deriveLvl2KeyPair
                (IsBootstrapEraAddr True)
                (ShouldCheckPassphrase False)
                psw sk accountIdx addressIdx
    address <- maybeStopProperty "withRandomOurAddress: couldn't derive HD address" addressMB
    pure $ encodeCType address

----------------------------------------------------------------------------
-- Wallet properties
----------------------------------------------------------------------------

-- Useful properties

-- | Checks that balance of address is positive and returns it.
expectedAddrBalance :: HasConfiguration => Address -> Coin -> WalletProperty ()
expectedAddrBalance addr expected = do
    balance <- lift $ getBalance addr
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
    arbitrary = DerivingIndex <$> choose (0, firstHardened - 1)
