{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
module Lib where

import           Prelude
import           CLI
import           Types (UberMonad)
import           Control.Concurrent
import           Control.Lens                         (view)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString                      as B
import           Data.Monoid
import           Data.String.Conv
import qualified Data.Text                            as T
import           Data.Time
import           Dhall
import           GHC.Generics                         (Generic)
import           Network.Haskoin.Crypto
import           Pos.Crypto.Random
import           Pos.DB.GState.Common                 (getTip)
import           Pos.StateLock
import           Pos.Util.BackupPhrase
import           Pos.Util.Servant
import           Pos.Util.Util                        (lensOf)
import           Pos.Wallet.Web.Account
import           Pos.Wallet.Web.ClientTypes
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.Methods.Logic
import           Pos.Wallet.Web.Methods.Restore
import           Rendering
import           Text.Printf

--
-- Types
--

data GenSpec = GenSpec {
    wallets       :: !Integer
    -- ^ How many wallets to create
    , wallet_spec :: WalletSpec
    -- ^ The specification for each wallet.
    } deriving (Show, Eq, Generic)

instance Interpret GenSpec

data WalletSpec = WalletSpec {
    accounts       :: !Integer
    -- ^ How many accounts to generate
    , account_spec :: AccountSpec
    -- ^ How specification for each account.
    } deriving (Show, Eq, Generic)

instance Interpret WalletSpec

data AccountSpec = AccountSpec {
    addresses :: !Integer
    -- How many addresses to generate.
    } deriving (Show, Eq, Generic)

instance Interpret AccountSpec

--
-- Functions
--

-- | Load the 'GenSpec' from an input file.
loadGenSpec :: FilePath -> IO GenSpec
loadGenSpec = input auto . toS

-- | Run an action and report the time it took.
timed :: MonadIO m => m a -> m a
timed action = do
    before <- liftIO getCurrentTime
    res <- action
    after <- liftIO getCurrentTime
    -- NOTE: Mind the `fromEnum` overflow.
    let diff = fromEnum $ after `diffUTCTime` before
    liftIO $ putStrLn $ printf "Action took %f seconds." (fromIntegral diff / (1000000000000 :: Double))
    return res

fakeSync :: UberMonad ()
fakeSync = do
    say "Faking StateLock syncing..."
    tip <- getTip
    (StateLock mvar _) <- view (lensOf @StateLock)
    () <$ liftIO (tryPutMVar mvar tip)

-- | The main entry point.
generate :: CLI -> GenSpec -> UberMonad ()
generate CLI{..} spec@GenSpec{..} = do
    fakeSync
    -- If `addTo` is not mempty, skip the generation
    -- but append the requested addresses to the input
    -- CAccountId.
    case addTo of
        Just accId -> addAddressesTo accId spec
        Nothing -> do
            say $ printf "Generating %d wallets..." wallets
            wallets' <- timed (forM [1..wallets] genWallet)
            forM_ (zip [1..] wallets') (genAccounts spec)
    say $ green "OK."

addAddressesTo :: AccountId -> GenSpec -> UberMonad ()
addAddressesTo cid spec = genAddresses spec cid

genAccounts :: GenSpec -> (Int, CWallet) -> UberMonad ()
genAccounts spec@(wallet_spec -> wspec) (idx, wallet) = do
    let accs = accounts wspec
    say $ printf "Generating %d accounts for Wallet %d..." accs idx
    cAccounts <- timed (forM [1..accs] (genAccount wallet))
    let cids = map toAccountId cAccounts
    forM_ cids (genAddresses spec)

toAccountId :: CAccount -> AccountId
toAccountId CAccount{..} = either (error . toS) id (decodeCType caId)

genAddresses :: GenSpec -> AccountId -> UberMonad ()
genAddresses (account_spec . wallet_spec -> aspec) cid = do
    let addrs = addresses aspec
    say $ printf "Generating %d addresses for Account %s..." addrs (renderAccountId cid)
    timed (forM_ [1..addrs] (const $ genAddress cid))


-- | Creates a new 'CWallet'.
genWallet :: Integer -> UberMonad CWallet
genWallet walletNum = do
    entropy   <- randomNumber walletNum
    mnemonic  <- newRandomMnemonic (toEntropy entropy)
    r         <- newWallet mempty (walletInit mnemonic)
    return r
  where
    walletInit :: BackupPhrase -> CWalletInit
    walletInit backupPhrase = CWalletInit {
      cwInitMeta      = CWalletMeta
          { cwName      = toS $ "Wallet #" <> show walletNum
          , cwAssurance = CWANormal
          , cwUnit      = 0
        }
      , cwBackupPhrase  = backupPhrase
      }

-- | Generates some 'Entropy' from an 'Integer'. Due to the fact
-- Haskoin accepts only multiple of 4 bytes, we pad the input with
-- zeros. It leads to quite crappy 12-words mnemonic, but as long as
-- they don't clash with each other, we are happy.
toEntropy :: Integer -> Entropy
toEntropy x =
    let packs = B.unpack (toS $ show x)
        len   = length packs
    in B.pack $ case len < 16 of
      True  -> packs <> replicate (16 - len) 0x0
      False -> take (len - (len `mod` 16)) packs

-- | Generates a new 'BackupPhrase' piggybacking on @Haskoin@ for
-- the BIP-39 words list.
newRandomMnemonic :: MonadIO m => Entropy -> m BackupPhrase
newRandomMnemonic entropy = case toMnemonic entropy of
    Left e  -> error ("Error: " <> e)
    Right x -> pure (mkBackupPhrase12 (T.words $ toS x))

-- | Creates a new 'CAccount'.
genAccount :: CWallet -> Integer -> UberMonad CAccount
genAccount CWallet{..} accountNum = do
    newAccountIncludeUnready True RandomSeed mempty accountInit
  where
    accountInit :: CAccountInit
    accountInit = CAccountInit {
      caInitMeta = CAccountMeta {
          caName = toS $ "Account Number #" <> show accountNum
          }
      , caInitWId  = cwId
      }

-- | Creates a new 'CAddress'.
genAddress :: AccountId -> UberMonad CAddress
genAddress cid = do
    let (walletId, addrNum) = (aiWId cid, aiIndex cid)
    newAddress RandomSeed mempty (AccountId walletId addrNum)

