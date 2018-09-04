{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Lib where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON, eitherDecodeStrict, withObject, (.:))
import qualified Data.ByteString as B
import           Data.Function (id)
import qualified Data.List.NonEmpty as NE
import           Data.Map (fromList, union)
import           Data.String.Conv (toS)
import           Data.Time (diffUTCTime, getCurrentTime)
import           GHC.Generics (Generic)

import           Crypto.Random.Entropy (getEntropy)
import           Pos.Client.Txp (TxHistoryEntry (..))
import           Pos.Core (Address, Coin, mkCoin)
import           Pos.Core.NetworkMagic (NetworkMagic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.DB.GState.Common (getTip)
import           Pos.Infra.StateLock (StateLock (..))
import           Pos.Txp (Tx (..), TxId, TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types (utxoToModifier)
import           Pos.Util.BackupPhrase (BackupPhrase (..))
import           Pos.Util.Mnemonics (toMnemonic)
import           Pos.Util.Servant (decodeCType)
import           Pos.Util.Util (lensOf)
import           Pos.Wallet.Web.Account (GenSeed (..))
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccount (..), CAccountInit (..),
                                             CAccountMeta (..), CAddress (..), CId (..),
                                             CWallet (..), CWalletAssurance (..), CWalletInit (..),
                                             CWalletMeta (..), Wal)
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.Methods.Logic (getAccounts, newAccountIncludeUnready, newAddress)
import           Pos.Wallet.Web.Methods.Restore (newWallet)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State.State (askWalletDB, getWalletSnapshot, getWalletUtxo,
                                             insertIntoHistoryCache, setWalletUtxo,
                                             updateWalletBalancesAndUtxo)
import           Test.QuickCheck (Gen, arbitrary, choose, frequency, generate, vectorOf)
import           Text.Printf (printf)

import           CLI (CLI (..))
import           Rendering (green, renderAccountId, say)
import           Types (UberMonad)

import           Test.Pos.Txp.Arbitrary ()

--
-- Types
--

-- | A simple example of how the configuration looks like.
_exampleSpec :: GenSpec
_exampleSpec = GenSpec
    { walletSpec = WalletSpec
        { accounts = 1
        , accountSpec = AccountSpec { addresses = 100 }
        , fakeUtxoCoinDistr = RangeDistribution { amount = 1000, range = 100 }
        , fakeTxsHistory = SimpleTxsHistory { txsCount = 100, numOutgoingAddress = 3 }
        }
    , wallets = 1
    }

data GenSpec = GenSpec
    { wallets    :: !Integer
    -- ^ How many wallets to create
    , walletSpec :: WalletSpec
    -- ^ The specification for each wallet.
    } deriving (Show, Eq, Generic)

instance FromJSON GenSpec
instance ToJSON GenSpec

data WalletSpec = WalletSpec
    { accounts          :: !Integer
    -- ^ How many accounts to generate
    , accountSpec       :: AccountSpec
    -- ^ How specification for each account.
    , fakeUtxoCoinDistr :: FakeUtxoCoinDistribution
    -- ^ Configuration for the generation of the fake UTxO.
    , fakeTxsHistory    :: FakeTxsHistory
    -- ^ Configuration for the generation of the fake txs history.
    } deriving (Show, Eq, Generic)

instance FromJSON WalletSpec
instance ToJSON WalletSpec

data AccountSpec = AccountSpec
    { addresses :: !Integer
    -- ^ How many addresses to generate.
    } deriving (Show, Eq, Generic)

instance FromJSON AccountSpec
instance ToJSON AccountSpec

-- TODO(ks): The question here is whether we need to support some other
-- strategies for distributing money, like maybe using a fixed amount
-- of `toAddress` to cap the distribution - we have examples where we
-- have around 80 000 addresses which is a lot and can be intensive?
-- For now, KISS.
data FakeUtxoCoinDistribution
    = NoDistribution
    -- ^ Do not distribute the coins.
    | RangeDistribution
        { range  :: !Integer
        -- ^ Distributes to only XX addresses.
        , amount :: !Integer
        -- ^ The amount we want to distribute to those addresses.
        }
    -- ^ TODO(adn): For now we KISS, later we can add more type constructors
    deriving (Show, Eq, Generic)

{-
λ> decode $ "{\"type\":\"none\"}" :: Maybe FakeUtxoCoinDistribution
Just NoDistribution
λ> decode $ "{\"type\":\"range\",\"range\":1000,\"amount\",10}" :: Maybe FakeUtxoCoinDistribution
Just (RangeDistribution {range = 1000})
-}

instance FromJSON FakeUtxoCoinDistribution where
    parseJSON = withObject "CoinDistribution" $ \o -> do
        distrType <- o .: "type"
        case distrType of
          "none"  -> pure NoDistribution
          "range" -> RangeDistribution <$> o .: "range" <*> o .: "amount"
          _       -> fail ("Unknown type: " ++ distrType)

instance ToJSON FakeUtxoCoinDistribution

type NumOfOutgoingAddresses = Int
type NumberOfBatches = Int

-- TODO(ks): As with @FakeUtxoCoinDistribution@, we may have different strategies
-- to generate @FakeTxsHistory@.
data FakeTxsHistory
    = NoHistory
    -- ^ Do not generate fake history.
    | SimpleTxsHistory
        { txsCount           :: !Integer
        -- ^ Number of txs we want to generate.
        , numOutgoingAddress :: !NumOfOutgoingAddresses
        -- ^ Number of outgoing addreses of a single @Tx@.
        }
    -- ^ Simple tx history generation.
    -- TODO(ks): For now KISS, we can add more generation strategies.
    deriving (Show, Eq, Generic)

instance FromJSON FakeTxsHistory where
    parseJSON = withObject "HistoryGeneration" $ \o -> do
        distrType <- o .: "type"
        case distrType of
          "none"   -> pure NoHistory
          "simple" -> SimpleTxsHistory <$> o .: "txsCount" <*> o .: "numOutgoingAddress"
          _        -> fail ("Unknown type: " ++ distrType)

instance ToJSON FakeTxsHistory


--
-- Functions
--

-- | Load the 'GenSpec' from an input file.
loadGenSpec :: FilePath -> IO GenSpec
loadGenSpec fpath = do
    fileContent <- B.readFile fpath
    either exception pure (eitherDecodeStrict fileContent)
  where
    -- TODO(ks): MonadThrow maybe?
    exception = error "Invalid JSON configuration file format!"


-- | Run an action and report the time it took.
timed :: MonadIO m => m a -> m a
timed action = do
    before  <- liftIO getCurrentTime
    res     <- action
    after   <- liftIO getCurrentTime
    -- NOTE: Mind the `fromEnum` overflow.
    let diff = fromEnum $ after `diffUTCTime` before

    let printString :: String
        printString = printf "Action took %f seconds." (fromIntegral diff / (1000000000000 :: Double))

    liftIO $ putStrLn printString
    return res


fakeSync :: UberMonad ()
fakeSync = do
    say "Faking StateLock syncing..."
    tip <- getTip
    (StateLock mvar _) <- view (lensOf @StateLock)
    () <$ tryPutMVar mvar tip


-- | The main entry point.
generateWalletDB :: NetworkMagic -> CLI -> GenSpec -> UberMonad ()
generateWalletDB nm CLI{..} spec@GenSpec{..} = do
    fakeSync
    -- If `addTo` is not mempty, skip the generation
    -- but append the requested addresses to the input
    -- CAccountId.

    -- TODO(ks): Simplify this?
    let fakeUtxoSpec = fakeUtxoCoinDistr walletSpec
    let fakeTxs      = fakeTxsHistory walletSpec

    case addTo of
        Just accId ->
            if (checkIfAddTo fakeUtxoSpec fakeTxs) then
                addAddressesTo nm spec accId
            else do
                timed $ generateFakeUtxo nm fakeUtxoSpec accId
                timed $ generateFakeTxs nm fakeTxs accId

        Nothing -> do
            say $ printf "Generating %d wallets..." wallets
            wallets' <- timed (forM [1..wallets] (genWallet nm))
            forM_ (zip [1..] wallets') (genAccounts nm spec)
    say $ green "OK."
  where
    checkIfAddTo :: FakeUtxoCoinDistribution -> FakeTxsHistory -> Bool
    checkIfAddTo NoDistribution NoHistory = True
    checkIfAddTo _              _         = False


-- | Here we generate fake txs. For now it's a simple arbitrary generation.
generateFakeTxs :: NetworkMagic -> FakeTxsHistory -> AccountId -> UberMonad ()
generateFakeTxs _  NoHistory _                = pure ()
generateFakeTxs nm SimpleTxsHistory{..} aId   = do
    -- Get the number of txs we need to generate.
    let txsNumber = fromIntegral txsCount

    let batchSize = 100
    let batches   = txsNumber `div` batchSize
    let remainder = txsNumber `mod` batchSize

    -- We don't generate all txs at once since we could run out of memory.
    -- That's why we use batching so GC can clear the memory behind us in
    -- batches.
    void $ replicateM batches (generateNFakeTxs nm batchSize numOutgoingAddress aId)
    generateNFakeTxs nm remainder numOutgoingAddress aId


-- | Se we can run it in batches so we don't run out of memory.
generateNFakeTxs
    :: NetworkMagic
    -> Int
    -> NumOfOutgoingAddresses
    -> AccountId
    -> UberMonad ()
generateNFakeTxs nm txsNumber numOfAddresses aId = do

    db <- askWalletDB

    accounts <- getAccounts nm Nothing

    let walletId :: CId Wal
        walletId = aiWId aId

    -- These are all the addresses from the accounts.
    let accountsAddrs :: [Address]
        accountsAddrs = rights . map unwrapCAddress $ concatMap caAddresses accounts

    -- We take just the number of @Address@ we require. It might not
    -- be that realistic that we always grab the first N, but KISS.
    let outputAddresses :: [Address]
        outputAddresses = take numOfAddresses accountsAddrs

    fakeTxs        <- liftIO $ replicateM txsNumber (generateRealTxHistE outputAddresses)

    let fakeMapTxs :: Map TxId TxHistoryEntry
        fakeMapTxs = fromList $ zip (map _thTxId fakeTxs) fakeTxs

    -- Insert into the @WalletStorage@.
    insertIntoHistoryCache db walletId fakeMapTxs


-- | Generate "realistic" @TxHistoryEntry@ from a list of @Address@ that
-- we will use as outputs.
generateRealTxHistE :: [Address] -> IO TxHistoryEntry
generateRealTxHistE outputAddresses = do

    -- Generate a list of @TxOut@.
    let genTxOut :: Gen [TxOut]
        genTxOut = forM outputAddresses $ \address -> TxOut address <$> genCoins

    -- Generate fields required for @TxHistoryEntry@.
    fakeTxIds <- liftIO $ generate arbitrary
    fakeChain <- liftIO $ generate arbitrary
    fakeTxOut <- liftIO $ generate genTxOut
    fakeTime  <- liftIO $ generate arbitrary
    fakeTx    <- liftIO $ generate $ genTxs fakeTxOut

    pure $ THEntry
        { _thTxId        = fakeTxIds
        , _thTx          = fakeTx
        , _thDifficulty  = fakeChain
        , _thInputs      = fakeTxOut
        , _thOutputAddrs = outputAddresses
        , _thTimestamp   = fakeTime
        }
  where
    genTxIn :: Gen [TxIn]
    genTxIn = do
        -- Generate a more realistic distribution. After release we have limit
        -- - ~76 inputs in transaction, and it can take up to a month to reach that limit.
        -- In other words, it should be pretty rare to see that stuff.
        numInputs     <- frequency
            [ (70, choose (1, 10))
            , (25, choose (20, 50))
            , (5 , choose (50, 76))
            ]

        vectorOf numInputs arbitrary


    -- | Generate sensible @Tx@.
    genTxs :: [TxOut] -> Gen Tx
    genTxs txOut = do

        _txInputs     <- NE.fromList <$> genTxIn
        let _txOutputs = NE.fromList txOut
        let _txAttributes = mkAttributes ()

        pure $ UnsafeTx {..}

    -- | Generate sensible amount of coins.
    genCoins :: Gen Coin
    genCoins = mkCoin <$> choose (1, 1000)


generateFakeUtxo :: NetworkMagic -> FakeUtxoCoinDistribution -> AccountId -> UberMonad ()
generateFakeUtxo _  NoDistribution _          = pure ()
generateFakeUtxo nm RangeDistribution{..} aId = do

    db <- askWalletDB
    ws <- getWalletSnapshot db

    let fromAddr = range
    -- First let's generate the initial addesses where we will fake money from.
    genCAddresses <- timed $ forM [1..fromAddr] (const $ genAddress nm aId)

    let generatedAddresses = rights $ map unwrapCAddress genCAddresses

    let coinAmount :: Coin
        coinAmount = mkCoin $ fromIntegral amount

    let txsOut :: [TxOutAux]
        txsOut = map (\address -> TxOutAux $ TxOut address coinAmount) generatedAddresses
        utxo = getWalletUtxo ws

    txInTxOutTuple <- liftIO $ sequence [ (,) <$> genTxIn <*> pure txOut | txOut <- txsOut ]
    let newUtxo    = utxo `union` fromList txInTxOutTuple

    setWalletUtxo db newUtxo

    -- Update state
    let mapModifier = utxoToModifier newUtxo
    updateWalletBalancesAndUtxo db mapModifier
  where
    genTxIn :: IO TxIn
    genTxIn = generate $ TxInUtxo <$> arbitrary <*> arbitrary


unwrapCAddress :: CAddress -> Either Text Address
unwrapCAddress = decodeCType . cadId


addAddressesTo :: NetworkMagic -> GenSpec -> AccountId -> UberMonad ()
addAddressesTo nm spec cid = genAddresses nm spec cid


genAccounts :: NetworkMagic -> GenSpec -> (Int, CWallet) -> UberMonad ()
genAccounts nm spec@(walletSpec -> wspec) (idx, wallet) = do
    let accs = accounts wspec
    say $ printf "Generating %d accounts for Wallet %d..." accs idx
    cAccounts <- timed (forM [1..accs] (genAccount nm wallet))
    let cids = map toAccountId cAccounts
    forM_ cids (genAddresses nm spec)


toAccountId :: CAccount -> AccountId
toAccountId CAccount{..} = either (error . toS) id (decodeCType caId)


genAddresses :: NetworkMagic -> GenSpec -> AccountId -> UberMonad ()
genAddresses nm (accountSpec . walletSpec -> aspec) cid = do
    let addrs = addresses aspec
    say $ printf "Generating %d addresses for Account %s..." addrs (renderAccountId cid)
    timed (forM_ [1..addrs] (const $ genAddress nm cid))


-- | Creates a new 'CWallet'.
genWallet :: NetworkMagic -> Integer -> UberMonad CWallet
genWallet nm walletNum = do
    mnemonic  <- newRandomMnemonic
    newWallet nm mempty (walletInit mnemonic)
  where
    walletInit :: BackupPhrase -> CWalletInit
    walletInit backupPhrase = CWalletInit {
      cwInitMeta      = CWalletMeta
          { cwName      = "Wallet #" <> show walletNum
          , cwAssurance = CWANormal
          , cwUnit      = 0
        }
      , cwBackupPhrase  = backupPhrase
      }


-- | Generates a new 'BackupPhrase'.
newRandomMnemonic :: WalletWebMode BackupPhrase
newRandomMnemonic = do

    -- The size 16 should give you 12 words after bip39 encoding.
    let mnemonic :: IO ByteString
        mnemonic = getEntropy 16

    genMnemonic  <- liftIO mnemonic

    let newMnemonic = either (error . show) id (toMnemonic genMnemonic)

    pure $ BackupPhrase $ words newMnemonic


-- | Creates a new 'CAccount'.
genAccount :: NetworkMagic -> CWallet -> Integer -> UberMonad CAccount
genAccount nm CWallet{..} accountNum = do
    newAccountIncludeUnready nm True RandomSeed mempty accountInit
  where
    accountInit :: CAccountInit
    accountInit = CAccountInit {
      caInitMeta = CAccountMeta {
          caName = "Account Number #" <> show accountNum
          }
      , caInitWId  = cwId
      }


-- | Creates a new 'CAddress'.
genAddress :: NetworkMagic -> AccountId -> UberMonad CAddress
genAddress nm cid = do
    let (walletId, addrNum) = (aiWId cid, aiIndex cid)
    newAddress nm RandomSeed mempty (AccountId walletId addrNum)
