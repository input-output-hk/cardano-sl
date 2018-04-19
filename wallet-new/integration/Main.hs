{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens hiding ((^..), (^?))
import           Data.Map (fromList)
import           Data.Traversable (for)
import qualified Pos.Core as Core
import           System.IO (hSetEncoding, stdout, utf8)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           CLI
import           Functions
import           Types

-- | Here we want to run main when the (local) nodes
-- have started.
main :: IO ()
main = do

    hSetEncoding stdout utf8
    CLOptions {..} <- getOptions

    _pubCert <- readFile tlsPubCertPath
    _privKey <- readFile tlsPrivKeyPath
    -- stateless

    -- TODO (akegalj): run server cluster in haskell, instead of using shell scripts
    -- serverThread <- async (runWalletServer options)

    printT "Starting the integration testing for wallet."

    when stateless $ do
        printT "The wallet test node is running in stateless mode."
        printT "Stateless mode not implemented currently!"

    -- TODO (akegalj): move these to CLOptions
    let baseUrl = BaseUrl Http "localhost" 8090 mempty
    manager <- newManager defaultManagerSettings

    let walletClient :: MonadIO m => WalletClient m
        walletClient = liftClient $ mkHttpClient baseUrl manager

    walletState <- initialWalletState walletClient

    printT $ "Initial wallet state: " <> show walletState

    -- some monadic fold or smth similar
    _ <- runActionCheck
        walletClient
        walletState
        actionDistribution

    hspec $ deterministicTests walletClient
  where
    actionDistribution :: ActionProbabilities
    actionDistribution = do
        (PostWallet, Weight 2)
            :| (PostTransaction, Weight 5)
            : fmap (\x -> (x, Weight 1)) [minBound .. maxBound]

initialWalletState :: WalletClient IO -> IO WalletState
initialWalletState wc = do
    -- We will have single genesis wallet in intial state that was imported from launching script
    _wallets <- fromResp $ getWallets wc
    _accounts <- concat <$> for _wallets (fromResp . getAccounts wc . walId)
    -- Lets set all wallet passwords for initial wallets (genesis) to default (emptyPassphrase)
    let _lastAction       = NoOp
        _walletsPass      = fromList $ map ((, V1 mempty) . walId) _wallets
        _addresses        = concatMap accAddresses _accounts
        -- TODO(akegalj): I am not sure does importing a genesis wallet (which we do prior launching integration tests) creates a transaction
        -- If it does, we should add this transaction to the list
        _transactions     = mempty
        _actionsNum       = 0
        _successActions   = mempty
    pure $ WalletState {..}
  where
    fromResp = (either throwM (pure . wrData) =<<)

deterministicTests :: WalletClient IO -> Spec
deterministicTests wc = do
    describe "Addresses" $ do
        it "Creating an address makes it available" $ do
            -- create a wallet
            Wallet{..} <- sampleWallet

            -- create an account
            accResp <- postAccount wc walId (NewAccount Nothing "hello")
            acc@Account{..} <- wrData <$> accResp `shouldPrism` _Right

            -- accounts should exist
            accResp' <- getAccounts wc walId
            accs <- wrData <$> accResp' `shouldPrism` _Right
            accs `shouldContain` [acc]

            -- create an address
            addResp <- postAddress wc (NewAddress Nothing accIndex walId)
            addr <- wrData <$> addResp `shouldPrism` _Right

            -- verify that address is in the API
            idxResp <- getAddressIndex wc
            addrs <- wrData <$> idxResp `shouldPrism` _Right

            map addrId addrs `shouldContain` [addrId addr]

        it "Index returns real data" $ do
            addrsResp <- getAddressIndex wc
            addrs <- wrData <$> addrsResp `shouldPrism` _Right

            addrsResp' <- getAddressIndex wc
            addrs' <- wrData <$> addrsResp' `shouldPrism` _Right

            addrs `shouldBe` addrs'

    describe "Wallets" $ do
        it "Creating a wallet makes it available." $ do
            newWallet <- randomWallet
            Wallet{..} <- createWalletCheck newWallet

            eresp <- getWallet wc walId
            void $ eresp `shouldPrism` _Right

        it "Updating a wallet persists the update" $ do
            newWallet <- randomWallet
            wallet <- createWalletCheck newWallet
            let newName = "Foobar Bazquux"
                newAssurance = NormalAssurance
            eupdatedWallet <- updateWallet wc (walId wallet) WalletUpdate
                { uwalName = newName
                , uwalAssuranceLevel = newAssurance
                }
            Wallet{..} <- wrData <$> eupdatedWallet `shouldPrism` _Right
            walName `shouldBe` newName
            walAssuranceLevel `shouldBe` newAssurance

    describe "Transactions" $ do
        it "posted transactions appear in the index" $ do
            genesis <- genesisWallet
            (fromAcct, _) <- firstAccountAndId genesis

            wallet <- sampleWallet
            (toAcct, toAddr) <- firstAccountAndId wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = halfOf (accAmount fromAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                halfOf (V1 c) = V1 (Core.mkCoin (Core.getCoin c `div` 2))

            etxn <- postTransaction wc payment

            txn <- fmap wrData etxn `shouldPrism` _Right

            eresp <- getTransactionIndex wc (Just (walId wallet)) (Just (accIndex toAcct)) Nothing
            resp <- fmap wrData eresp `shouldPrism` _Right

            map txId resp `shouldContain` [txId txn]

        it "estimate fees of a well-formed transaction" $ do
            ws <- (,)
                <$> (randomWallet >>= createWalletCheck)
                <*> (randomWallet >>= createWalletCheck)

            ((fromAcct, _), (_toAcct, toAddr)) <- (,)
                <$> firstAccountAndId (fst ws)
                <*> firstAccountAndId (snd ws)

            let amount = V1 (Core.mkCoin 42)

            let payment = Payment
                    { pmtSource = PaymentSource
                        { psWalletId = walId (fst ws)
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = amount
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

            efee <- getTransactionFee wc payment
            fee <- fmap (feeEstimatedAmount . wrData) efee `shouldPrism` _Right
            fee `shouldSatisfy` (> amount)

        it "fails if you spend too much money" $ do
            wallet <- sampleWallet
            (toAcct, toAddr) <- firstAccountAndId wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId wallet
                        , psAccountIndex = accIndex toAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = tooMuchCash (accAmount toAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                tooMuchCash (V1 c) = V1 (Core.mkCoin (Core.getCoin c * 2))
            etxn <- postTransaction wc payment

            void $ etxn `shouldPrism` _Left

  where
    randomWallet =
        generate $
            NewWallet
                <$> arbitrary
                <*> pure Nothing
                <*> arbitrary
                <*> pure "Wallet"
                <*> pure CreateWallet

    createWalletCheck newWallet = do
        result <- fmap wrData <$> postWallet wc newWallet
        result `shouldPrism` _Right

    firstAccountAndId wallet = do
        etoAccts <- getAccounts wc (walId wallet)
        toAccts <- fmap wrData etoAccts `shouldPrism` _Right

        toAccts `shouldSatisfy` (not . null)
        let (toAcct : _) = toAccts

        accAddresses toAcct `shouldSatisfy` (not . null)
        let (toAddr : _) = accAddresses toAcct

        pure (toAcct, toAddr)

    -- this is a "Safe' usage of `unsafePerformIO`. if it's too gross then
    -- I can delete it.
    walletRef :: MVar Wallet
    walletRef = unsafePerformIO newEmptyMVar
    {-# NOINLINE walletRef #-}

    sampleWallet :: IO Wallet
    sampleWallet = do
        mwallet <- tryTakeMVar walletRef
        case mwallet of
            Just wallet -> do
                putMVar walletRef wallet
                pure wallet
            Nothing -> do
                w <- randomWallet
                w' <- createWalletCheck w
                didWrite <- tryPutMVar walletRef w'
                if didWrite
                    then pure w'
                    else readMVar walletRef

    genesisWallet :: IO Wallet
    genesisWallet = do
        mwallet <- tryTakeMVar genesisRef
        case mwallet of
            Just wallet -> do
                putMVar genesisRef wallet
                pure wallet
            Nothing -> do
                Right allWallets <- fmap wrData <$> getWallets wc
                wallet <- maybe
                    (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
                    return
                    (find (("Genesis wallet" ==) . walName) allWallets)
                didWrite <- tryPutMVar genesisRef wallet
                if didWrite
                    then pure wallet
                    else readMVar genesisRef

    genesisRef :: MVar Wallet
    genesisRef = unsafePerformIO newEmptyMVar
    {-# NOINLINE genesisRef #-}

shouldPrism :: Show s => s -> Prism' s a -> IO a
shouldPrism a b = do
    a `shouldSatisfy` has b
    let Just x = a ^? b
    pure x

infixr 8 `shouldPrism`
