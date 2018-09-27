{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module AccountSpecs (accountSpecs) where

import           Universum

import           Cardano.Wallet.API.Indices (accessIx)
import           Cardano.Wallet.Client.Http
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Lens
import           Functions (randomTest)
import           Pos.Core.Common (mkCoin)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, shuffle)
import           Test.QuickCheck.Monadic (pick, run)

import           Util

import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core
import qualified Prelude


accountSpecs :: WalletRef -> WalletClient IO -> Spec
accountSpecs wRef wc =

    describe "Accounts" $ do

        randomTest "can retrieve only an account's balance" 1 $ do
            let zero = V1 (mkCoin 0)
            (Wallet{..}, Account{..}) <- run $ randomAccount wc
            eresp <- run $ getAccountBalance wc walId accIndex

            partialAccount <- run $ wrData <$> eresp `shouldPrism` _Right
            liftIO $ partialAccount `shouldBe` AccountBalance zero

        randomTest "can retrieve only an account's addresses" 1 $ do
            pair@(Wallet{..}, Account{..}) <- run $ randomAccount wc
            addresses <- run $ createAddresses wc 10 pair
            let addr = Prelude.head addresses
            let tests =
                    [ PaginationTest (Just 1) (Just 5) NoFilters NoSorts
                        (expectNAddresses 5)
                    , PaginationTest (Just 1) (Just 5) (filterByAddress addr) NoSorts
                        (expectExactlyAddresses [addr])
                    , PaginationTest (Just 2) (Just 5) (filterByAddress addr) NoSorts
                        (expectExactlyAddresses [])
                    ]

            forM_ tests $ \PaginationTest{..} -> do
                eresp <- run $ getAccountAddresses wc walId accIndex page perPage filters
                liftIO $ expectations . acaAddresses . wrData =<< eresp `shouldPrism` _Right

        randomTest ("can retrieve initial and updated balances of several accounts from getAccountBalances"
              <> "that are equivalent to what is obtained from getAccounts") 1 $ do
            genesis <- run $ genesisWallet wc
            (fromAcct, _) <- run $ firstAccountAndId wc genesis

            wallet <- run $ sampleWallet wRef wc
            -- We create 4 accounts, plus one is created automatically
            -- by the 'sampleWallet', for a total of 5.
            randomNewAccount <- forM [1..4] $ \(_i :: Int) ->
                pick (arbitrary @NewAccount)
            forM_ randomNewAccount $ \(rAcc :: NewAccount) ->
                run $ postAccount wc (walId wallet) rAcc

            accResp' <- run $ getAccounts wc (walId wallet)
            accs <- run $ wrData <$> accResp' `shouldPrism` _Right

            balancesPartialResp' <- forM (map accIndex accs) $ \(accIndex :: AccountIndex) ->
                run $ getAccountBalance wc (walId wallet) accIndex

            balancesPartial <- run $ mapM (\resp -> wrData <$> resp `shouldPrism` _Right) balancesPartialResp'

            liftIO $ map (AccountBalance . accAmount) accs `shouldBe` balancesPartial

            -- Now transfering money to 5 accounts from genesis wallet and checking balances once again
            let payment amount toAddr = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = V1 (Core.mkCoin amount)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
            amounts <- pick $ shuffle [1..5]
            let addrAndAmount = zip (map (\(addr : _) -> addr) $ map accAddresses accs) amounts
            forM_  addrAndAmount $ \(addr, amount) ->
                run $ postTransaction wc (payment amount addr)

            liftIO $ threadDelay 90000000

            accUpdatedResp' <- run $ getAccounts wc (walId wallet)
            accsUpdated <- run $ wrData <$> accUpdatedResp' `shouldPrism` _Right

            balancesPartialUpdatedResp' <- run $ forM (map accIndex accsUpdated) $
                \(accIndex :: AccountIndex) -> getAccountBalance wc (walId wallet) accIndex

            balancesPartialUpdated <-
                run $ mapM (\resp -> wrData <$> resp `shouldPrism` _Right) balancesPartialUpdatedResp'

            liftIO $ map (AccountBalance . accAmount) accsUpdated `shouldBe` balancesPartialUpdated


        randomTest "redeeming avvm key gives rise to the corresponding increase of balance of wallet'account - mnemonic not used" 1 $ do

            newWallet <- run $ randomWallet CreateWallet
            Wallet{..} <- run $ createWalletCheck wc newWallet

            --adding new account
            rAcc <- pick (arbitrary @NewAccount)
            newAcctResp <- run $ postAccount wc walId rAcc
            newAcct <- run $ wrData <$> newAcctResp `shouldPrism` _Right

            balancePartialResp0 <- run $ getAccountBalance wc walId (accIndex newAcct)
            balancesPartial0 <- run $ wrData <$> balancePartialResp0 `shouldPrism` _Right
            let zeroBalance = AccountBalance $ V1 (Core.mkCoin 0)
            liftIO $ balancesPartial0 `shouldBe` zeroBalance

            -- state-demo/genesis-keys/keys-fakeavvm/fake-9.seed
            let avvmKey = "QBYOctbb6fJT/dBDLwg4je+SAvEzEhRxA7wpLdEFhnY="

            --password is set to Nothing in the current implementation of randomWallet
            --when it changes redemptionSpendingPassword handles it, otherwise passPhare addresses it
            let redemption = Redemption
                             { redemptionRedemptionCode =
                                     ShieldedRedemptionCode avvmKey
                             , redemptionMnemonic = Nothing
                             , redemptionSpendingPassword =
                                     case newwalSpendingPassword newWallet of
                                         Just spPassw -> spPassw
                                         Nothing      -> mempty
                             , redemptionWalletId = walId
                             , redemptionAccountIndex = accIndex newAcct
                             }

            etxn <- run $ redeemAda wc redemption

            txn <- run $ fmap wrData etxn `shouldPrism` _Right

            --checking if redemption give rise to transaction indexing
            let poll = pollTransactions walId (accIndex newAcct) (txId txn)
            run $ ("waiting for Tx to be accepted", poll) `noLongerThan` (120 * oneSecond)

            --balance for the previously zero-balance account should increase by 100000
            balancePartialResp <- run $ getAccountBalance wc walId (accIndex newAcct)
            balancesPartial <- run $ wrData <$> balancePartialResp `shouldPrism` _Right
            let nonzeroBalance = AccountBalance $ V1 (Core.mkCoin 100000)
            liftIO $ balancesPartial `shouldBe` nonzeroBalance

            --redeemAda for the same redeem address should result in error
            etxnAgain <- run $ redeemAda wc redemption

            clientError <- run $ etxnAgain `shouldPrism` _Left
            liftIO $ clientError
                `shouldBe`
                    ClientWalletError TxRedemptionDepleted
  where
    pollTransactions :: WalletId -> AccountIndex -> V1 Txp.TxId -> IO ()
    pollTransactions wid cid tid = do
        resp <- getTransactionIndex wc (Just wid) (Just cid) Nothing
            >>= shouldPrismFlipped _Right
        let Just tx = find ((== tid) . txId) (wrData resp)
        if (txStatus tx `elem` [InNewestBlocks, Persisted]) then
            return ()
        else
            threadDelay oneSecond >> pollTransactions wid cid tid

    oneSecond :: Int
    oneSecond = 1000000

    noLongerThan :: (String, IO a) -> Int -> IO a
    noLongerThan (msg, action) maxWaitingTime = do
        res <- race (threadDelay maxWaitingTime) action
        case res of
            Left _  -> fail ("Waiting too long for action: " <> msg)
            Right a -> return a

    filterByAddress :: WalletAddress -> FilterOperations '[V1 Address] WalletAddress
    filterByAddress addr =
        FilterOp (FilterByIndex $ accessIx @_ @(V1 Core.Address) addr) NoFilters

    expectNAddresses :: Int -> [WalletAddress] -> IO ()
    expectNAddresses n addrs =
        length addrs `shouldBe` n

    expectExactlyAddresses :: [WalletAddress] -> [WalletAddress] -> IO ()
    expectExactlyAddresses as bs =
        sort as `shouldBe` sort bs
