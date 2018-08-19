{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module AccountSpecs (accountSpecs) where

import           Universum

import           Cardano.Wallet.API.Indices (accessIx)
import           Cardano.Wallet.Client.Http
import           Control.Lens
import           Pos.Core.Common (mkCoin)
import           Test.Hspec
import           Util

import qualified Pos.Core as Core
import qualified Prelude


accountSpecs :: WalletRef -> WalletClient IO -> Spec
accountSpecs _ wc =
    describe "Accounts" $ do
        it "can retrieve only an account's balance" $ do
            let zero = V1 (mkCoin 0)
            (Wallet{..}, Account{..}) <- randomAccount wc
            eresp <- getAccountBalance wc walId accIndex

            partialAccount <- wrData <$> eresp `shouldPrism` _Right
            partialAccount `shouldBe` AccountBalance zero

        it "can retrieve only an account's addresses" $ do
            pair@(Wallet{..}, Account{..}) <- randomAccount wc
            addresses <- createAddresses wc 10 pair
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
                eresp <- getAccountAddresses wc walId accIndex page perPage filters
                expectations . acaAddresses . wrData =<< eresp `shouldPrism` _Right
  where
    filterByAddress :: WalletAddress -> FilterOperations '[V1 Address] WalletAddress
    filterByAddress addr =
        FilterOp (FilterByIndex $ accessIx @_ @(V1 Core.Address) addr) NoFilters

    expectNAddresses :: Int -> [WalletAddress] -> IO ()
    expectNAddresses n addrs =
        length addrs `shouldBe` n

    expectExactlyAddresses :: [WalletAddress] -> [WalletAddress] -> IO ()
    expectExactlyAddresses as bs =
        sort as `shouldBe` sort bs
