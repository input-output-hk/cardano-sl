module RequestSpec (spec) where

import           Universum

import           Data.Either (isLeft)
import           Formatting (build, sformat)
import           Test.Hspec

import           Cardano.Wallet.API.Request.Filter
import           Cardano.Wallet.API.Request.Sort
import           Cardano.Wallet.API.V1.Types
import qualified Pos.Core as Core

spec :: Spec
spec = describe "Request" $ do
    describe "Sort" sortSpec
    describe "Filter" filterSpec

sortSpec :: Spec
sortSpec =
    describe "parseSortOperation" $ do
        describe "Transaction" $ do
            let ptimestamp = Proxy @(V1 Core.Timestamp)
                pt = Proxy @Transaction

            it "knows the query param" $ do
                parseSortOperation pt ptimestamp "ASC[created_at]"
                    `shouldBe`
                        Right (SortByIndex SortAscending ptimestamp)

            it "infers DESC for nonspecified sort" $
                parseSortOperation pt ptimestamp "created_at"
                    `shouldBe`
                        Right (SortByIndex SortDescending ptimestamp)

            it "fails if the param name is wrong" $ do
                parseSortOperation pt ptimestamp "ASC[balance]"
                    `shouldSatisfy`
                        isLeft

            it "fails if the syntax is wrong" $ do
                parseSortOperation pt ptimestamp "ASC[created_at"
                    `shouldSatisfy`
                        isLeft

filterSpec :: Spec
filterSpec = do
    describe "parseFilterOperation" $ do
        describe "Wallet" $ do
            let pw = Proxy @Wallet
                pwid = Proxy @WalletId
                pcoin = Proxy @Core.Coin

            it "supports index" $ do
                parseFilterOperation pw pwid "asdf"
                    `shouldBe`
                        Right (FilterByIndex (WalletId "asdf"))

            forM_ [minBound .. maxBound] $ \p ->
                it ("supports predicate: " <> show p) $ do
                    parseFilterOperation pw pwid
                        (sformat build p <> "[asdf]")
                        `shouldBe`
                            Right (FilterByPredicate p (WalletId "asdf"))

            it "supports range" $ do
                parseFilterOperation pw pcoin "RANGE[123,456]"
                    `shouldBe`
                        Right
                            (FilterByRange (Core.mkCoin 123)
                                           (Core.mkCoin 456))

            it "fails if the thing can't be parsed" $ do
                parseFilterOperation pw pcoin "nope"
                    `shouldSatisfy`
                        isLeft

            it "supports IN" $ do
                parseFilterOperation pw pcoin "IN[1,2,3]"
                    `shouldBe`
                        Right
                            (FilterIn (map Core.mkCoin [1,2,3]))

    describe "toQueryString" $ do
        let ops = FilterByRange (Core.mkCoin 2345) (Core.mkCoin 2348)
                `FilterOp` FilterByIndex (WalletId "hello")
                `FilterOp` NoFilters
                :: FilterOperations Wallet
        it "does what you'd want it to do" $ do
            toQueryString ops
                `shouldBe`
                    [ ("balance", Just "RANGE[2345,2348]")
                    , ("id",      Just "hello")
                    ]

    describe "toFilterOperations" $ do
        let params :: [(Text, Maybe Text)]
            params =
                [ ("id",      Just "3")
                , ("balance", Just "RANGE[10,50]")
                ]
            fops :: FilterOperations Wallet
            fops = FilterByIndex (WalletId "3")
                `FilterOp` FilterByRange (Core.mkCoin 10) (Core.mkCoin 50)
                `FilterOp` NoFilters
            prxy :: Proxy '[WalletId, Core.Coin]
            prxy = Proxy

        it "can parse the thing" $ do
            toFilterOperations params prxy
                `shouldBe`
                    fops
