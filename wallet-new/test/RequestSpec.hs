module RequestSpec where

import           Universum

import           Data.Char (isPrint)
import           Data.Either (isLeft)
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
import           Test.QuickCheck

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
                parse = parseFilterOperation pw pwid

            it "supports index" $ do
                parseFilterOperation pw pwid "asdf"
                    `shouldBe`
                        Right (FilterByIndex (WalletId "asdf"))

            forM_ [minBound .. maxBound] $ \pred ->
                it ("supports predicate: " <> show pred) $ do
                    parseFilterOperation pw pwid
                        (renderFilterOrdering pred <> "[asdf]")
                        `shouldBe`
                            Right (FilterByPredicate pred (WalletId "asdf"))

            it "supports range" $ do
                parse "RANGE[hello,world]"
                    `shouldBe`
                        Right
                            (FilterByRange (WalletId "hello")
                                           (WalletId "world"))

            it "fails if the thing can't be parsed" $ do
                parseFilterOperation pw pcoin "nope"
                    `shouldSatisfy`
                        isLeft
  where
    text'preds =
        map (\x -> (renderFilterOrdering x, x)) [minBound .. maxBound]
-- Vendored from quickcheck. Can discard when QuickCheck 2.10 is in use.

--------------------------------------------------------------------------
-- | @PrintableString@: generates a printable unicode String.
-- The string will not contain surrogate pairs.
newtype PrintableString = PrintableString {getPrintableString :: String}
  deriving ( Eq, Ord, Show, Read )

instance Arbitrary PrintableString where
  arbitrary = PrintableString `fmap` listOf arbitraryPrintableChar
  shrink (PrintableString xs) = PrintableString `fmap` shrink xs

-- | Generates a printable Unicode character.
arbitraryPrintableChar :: Gen Char
arbitraryPrintableChar = arbitrary `suchThat` isPrint
