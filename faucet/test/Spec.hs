{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Universum

import           Cardano.Faucet.Types
import           Cardano.Faucet.Types.Recaptcha
import           Cardano.WalletClient

main :: IO ()
main = hspec $ do
    describe "randomAmount" $ do
        it "should be within PaymentDistribution" $ do
            property $ prop_randomAmount

    describe "Aeson encode and decode" $ do
        it "CreatedWallet" $ property (prop_aeson_id :: CreatedWallet -> Bool)
        it "WithdrawalRequest" $ property (prop_aeson_id :: WithdrawalRequest -> Bool)
        it "CaptchaResponse decode" $ captchaDecode

    describe "Reading the recaptcha secret from a file" $ do
        it "Should read from a file that exists" $ do
            readCaptchaSecret "test/valid-secret.txt" `shouldReturn` CaptchaSecret "test"
        it "Should throw ReadRecaptchaSecretError if the file doesn't have one line" $ do
            readCaptchaSecret "test/multi-line-secret.txt"
            `shouldThrow`
            (== MoreThanOneLine "test/multi-line-secret.txt")

prop_randomAmount :: PaymentDistribution -> Property
prop_randomAmount pd = monadicIO $ do
    c <- run (liftIO $ randomAmount pd)
    assert (between pd c)
  where
    between (PaymentDistribution m s) c = c <= m + s
                                      && c >= m - s

prop_aeson_id :: (Arbitrary a, ToJSON a, FromJSON a, Eq a) => a -> Bool
prop_aeson_id a = Just a == (decode $ encode a)


t :: UTCTime
t = posixSecondsToUTCTime 1527811200

getJSON :: FromJSON a => FilePath -> IO (Either String a)
getJSON f = eitherDecode <$> BSL.readFile f

captchaDecode :: Expectation
captchaDecode = do
    getJSON "test/captcha-resp-ok.json" `shouldReturn` Right (CaptchaResponse True (Just t) (Just "test") [])
