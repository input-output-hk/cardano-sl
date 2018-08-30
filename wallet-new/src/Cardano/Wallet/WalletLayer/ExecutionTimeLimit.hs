{-# LANGUAGE DeriveGeneric #-}
module Cardano.Wallet.WalletLayer.ExecutionTimeLimit (
      limitExecutionTimeTo
    , TimeExecutionLimit(..)
    ) where

import           Universum

import           Formatting (bprint, shown, (%))
import qualified Formatting.Buildable

import           Test.QuickCheck (Arbitrary (..), Gen, Positive (..))

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (AsyncCancelled (..))
import qualified Control.Concurrent.Async as Async
import           Control.Exception (evaluate)
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject,
                     (.:), (.=))
import           Data.Time.Units (Second, fromMicroseconds, toMicroseconds)
import           Generics.SOP.TH (deriveGeneric)


data TimeExecutionLimit =
    TimeExecutionLimitReached Second
    deriving (Generic, Eq, Show)

deriveGeneric ''TimeExecutionLimit

instance Exception TimeExecutionLimit

instance FromJSON TimeExecutionLimit where
    parseJSON =
        withObject "TimeExecutionLimit" $ \v ->
        TimeExecutionLimitReached . fromMicroseconds <$> v .: "TimeExecutionLimitReached"

-- Since we don't have ToJSON and FromJSON instances for Data.Time.Units.Second,
-- we do this manually.
instance ToJSON TimeExecutionLimit where
  toJSON (TimeExecutionLimitReached s) = object [ "TimeExecutionLimitReached" .= (toMicroseconds s)]

instance Buildable TimeExecutionLimit where
    build (TimeExecutionLimitReached secs) =
        bprint ("TimeExecutionLimitReached " % shown) secs

instance Arbitrary TimeExecutionLimit where
    arbitrary = TimeExecutionLimitReached . fromIntegral
                                          . getPositive <$> (arbitrary :: Gen (Positive Int))

limitExecutionTimeTo :: Second
                     -> (TimeExecutionLimit -> b)
                     -> IO (Either b a)
                     -> IO (Either b a)
limitExecutionTimeTo secs onTimeLimit action = do
    let onCancellation = \AsyncCancelled ->
            return $ Left (onTimeLimit (TimeExecutionLimitReached secs))
    let limited = (action >>= evaluate) `catch` onCancellation
    result <- Async.race limited (threadDelay (1000000 * fromIntegral secs))
    case result of
         Left actionResult -> return actionResult
         Right ()          -> return $ Left (onTimeLimit (TimeExecutionLimitReached secs))

