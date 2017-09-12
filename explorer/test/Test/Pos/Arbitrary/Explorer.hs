
-- | Arbitrary instances for typse in `cardano-sl-explorer`

module Test.Pos.Arbitrary.Explorer
    (

    ) where

import           Universum

import           Pos.Explorer.Socket                (NotifierSettings (..))
import           System.Wlog                        (LoggerName(..))

import           Test.QuickCheck                    (Arbitrary (..))

instance Arbitrary NotifierSettings where
    arbitrary = NotifierSettings <$> arbitrary

instance Arbitrary LoggerName where
    arbitrary = LoggerName <$> arbitrary
