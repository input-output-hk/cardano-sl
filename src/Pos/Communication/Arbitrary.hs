-- | Arbitrary instances for Communication types.

module Pos.Communication.Arbitrary () where

import           Test.QuickCheck                    (Arbitrary (..))
import           Universum

import           Pos.Communication.Types.Delegation (ConfirmProxySK (..),
                                                     CheckProxySKConfirmed (..),
                                                     CheckProxySKConfirmedRes (..),
                                                     SendProxySK (..))
import           Pos.Communication.Types.SysStart   (SysStartRequest (..),
                                                     SysStartResponse (..))

instance Arbitrary SendProxySK where
    arbitrary = SendProxySK <$> arbitrary

instance Arbitrary ConfirmProxySK where
    arbitrary = ConfirmProxySK <$> arbitrary <*> arbitrary

instance Arbitrary CheckProxySKConfirmed where
    arbitrary = CheckProxySKConfirmed <$> arbitrary

instance Arbitrary CheckProxySKConfirmedRes where
    arbitrary = CheckProxySKConfirmedRes <$> arbitrary

instance Arbitrary SysStartRequest where
    arbitrary = pure SysStartRequest

instance Arbitrary SysStartResponse where
    arbitrary = SysStartResponse <$> arbitrary <*> arbitrary
