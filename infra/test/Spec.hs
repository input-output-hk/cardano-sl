module Spec
    ( spec
    ) where

import           Test.Hspec
import qualified Test.Pos.Infra.Diffusion.Subscription.SubscriptionSpec (spec)

spec :: Spec
spec = Test.Pos.Infra.Diffusion.Subscription.SubscriptionSpec.spec
