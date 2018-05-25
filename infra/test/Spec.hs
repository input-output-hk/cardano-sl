module Spec
    ( spec
    ) where

import           Test.Hspec
import qualified Test.Pos.Infra.Diffusion.Subscription.StatusSpec (spec)
import qualified Test.Pos.Infra.Diffusion.Subscription.SubscriptionSpec (spec)

spec :: Spec
spec = describe "Subscription" $ do
    Test.Pos.Infra.Diffusion.Subscription.StatusSpec.spec
    Test.Pos.Infra.Diffusion.Subscription.SubscriptionSpec.spec
