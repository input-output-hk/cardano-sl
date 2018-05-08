-- | Wrappers around hspec functionality that uses 'Buildable' instead of
-- 'Show' to better fit in with the rest of the Cardano codebase
--
-- Intended as a drop-in replacement for "Test.HSpec".
module Util.Buildable.Hspec (
    -- * Wrappers around Test.HSpec.Expectations
    shouldSatisfy
  , shouldBe
  , shouldNotBe
  , shouldReturn
  , shouldMatchList
    -- * Working with Validated
  , valid
  , shouldBeValidated
    -- * Re-exports
  , H.Expectation
  , H.Spec
  , H.around
  , H.describe
  , H.hspec
  , H.it
  , H.beforeAll
  , H.parallel
  ) where

import qualified Test.Hspec as H
import           Universum

import           Util.Buildable
import           Util.Validated

{-------------------------------------------------------------------------------
  Wrappers around Test.HSpec.Expectations
-------------------------------------------------------------------------------}

infix 1 `shouldSatisfy`, `shouldBe`, `shouldReturn`, `shouldMatchList`

shouldSatisfy :: (HasCallStack, Buildable a)
              => a -> (a -> Bool) -> H.Expectation
shouldSatisfy a f = H.shouldSatisfy (STB a) (f . unSTB)

shouldBe :: (HasCallStack, Buildable a, Eq a)
         => a -> a -> H.Expectation
shouldBe a a' = H.shouldBe (STB a) (STB a')

shouldNotBe :: (HasCallStack, Buildable a, Eq a)
            => a -> a -> H.Expectation
shouldNotBe a a' = H.shouldNotBe (STB a) (STB a')

shouldReturn :: (HasCallStack, Buildable a, Eq a)
             => IO a -> a -> H.Expectation
shouldReturn act a = H.shouldReturn (STB <$> act) (STB a)

shouldMatchList :: (HasCallStack, Buildable a, Eq a)
                => [a] -> [a] -> H.Expectation
shouldMatchList a b = H.shouldMatchList (map STB a) (map STB b)

{-------------------------------------------------------------------------------
  Wrappers around Validated
-------------------------------------------------------------------------------}

valid :: (HasCallStack, Buildable e, Buildable a)
      => String -> Validated e a -> H.Spec
valid s = H.it s . shouldBeValidated

shouldBeValidated :: (Buildable e, Buildable a)
                  => Validated e a -> H.Expectation
shouldBeValidated ma = shouldSatisfy ma isValidated
