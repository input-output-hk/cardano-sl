module ChainExtension
  ( spec
  ) where

import Universum
import Test.Pos.Configuration (withDefConfiguration)
import           Test.Hspec.QuickCheck
import Test.Hspec
import Infrastructure.Generator

spec :: Spec
spec = do
  describe "Chain extension verification" $ do
    it "New block must specify previous block as predecessor" $
      undefined

    it "New block does not live in a slot in the past" $
      undefined

    it "Issuer is the slot leader" $
      undefined
