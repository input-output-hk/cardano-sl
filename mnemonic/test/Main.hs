module Main where

import           Universum

import           Test.Hspec

import qualified Cardano.MnemonicSpec


main :: IO ()
main = hspec $ do
    Cardano.MnemonicSpec.spec
