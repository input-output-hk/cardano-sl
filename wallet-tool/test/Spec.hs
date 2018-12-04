module Main where

import Universum

import           Test.Hspec

import qualified WaitSpec

main :: IO ()
main = do
    hspec $ WaitSpec.spec
