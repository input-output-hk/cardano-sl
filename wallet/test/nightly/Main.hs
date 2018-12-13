module Main where

import           Universum

import           Test.Hspec (hspec)

import qualified TxMetaStorage


main :: IO ()
main = hspec $ TxMetaStorage.spec
