{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import           Data.Typeable (typeRep)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

-- import           Cardano.Wallet.API.V1.Types

import qualified ChainExtension as CE
import qualified Translation as Tr

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
main :: IO ()
main = hspec $ do
    CE.spec
