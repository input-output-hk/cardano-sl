{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import           Test.Hspec

import qualified ChainExtension as CE
import qualified Translation as Tr

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
main :: IO ()
main = hspec $ do
    CE.spec
    Tr.spec
