{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
   An orphanage for all the non-interesting orphan instances.
-}

module Cardano.Wallet.Orphans where

import           Universum

import           Data.SafeCopy

instance SafeCopy Void where
    version         = 0
    kind            = primitive
    objectProfile   = PrimitiveProfile
    errorTypeName _ = "Void"
    putCopy         = absurd
    getCopy         = contain (fail "absurd decoding of Void")
