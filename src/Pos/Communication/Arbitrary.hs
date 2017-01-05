{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Communication types.

module Pos.Communication.Arbitrary () where

import           Data.DeriveTH                    (derive, makeArbitrary)
import           Test.QuickCheck                  (Arbitrary (..))
import           Universum

import           Pos.Communication.Types.SysStart (SysStartRequest (..),
                                                   SysStartResponse (..))

derive makeArbitrary ''SysStartRequest
derive makeArbitrary ''SysStartResponse
