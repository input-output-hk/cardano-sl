{-# LANGUAGE TypeFamilies #-}

-- | Types for Shared Seed calculation.

module Pos.Ssc.Class.Types
       ( Ssc(..)
       ) where

import           Data.Text.Buildable (Buildable)

import           Pos.Binary.Class    (Bi)
import           Pos.Ssc.Core.Types  (SscPayload(..), SscProof(..))

-- | Main Shared Seed Calculation type class. Stores all needed type
-- parameters for general implementation of SSC.
class ( Buildable SscVerifyError
      , Bi SscProof
      , Bi SscPayload
      ) =>
      Ssc where

    -- | Type for verification error
    type SscVerifyError
