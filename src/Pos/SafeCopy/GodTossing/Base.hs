{-# LANGUAGE TemplateHaskell #-}

-- | SafeCopy serialization of base types from GodTossing.

module Pos.SafeCopy.GodTossing.Base
       (
       ) where

import           Data.SafeCopy                 (base, deriveSafeCopySimple)

import           Pos.SafeCopy.Types            ()
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), CommitmentsMap,
                                                Opening (..), VssCertificate (..))

deriveSafeCopySimple 0 'base ''VssCertificate
deriveSafeCopySimple 0 'base ''Opening
deriveSafeCopySimple 0 'base ''Commitment
deriveSafeCopySimple 0 'base ''CommitmentsMap
