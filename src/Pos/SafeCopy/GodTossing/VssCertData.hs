{-# LANGUAGE TemplateHaskell #-}

-- | VssCertData SafeCopy serialization.

module Pos.SafeCopy.GodTossing.VssCertData
       (
       ) where

import           Data.SafeCopy                  (base, deriveSafeCopySimple)

import           Pos.SafeCopy.GodTossing.Base   ()
import           Pos.Ssc.GodTossing.VssCertData (VssCertData)

deriveSafeCopySimple 0 'base ''VssCertData
