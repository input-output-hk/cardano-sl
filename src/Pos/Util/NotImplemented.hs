{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Instances and other stuff for 'NotImplemented'. We don't want GHC to
-- emit warnings about them, so we have to put them into a separate module.
module Pos.Util.NotImplemented
       (
       ) where

import           Data.Binary   (Binary)
import           Data.Hashable (Hashable)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Universum


instance Binary NotImplemented
instance Hashable NotImplemented

deriveSafeCopySimple 0 'base ''NotImplemented
