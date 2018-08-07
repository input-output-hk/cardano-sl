{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Aeson orphan instances for core

module Pos.Core.Aeson.Orphans
       (
       ) where

import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Time.Units (Microsecond, Millisecond, Second)

deriveJSON S.defaultOptions ''Millisecond
deriveJSON S.defaultOptions ''Microsecond
deriveJSON S.defaultOptions ''Second
