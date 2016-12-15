{-# LANGUAGE DeriveGeneric #-}

-- | Type which is instance of all SSC classes.

module Pos.Modern.Ssc.GodTossing.Types.Type
       ( SscGodTossing
       ) where

import           Universum

-- | Data type which represents shared seed calculation tag
-- in -XTypeApplication hacks with type families.
data SscGodTossing
    deriving (Generic)
