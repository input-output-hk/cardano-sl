{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Type which is instance of all SSC classes.

module Pos.Ssc.GodTossing.Types.Type
       ( SscGodTossing
       ) where

import           Universum

-- | Data type which represents shared seed calculation tag
-- in -XTypeApplication hacks with type families.
data SscGodTossing
    deriving (Generic)

deriving instance Show SscGodTossing
deriving instance Eq SscGodTossing
