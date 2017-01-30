-- | This module re-exports functionality from Pos.Ssc.Extra.* modules.
-- Pos.Ssc.Extra provides SSC functionality independent of particular SSC.

module Pos.Ssc.Extra
       ( module Pos.Ssc.Extra.Class
       , module Pos.Ssc.Extra.Logic
       , module Pos.Ssc.Extra.Holder
       , module Pos.Ssc.Extra.Types
       ) where

import           Pos.Ssc.Extra.Class
import           Pos.Ssc.Extra.Holder
import           Pos.Ssc.Extra.Logic
import           Pos.Ssc.Extra.Types
