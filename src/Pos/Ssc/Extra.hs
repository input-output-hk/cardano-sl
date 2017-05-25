-- | This module re-exports functionality from Pos.Ssc.Extra.* modules.
-- Pos.Ssc.Extra provides SSC functionality independent of particular SSC.

-- TODO: reenable this when all of Pos.Ssc.Extra is moved to ssc/

-- {-# OPTIONS_GHC -F -pgmF autoexporter #-}

module Pos.Ssc.Extra
       ( module X
       ) where

import           Pos.Ssc.Extra.Class  as X
import           Pos.Ssc.Extra.Holder as X
import           Pos.Ssc.Extra.Logic  as X
import           Pos.Ssc.Extra.Types  as X
