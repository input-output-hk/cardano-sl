{-# LANGUAGE ConstraintKinds #-}

-- | Re-exports of Pos.Ssc.Class.* modules.

module Pos.Ssc.Class
       ( module Pos.Ssc.Class.Helpers
       , module Pos.Ssc.Class.Listeners
       , module Pos.Ssc.Class.LocalData
       , module Pos.Ssc.Class.Storage
       , module Pos.Ssc.Class.Types
       , module Pos.Ssc.Class.Workers

       , SscConstraint
       , WorkModeSsc
       ) where


import           Pos.Ssc.Class.Helpers
import           Pos.Ssc.Class.Listeners
import           Pos.Ssc.Class.LocalData
import           Pos.Ssc.Class.Storage
import           Pos.Ssc.Class.Types
import           Pos.Ssc.Class.Workers

import           Pos.Security            (SecurityWorkersClass)

type WorkModeSsc ssc =
    ( SscLocalDataClass ssc
    , SscHelpersClass ssc
    )

type SscConstraint ssc =
    ( Ssc ssc
    , SscListenersClass ssc
    , SscLocalDataClass ssc
    , SscHelpersClass ssc
    , SscStorageClass ssc
    , SscWorkersClass ssc
    , SecurityWorkersClass ssc
    )
