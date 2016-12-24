{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Re-exports of Pos.Ssc.Class.* modules.

module Pos.Ssc.Class
       ( module Class
       , SscConstraint
       ) where


import           Pos.Ssc.Class.Helpers   as Class
import           Pos.Ssc.Class.Listeners as Class
import           Pos.Ssc.Class.LocalData as Class
import           Pos.Ssc.Class.Storage   as Class
import           Pos.Ssc.Class.Types     as Class
import           Pos.Ssc.Class.Workers   as Class

import           Pos.Security            (SecurityWorkersClass)

type SscConstraint ssc =
               (Ssc ssc,
                SscListenersClass ssc,
                SscLocalDataClass ssc,
                SscHelpersClass ssc,
                SscStorageClass ssc,
                SscWorkersClass ssc,
                SecurityWorkersClass ssc)
