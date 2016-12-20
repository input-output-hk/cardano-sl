{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Re-exports of Pos.Ssc.Class.* modules.

module Pos.Ssc.Class
       ( module Class
       , SscConstraint
       ) where


import           Data.Default            (Default)
import           Pos.Ssc.Class.Helpers   as Class
import           Pos.Ssc.Class.Listeners as Class
import           Pos.Ssc.Class.LocalData as Class
import           Pos.Ssc.Class.Storage   as Class
import           Pos.Ssc.Class.Types     as Class
import           Pos.Ssc.Class.Workers   as Class

type SscConstraint ssc =
               (Ssc ssc,
                Default (SscStorage ssc),
                SscListenersClass ssc,
                SscLocalDataClass ssc,
                SscLocalDataClassM ssc,
                SscHelpersClass ssc,
                SscHelpersClassM ssc,
                SscStorageMode ssc,
                SscWorkersClass ssc)
