{-# LANGUAGE TypeFamilies #-}

module Pos.DB.Ssc.State
       ( mkSscState
       , module Pos.DB.Ssc.State.Global
       , module Pos.DB.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM

import           Pos.Chain.Ssc (SscState (..))
import           Pos.Core.Slotting (MonadSlots)
import           Pos.DB (MonadDBRead)
import           Pos.Util.Trace.Named (TraceNamed)

-- Reexports
import           Pos.DB.Ssc.State.Global
import           Pos.DB.Ssc.State.Local

mkSscState
    :: forall ctx m .
       ( MonadDBRead m
       , MonadSlots ctx m
       )
    => TraceNamed m
    -> m SscState
mkSscState logTrace = do
    gState <- sscLoadGlobalState logTrace
    ld <- sscNewLocalData
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
