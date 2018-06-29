{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.State
       ( mkSscState
       , module Pos.Ssc.State.Global
       , module Pos.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
--import           Pos.Util.Log (WithLogger)

import           Pos.DB (MonadDBRead)
import           Pos.Infra.Slotting.Class (MonadSlots)
import           Pos.Ssc.Types (SscState (..))

-- Reexports
import           Pos.Ssc.State.Global
import           Pos.Ssc.State.Local

import           Pos.Util.Trace.Named (TraceNamed)

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
