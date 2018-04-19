{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.State
       ( mkSscState
       , module Pos.Ssc.State.Global
       , module Pos.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           System.Wlog (WithLogger)

import           Pos.DB (MonadDBRead)
import           Pos.Lrc.Context (LrcContext)
import           Pos.Slotting.Class (MonadSlots)
import           Pos.Ssc.Types (SscState (..))
import           Pos.Util.Util (HasLens (..))
import           Pos.Core (HasProtocolConstants)

-- Reexports
import           Pos.Ssc.State.Global
import           Pos.Ssc.State.Local

mkSscState
    :: forall ctx m .
       ( WithLogger m
       , MonadReader ctx m
       , HasLens LrcContext ctx LrcContext
       , MonadDBRead m
       , MonadIO m
       , MonadSlots ctx m
       , HasProtocolConstants
       )
    => m SscState
mkSscState = do
    gState <- sscLoadGlobalState
    ld <- sscNewLocalData
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
