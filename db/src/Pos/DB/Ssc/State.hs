{-# LANGUAGE TypeFamilies #-}

module Pos.DB.Ssc.State
       ( mkSscState
       , module Pos.DB.Ssc.State.Global
       , module Pos.DB.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           System.Wlog (WithLogger)

import           Pos.Chain.Ssc (SscState (..))
import           Pos.Core.Slotting (MonadSlots)
import           Pos.DB (MonadDBRead)

-- Reexports
import           Pos.DB.Ssc.State.Global
import           Pos.DB.Ssc.State.Local

mkSscState
    :: forall ctx m .
       ( WithLogger m
       , MonadDBRead m
       , MonadSlots ctx m
       )
    => m SscState
mkSscState = do
    gState <- sscLoadGlobalState
    ld <- sscNewLocalData
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
