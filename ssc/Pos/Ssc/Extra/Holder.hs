{-# LANGUAGE TypeFamilies #-}

-- | Monad transformer which stores SSC data.

module Pos.Ssc.Extra.Holder
       ( SscMemTag
       , SscState
       , mkSscState
       ) where

import           Universum

import qualified Control.Concurrent.STM  as STM
import           Ether.Internal          (HasLens (..))
import           System.Wlog             (WithLogger)

import           Pos.DB                  (MonadDBRead)
import           Pos.Lrc.Context         (LrcContext)
import           Pos.Slotting.Class      (MonadSlots)
import           Pos.Ssc.GState          (sscLoadGlobalState)
import           Pos.Ssc.Mem             (SscMemTag)
import           Pos.Ssc.Types           (SscState (..))
import           Pos.Ssc.LocalData       (sscNewLocalData)

mkSscState
    :: forall ctx m .
       ( WithLogger m
       , MonadReader ctx m
       , HasLens LrcContext ctx LrcContext
       , MonadDBRead m
       , MonadIO m
       , MonadSlots ctx m
       )
    => m SscState
mkSscState = do
    gState <- sscLoadGlobalState
    ld <- sscNewLocalData
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
