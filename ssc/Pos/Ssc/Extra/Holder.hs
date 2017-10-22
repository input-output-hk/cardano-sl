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
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (sscNewLocalData))
import           Pos.Ssc.Class.Storage   (SscGStateClass (sscLoadGlobalState))
import           Pos.Ssc.Extra.Class     (SscMemTag)
import           Pos.Ssc.Types           (SscState (..))

mkSscState
    :: forall ctx m .
       ( WithLogger m
       , MonadReader ctx m
       , HasLens LrcContext ctx LrcContext
       , SscGStateClass
       , SscLocalDataClass
       , MonadDBRead m
       , MonadIO m
       , MonadSlots ctx m
       )
    => m SscState
mkSscState = do
    gState <- sscLoadGlobalState
    ld <- sscNewLocalData
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
