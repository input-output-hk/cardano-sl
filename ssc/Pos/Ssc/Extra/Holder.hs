{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Monad transformer which stores SSC data.

module Pos.Ssc.Extra.Holder
       ( SscMemTag
       , SscState
       , mkSscState
       , bottomSscState
       ) where

import           Universum

import qualified Control.Concurrent.STM  as STM
import qualified Ether
import           System.Wlog             (WithLogger)

import           Pos.DB                  (MonadDBPure)
import           Pos.Lrc.Context         (LrcContext)
import           Pos.Slotting.Class      (MonadSlots)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (sscNewLocalData))
import           Pos.Ssc.Class.Storage   (SscGStateClass (sscLoadGlobalState))
import           Pos.Ssc.Extra.Class     (SscMemTag)
import           Pos.Ssc.Extra.Types     (SscState (..))

mkSscState
    :: forall ssc m .
       ( WithLogger m
       , Ether.MonadReader' LrcContext m
       , SscGStateClass ssc
       , SscLocalDataClass ssc
       , MonadDBPure m
       , MonadIO m
       , MonadSlots m
       )
    => m (SscState ssc)
mkSscState = do
    gState <- sscLoadGlobalState @ssc
    ld <- sscNewLocalData @ssc
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld

bottomSscState :: SscState ssc
bottomSscState = error "SSC var: don't force me"
