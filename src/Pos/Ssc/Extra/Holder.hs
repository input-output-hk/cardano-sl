{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Monad transformer which stores SSC data.

module Pos.Ssc.Extra.Holder
       ( SscHolder
       , mkSscHolderState
       , mkStateAndRunSscHolder
       , runSscHolder
       , ignoreSscHolder
       ) where

import qualified Control.Concurrent.STM       as STM
import qualified Control.Monad.Ether.Implicit as Ether
import           System.Wlog                  (WithLogger)
import           Universum

import           Pos.DB                       (MonadDB)
import           Pos.Lrc.Context              (LrcContext)
import           Pos.Slotting.Class           (MonadSlots)
import           Pos.Ssc.Class.LocalData      (SscLocalDataClass (sscNewLocalData))
import           Pos.Ssc.Class.Storage        (SscGStateClass (sscLoadGlobalState))
import           Pos.Ssc.Extra.Class          (MonadSscMem (..))
import           Pos.Ssc.Extra.Types          (SscState (..))
import           Pos.Util.Context             (HasContext)

type SscHolder ssc = Ether.ReaderT (SscState ssc)

instance Monad m => MonadSscMem ssc (SscHolder ssc m) where
    askSscMem = Ether.ask

-- | Run 'SscHolder' reading GState from DB (restoring from blocks)
-- and using default (uninitialized) local state.
runSscHolder
    :: forall ssc m a.
       ( --WithLogger m
       --, WithNodeContext ssc m
       --, SscGStateClass ssc
       --, SscLocalDataClass ssc
       --, MonadDB m
       --, MonadSlots m
       )
    => SscState ssc
    -> SscHolder ssc m a
    -> m a
runSscHolder st holder = Ether.runReaderT holder st

mkStateAndRunSscHolder
    :: forall ssc m a.
       ( WithLogger m
       , HasContext LrcContext m
       , SscGStateClass ssc
       , SscLocalDataClass ssc
       , MonadDB m
       , MonadSlots m
       )
    => SscHolder ssc m a
    -> m a
mkStateAndRunSscHolder holder = do
    st <- mkSscHolderState
    runSscHolder st holder

mkSscHolderState
    :: forall ssc m .
       ( WithLogger m
       , HasContext LrcContext m
       , SscGStateClass ssc
       , SscLocalDataClass ssc
       , MonadDB m
       , MonadSlots m
       )
    => m (SscState ssc)
mkSscHolderState = do
    gState <- sscLoadGlobalState @ssc
    ld <- sscNewLocalData @ssc
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld

ignoreSscHolder :: SscHolder ssc m a -> m a
ignoreSscHolder holder =
    Ether.runReaderT holder (error "SSC var: don't force me")
