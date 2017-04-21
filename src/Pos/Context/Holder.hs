{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of WithNodeContext.

module Pos.Context.Holder
       ( ContextHolder
       , runContextHolder
       ) where

import           Control.Concurrent.MVar          (withMVar)
import qualified Control.Monad.Ether              as Ether.E
import qualified Control.Monad.Trans.Ether.Tagged as Ether
import           Formatting                       (sformat, shown, (%))
import           Mockable                         (Catch, Mockable, catchAll)
import           System.Wlog                      (WithLogger, logWarning)
import           Universum                        hiding (catchAll)

import           Pos.Communication.Relay          (MonadRelayMem (..), RelayContext (..))
import           Pos.Context.Class                (ContextTagK (..))
import           Pos.Context.Context              (NodeContext (..))
import           Pos.DHT.MemState                 (DhtContext (..), MonadDhtMem (..))
import           Pos.Launcher.Param               (bpKademliaDump, npBaseParams,
                                                   npPropagation, npReportServers)
import           Pos.Reporting                    (MonadReportingMem (..),
                                                   ReportingContext (..))
import           Pos.Shutdown                     (MonadShutdownMem (..),
                                                   ShutdownContext (..))
import           Pos.Util.JsonLog                 (MonadJL (..), appendJL)
import           Pos.Util.Util                    (ether)

-- | Wrapper for monadic action which brings 'NodeContext'.
type ContextHolder ssc = Ether.E.ReaderT 'ContextTag (NodeContext ssc)

type ContextHolder' = Ether.TaggedTrans 'ContextTag

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder = flip (Ether.E.runReaderT (Proxy @'ContextTag))

instance (MonadIO m, Mockable Catch m, WithLogger m, t ~ ReaderT (NodeContext ssc)) =>
  MonadJL (ContextHolder' t m) where
    jlLog ev = ether (asks ncJLFile) >>= maybe (pure ()) doLog
      where
        doLog logFileMV =
          (liftIO . withMVar logFileMV $ flip appendJL ev)
            `catchAll` \e -> logWarning $ sformat ("Can't write to json log: " % shown) e

instance (Monad m, t ~ ReaderT (NodeContext ssc)) => MonadReportingMem (ContextHolder' t m) where
    askReportingContext =
        ether $
            asks (\NodeContext{..} ->
                    ReportingContext (npReportServers ncNodeParams)
                                     ncLoggerConfig)

instance (Monad m, t ~ ReaderT (NodeContext ssc)) => MonadDhtMem (ContextHolder' t m) where
    askDhtMem = ether $
      asks (DhtContext . bpKademliaDump . npBaseParams . ncNodeParams)

instance (Monad m, t ~ ReaderT (NodeContext ssc)) => MonadRelayMem (ContextHolder' t m) where
    askRelayMem =
        ether
            (RelayContext
                <$> asks (npPropagation . ncNodeParams)
                <*> asks ncInvPropagationQueue
            )

instance (Monad m, t ~ ReaderT (NodeContext ssc)) => MonadShutdownMem (ContextHolder' t m) where
    askShutdownMem =
        ether
            (ShutdownContext
                <$> asks ncShutdownFlag
                <*> asks ncShutdownNotifyQueue
            )
