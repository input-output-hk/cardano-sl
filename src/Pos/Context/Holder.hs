{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of WithNodeContext.

module Pos.Context.Holder
       ( ContextHolder
       , runContextHolder
       ) where

import           Control.Concurrent.MVar (withMVar)
import qualified Control.Monad.Ether     as Ether.E
import           Formatting              (sformat, shown, (%))
import           Mockable                (Catch, Mockable, catchAll)
import           System.Wlog             (WithLogger, logWarning)
import           Universum               hiding (catchAll)

import           Pos.Context.Context     (NodeContext (..))
import           Pos.Util.Context        (ContextHolder', ContextTagK (..))
import           Pos.Util.JsonLog        (MonadJL (..), appendJL)
import           Pos.Util.Util           (ether)

-- | Wrapper for monadic action which brings 'NodeContext'.
type ContextHolder ssc = ContextHolder' (ContextHolderTrans ssc)

type ContextHolderTrans ssc = ReaderT (NodeContext ssc)

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder = flip (Ether.E.runReaderT (Proxy @'ContextTag))

instance
    (MonadIO m, Mockable Catch m, WithLogger m, ContextHolderTrans ssc ~ t) =>
        MonadJL (ContextHolder' t m)
  where
    jlLog ev =
        whenJustM (ether (asks ncJLFile)) $ \logFileMV ->
            (liftIO . withMVar logFileMV $ flip appendJL ev)
            `catchAll` \e ->
                logWarning $ sformat ("Can't write to json log: "%shown) e
