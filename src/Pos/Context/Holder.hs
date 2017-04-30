{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of WithNodeContext.

module Pos.Context.Holder
       ( ContextHolder
       , runContextHolder
       , JLContext
       , runJLContext
       ) where

import           Control.Concurrent.MVar (withMVar)
import           Data.Coerce
import qualified Ether
import           Formatting              (sformat, shown, (%))
import           Mockable                (Catch, Mockable, catchAll)
import           System.Wlog             (WithLogger, logWarning)
import           Universum               hiding (catchAll)

import           Pos.Context.Class       (WithNodeContext)
import           Pos.Context.Context     (NodeContext (..), NodeContextTag)
import           Pos.Util.JsonLog        (MonadJL (..), appendJL)

data JLContextTag

type JLContext = Ether.TaggedTrans JLContextTag Ether.IdentityT

runJLContext :: JLContext m a -> m a
runJLContext = coerce

instance
    ( MonadIO m, Mockable Catch m, WithLogger m, t ~ Ether.IdentityT
    , WithNodeContext ssc m ) =>
        MonadJL (Ether.TaggedTrans JLContextTag t m)
  where
    jlLog ev =
        whenJustM (Ether.asks @NodeContextTag ncJLFile) $ \logFileMV ->
            (liftIO . withMVar logFileMV $ flip appendJL ev)
            `catchAll` \e ->
                logWarning $ sformat ("Can't write to json log: "%shown) e

-- | Wrapper for monadic action which brings 'NodeContext'.
type ContextHolder ssc = Ether.ReadersT (NodeContext ssc)

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder = flip Ether.runReadersT

