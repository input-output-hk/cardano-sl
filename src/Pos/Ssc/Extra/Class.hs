{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class for in-memory state of SSC. Doesn't depend on concrete SSC.

module Pos.Ssc.Extra.Class
       ( MonadSscMem (..)
       ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans  (MonadTrans)
import           Universum

import           Pos.Ssc.Extra.Types  (SscState)

class Monad m =>
      MonadSscMem ssc m | m -> ssc where
    askSscMem :: m (SscState ssc)
    default askSscMem :: (MonadTrans t, MonadSscMem ssc m', t m' ~ m) =>
        m (SscState ssc)
    askSscMem = lift askSscMem

instance (MonadSscMem ssc m) => MonadSscMem ssc (ReaderT x m)
instance (MonadSscMem ssc m) => MonadSscMem ssc (StateT x m)
instance (MonadSscMem ssc m) => MonadSscMem ssc (ExceptT x m)
