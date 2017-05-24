-- | 'Control.Corcurrenc.ReadWriteVar' reimport, lifted.

module Pos.Util.Concurrent.RWVar
       ( RWVar
       , new
       ) where

import           Universum

import           Control.Concurrent.ReadWriteVar (RWVar)


new :: MonadIO m => a -> m (RWVar a)
new = liftIO . new

-- TBD
