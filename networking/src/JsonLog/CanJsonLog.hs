{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

{-|
Module      : JsonLog.CanJsonLog
Description : Class of monads that support JSON logging
License:      MIT
Maintainer:   lars.bruenjes@iohk.io
Stability:    experimental
Portability:  GHC

This module defines @'CanJsonLog'@,
a class for monads that support JSON logging.
-}

module JsonLog.CanJsonLog
    ( CanJsonLog (..)
    , jsonLogWrappedM
    ) where

import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Writer (WriterT)
import           Data.Aeson.Types (ToJSON)
import           Serokell.Util.Lens (WrappedM (..))
import           System.Wlog.LoggerNameBox (LoggerNameBox)

-- | An instance of class @'CanJsonLog'@ supports the effect of
-- JSON logging.
class Monad m => CanJsonLog m where

    -- | @'jsonLog' x@ serializes @x@ to JSON and
    -- writes the resulting JSON value to the JSON log.
    jsonLog :: ToJSON a => a -> m ()

    default jsonLog :: ( CanJsonLog n
                       , MonadTrans t
                       , m ~ t n
                       , ToJSON a)
                    => a
                    -> m ()
    jsonLog x = lift $ jsonLog x

instance CanJsonLog m => CanJsonLog (IdentityT m)
instance CanJsonLog m => CanJsonLog (ReaderT r m)
instance CanJsonLog m => CanJsonLog (StateT s m)
instance (Monoid w, CanJsonLog m) => CanJsonLog (WriterT w m)
instance CanJsonLog m => CanJsonLog (LoggerNameBox m)
instance CanJsonLog m => CanJsonLog (ResourceT m)

-- | @'jsonLogWrappedM'@ is a convenience default implementation
-- of @'jsonLog'@ to facilitate providing instances of class @'CanJsonLog'@
-- for instances of class @'WrappedM'@.
jsonLogWrappedM :: (WrappedM m, CanJsonLog (UnwrappedM m), ToJSON a) => a -> m ()
jsonLogWrappedM = unpackM . jsonLog
