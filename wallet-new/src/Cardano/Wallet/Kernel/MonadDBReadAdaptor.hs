{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Wallet.Kernel.MonadDBReadAdaptor (
    WithMonadDBRead    -- opaque
  , MonadDBReadAdaptor -- opaque
  , newMonadDBReadAdaptor
  , withMonadDBRead
  , rocksDBNotAvailable
  ) where

import           Universum

import           Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (UnliftIO),
                     askUnliftIO, unliftIO, withUnliftIO)
import           Control.Monad.Trans.Resource (transResourceT)
import           Data.Conduit (transPipe)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB.Block (getSerializedBlock, getSerializedUndo)
import           Pos.DB.Class (MonadDBRead (..), Serialized (Serialized))
import           Pos.DB.Rocks.Functions (dbGetDefault, dbIterSourceDefault)
import           Pos.DB.Rocks.Types (NodeDBs)

-- | @WithMonadDBRead m@ is a monad in which we have a 'MonadDBRead' instance
-- available (as well as a whole bunch of other instances).
--
-- See 'MonadDBReadAdapter' for running 'WithMonadDBRead' actions.
newtype WithMonadDBRead m a = Wrap { unwrap :: ReaderT NodeDBs m a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadThrow
    , MonadTrans
    , MonadIO
    )

instance MonadUnliftIO m => MonadUnliftIO (WithMonadDBRead m) where
  askUnliftIO = Wrap $ withUnliftIO $ \u ->
                  pure $ UnliftIO (unliftIO u . unwrap)

instance ( HasConfiguration -- silly superclass constraint in core, cannot avoid
         , MonadThrow m
         , MonadIO    m
         , MonadCatch m
         ) => MonadDBRead (WithMonadDBRead m) where
  dbGet tag bs       = Wrap $ dbGetDefault tag bs
  dbGetSerBlock hh   = Wrap $ fmap Serialized <$> getSerializedBlock hh
  dbGetSerUndo hh    = Wrap $ fmap Serialized <$> getSerializedUndo  hh
  dbIterSource tag p = transPipe (transResourceT Wrap) $
                         dbIterSourceDefault tag p

-- | Adaptor for running 'WithMonadDBRead' actions. See 'newMonadBDReadAdaptor'
newtype MonadDBReadAdaptor m = Adaptor {
      withMonadDBRead :: forall a.
                         (HasConfiguration => WithMonadDBRead m a)
                      -> m a
    }

-- | Constructor for 'MonadDBReadAdaptor'
--
-- NOTE: This captures the 'HasConfiguration' constraint in the closure so
-- that the adaptor can be used in a place where this constraint is not
-- available.
newMonadDBReadAdaptor :: HasConfiguration => NodeDBs -> MonadDBReadAdaptor m
newMonadDBReadAdaptor ndbs = Adaptor $ \act -> runReaderT (unwrap act) ndbs

-- | Drop-in replacement for the 'MonadDBReadAdaptor' for when rocks DB is
-- not available (throws an exception when used).
--
-- Used only for tests.
rocksDBNotAvailable :: MonadThrow m => MonadDBReadAdaptor m
rocksDBNotAvailable = Adaptor $ \_act -> throwM RocksDBNotAvailable

-- | Thrown when using the 'rocksDBNotAvailable' adaptor.
data RocksDBNotAvailable = RocksDBNotAvailable
  deriving (Show)

instance Exception RocksDBNotAvailable
