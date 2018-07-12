{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Kernel.Restore (runMonadDBRead) where

import Universum
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans.Resource (transResourceT)
import Data.Conduit (transPipe)
import Pos.Core (CoreConfiguration, withCoreConfiguration,
                 GenesisData, withGenesisData,
                 GenesisHash, withGenesisHash, getGenesisHash,
                 GeneratedSecrets, withGeneratedSecrets,
                 BlockVersionData, withGenesisBlockVersionData,
                 ProtocolConstants, withProtocolConstants)
import Pos.Core.Configuration (HasConfiguration)
import Pos.DB.Class (Serialized(Serialized), MonadDBRead(..))

import Pos.DB.Block (getSerializedUndo, getSerializedBlock)
import Pos.DB.Rocks.Functions (dbGetDefault, dbIterSourceDefault)
import Pos.DB.Rocks.Types (MonadRealDB, NodeDBs)

newtype MyDBReadT m a = MyDBReadT { unMyDBReadT :: ReaderT NodeDBs m a }
  deriving (Functor, Applicative, Monad, MonadThrow)

instance (HasConfiguration, MonadThrow (MyDBReadT m), MonadRealDB NodeDBs (ReaderT NodeDBs m))
    => MonadDBRead (MyDBReadT m) where
    dbGet tag bs = MyDBReadT (dbGetDefault tag bs)
    dbIterSource tag p = transPipe (transResourceT MyDBReadT) (dbIterSourceDefault tag p)
    dbGetSerBlock hh = MyDBReadT (fmap Serialized <$> getSerializedBlock hh)
    dbGetSerUndo hh = MyDBReadT (fmap Serialized <$> getSerializedUndo hh)

runMonadDBRead
  :: (MonadThrow m, MonadCatch m, MonadIO m)
  => CoreConfiguration
  -> Maybe GeneratedSecrets
  -> GenesisData
  -> GenesisHash
  -> BlockVersionData -- ^ From genesis block
  -> ProtocolConstants
  -> NodeDBs
  -> (forall n. MonadDBRead n => n a)
  -> m a
runMonadDBRead cc ygs gd gh bvd pc ndbs act =
  withCoreConfiguration cc $
  withGeneratedSecrets ygs $
  withGenesisData gd $
  withGenesisHash (getGenesisHash gh) $
  withGenesisBlockVersionData bvd $
  withProtocolConstants pc $
  runReaderT (unMyDBReadT act) ndbs

