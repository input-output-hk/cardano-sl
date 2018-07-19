-- | This module exports various tools for compatibility with old wallet
-- dependencies.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Wallet.Kernel.Compat
  ( runDBReadT
  , getCoreConfigurations
  ) where

import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)
import           Control.Monad.Trans.Resource (transResourceT)
import           Data.Conduit (transPipe)
import           Pos.Core (BlockVersionData, CoreConfiguration,
                     GeneratedSecrets, GenesisData, GenesisHash (GenesisHash),
                     ProtocolConstants, ProtocolMagic, Timestamp,
                     generatedSecrets, genesisBlockVersionData, genesisData,
                     genesisHash, getGenesisHash, protocolConstants,
                     withCoreConfiguration, withCoreConfigurations,
                     withGeneratedSecrets, withGenesisBlockVersionData,
                     withGenesisData, withGenesisHash, withProtocolConstants)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB.Class (MonadDBRead (..), Serialized (Serialized))
import           Universum

import           Pos.DB.Block (getSerializedBlock, getSerializedUndo)
import           Pos.DB.Rocks.Functions (dbGetDefault, dbIterSourceDefault)
import           Pos.DB.Rocks.Types (MonadRealDB, NodeDBs)

--------------------------------------------------------------------------------

-- | This monad transformer exists solely to provide a 'MonadRealDB' instance,
-- as required by upstream libraries.
newtype DBReadT m a = DBReadT { unDBReadT :: ReaderT NodeDBs m a }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadTrans)

instance (HasConfiguration, MonadThrow (DBReadT m), MonadRealDB NodeDBs (ReaderT NodeDBs m))
    => MonadDBRead (DBReadT m) where
    dbGet tag bs = DBReadT (dbGetDefault tag bs)
    dbIterSource tag p = transPipe (transResourceT DBReadT) (dbIterSourceDefault tag p)
    dbGetSerBlock hh = DBReadT (fmap Serialized <$> getSerializedBlock hh)
    dbGetSerUndo hh = DBReadT (fmap Serialized <$> getSerializedUndo hh)

-- | Runs a 'DBReadT'.
--
-- This is also a monad morphism from @'DBReadT' m@ to @m@.
runDBReadT
  :: (MonadCatch m, MonadIO m)
  => CoreConfiguration
  -> Maybe GeneratedSecrets
  -> GenesisData
  -> GenesisHash
  -> BlockVersionData -- ^ From genesis block
  -> ProtocolConstants
  -> NodeDBs
  -> (MonadDBRead (DBReadT m) => DBReadT m a)
  -> m a
runDBReadT cc ygs gd gh bvd pc ndbs act =
  withCoreConfiguration cc $
  withGeneratedSecrets ygs $
  withGenesisData gd $
  withGenesisHash (getGenesisHash gh) $
  withGenesisBlockVersionData bvd $
  withProtocolConstants pc $
  runReaderT (unDBReadT act) ndbs

-- | Like 'Pos.Core.Configuration.Core.withCoreConfigurations', but doesn't
-- rely on 'Given'. Rather, it returns all of the values expected by
-- 'HasConfiguration' as first class values.
getCoreConfigurations
  :: CoreConfiguration
  -> FilePath
  -- ^ Directory where 'configuration.yaml' is stored.
  -> Maybe Timestamp
  -- ^ Optional system start time.
  --   It must be given when the genesis spec uses a testnet initializer.
  -> Maybe Integer
  -- ^ Optional seed which overrides one from testnet initializer if
  -- provided.
  -> IO ( ProtocolMagic
        , Maybe GeneratedSecrets
        , GenesisData
        , GenesisHash
        , BlockVersionData
        , ProtocolConstants )
getCoreConfigurations cc fp yts yseed =
    withCoreConfigurations cc fp yts yseed $ \pm -> do
        pure ( pm
             , generatedSecrets
             , genesisData
             , GenesisHash genesisHash
             , genesisBlockVersionData
             , protocolConstants )

