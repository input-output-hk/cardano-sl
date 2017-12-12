{-# LANGUAGE ExistentialQuantification #-}

-- | Batch operations.

module Pos.DB.BatchOp
       ( RocksBatchOp (..)
       , SomeBatchOp (..)
       , SomePrettyBatchOp (..)
       , dbWriteBatch'
       , rocksWriteBatch
       ) where

import           Universum

import qualified Data.Text.Buildable
import qualified Database.RocksDB as Rocks
import           Formatting (bprint)
import           Serokell.Util.Text (listJson)

import           Pos.DB.Class (DBTag, MonadDB (dbWriteBatch))
import           Pos.DB.Rocks.Types (DB (..))

class RocksBatchOp a where
    toBatchOp :: a -> [Rocks.BatchOp]

instance RocksBatchOp Rocks.BatchOp where
    toBatchOp = one

data EmptyBatchOp

instance RocksBatchOp EmptyBatchOp where
    toBatchOp _ = []

instance Buildable EmptyBatchOp where
    build _ = ""

data SomeBatchOp =
    forall a. RocksBatchOp a =>
              SomeBatchOp a

instance Semigroup SomeBatchOp where
    a <> b = SomeBatchOp [a, b]

instance Monoid SomeBatchOp where
    mempty = SomeBatchOp ([]::[EmptyBatchOp])
    mappend = (<>)

instance RocksBatchOp SomeBatchOp where
    toBatchOp (SomeBatchOp a) = toBatchOp a

data SomePrettyBatchOp =
    forall a. (RocksBatchOp a, Buildable a) =>
              SomePrettyBatchOp a

instance Semigroup SomePrettyBatchOp where
    a <> b = SomePrettyBatchOp [a, b]

instance Monoid SomePrettyBatchOp where
    mempty = SomePrettyBatchOp ([]::[SomePrettyBatchOp])
    mappend = (<>)

instance RocksBatchOp SomePrettyBatchOp where
    toBatchOp (SomePrettyBatchOp a) = toBatchOp a

instance Buildable SomePrettyBatchOp where
    build (SomePrettyBatchOp x) = Data.Text.Buildable.build x

-- instance (Foldable t, RocksBatchOp a) => RocksBatchOp (t a) where
--     toBatchOp = concatMap toBatchOp -- overlapping instances, wtf ?????

instance RocksBatchOp a => RocksBatchOp [a] where
    toBatchOp = concatMap toBatchOp

instance RocksBatchOp a => RocksBatchOp (NonEmpty a) where
    toBatchOp = concatMap toBatchOp

instance Buildable [SomePrettyBatchOp] where
    build = bprint listJson

-- | Write a batch of some operations using 'MonadDB' interface.
-- The only difference from 'dbWriteBatch' is that this function
-- works with whatever types which are instances of 'RocksBatchOp'.
dbWriteBatch' :: (RocksBatchOp a, MonadDB m) => DBTag -> [a] -> m ()
dbWriteBatch' tag batch = dbWriteBatch tag (concatMap toBatchOp batch)

-- | Write a batch of some operations to RocksDB.
rocksWriteBatch :: (RocksBatchOp a, MonadIO m) => [a] -> DB -> m ()
rocksWriteBatch batch DB {..} =
    Rocks.write rocksDB rocksWriteOpts (concatMap toBatchOp batch)
