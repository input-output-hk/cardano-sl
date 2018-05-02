{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Sqlite database for the 'TxMeta' portion of the wallet kernel.
module Cardano.Wallet.Kernel.DB.Sqlite (openMetaDB, MetaDB, putTxMeta) where

import           Universum

import           Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import qualified Database.Beam.Query as SQL
import           Database.Beam.Schema (Beamable, Database, DatabaseSettings, PrimaryKey, Table)
import qualified Database.Beam.Schema as Beam
import           Database.Beam.Sqlite.Connection (Sqlite, runBeamSqliteDebug)
import           Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import qualified Database.SQLite.Simple as Sqlite

import           Data.Scientific (scientific)
import           Data.Time.Units (toMicroseconds)
import           Formatting (sformat)
import           GHC.Generics (Generic)

import qualified Cardano.Wallet.Kernel.DB.TxMeta as Kernel
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (hashHexF)


data MetaDBHandle = MetaDBHandle {
    _mDbHandleConnection :: Sqlite.Connection
}


data MetaDB f = MetaDB { _mDbMeta    :: f (Beam.TableEntity TxMetaT)
                       , _mDbInputs  :: f (Beam.TableEntity TxInputsT)
                       , _mDbOutputs :: f (Beam.TableEntity TxOutputsT)
                       } deriving Generic

instance Database Sqlite MetaDB

{--

* Table 1: ~tx_metas~
** Primary Index: ~tx_meta_id~
** Secondary Indexes (for now): ~tx_meta_created_at~

| tx_meta_id | tx_meta_amount | tx_meta_created_at | tx_meta_is_local | tx_meta_is_outgoing |
|------------+----------------+--------------------+------------------+---------------------|
| Core.TxId  | Core.Coin      | Core.Timestamp     | Bool             | Bool                |

--}
data TxMetaT f = TxMetaT {
      _txMetaTableId         :: Beam.Columnar f Core.TxId
    , _txMetaTableAmount     :: Beam.Columnar f Core.Coin
    , _txMetaTableCreatedAt  :: Beam.Columnar f Core.Timestamp
    , _txMetaTableIsLocal    :: Beam.Columnar f Bool
    , _txMetaTableIsOutgoing :: Beam.Columnar f Bool
    } deriving Generic

instance Beamable TxMetaT

instance Table TxMetaT where
    data PrimaryKey TxMetaT f = TxIdPrimKey (Beam.Columnar f Core.TxId) deriving Generic
    primaryKey = TxIdPrimKey . _txMetaTableId

instance Beamable (PrimaryKey TxMetaT)

{--

* Table 2: ~tx_meta_inputs~
** Primary Index: ~tx_meta_input_address~
** Secondary Indexes: ~tx_meta_id~

| tx_meta_input_address | tx_meta_coin | tx_meta_id |
|-----------------------+--------------+------------|
| Core.Address          | Core.Coin    | Core.TxId  |

** Table 3: ~tx_meta_outputs~
** Primary Index: ~tx_meta_output_address~
** Secondary Indexes: ~tx_meta_id~

| tx_meta_output_address | tx_meta_coin | tx_meta_id |
|------------------------+--------------+------------|
| Core.Address           | Core.Coin    | Core.TxId  |

--}

data TxCoinDistributionTableT f = TxCoinDistributionTableT {
      _txCoinDistributionTableAddress :: Beam.Columnar f Core.Address
    , _txCoinDistributionTableCoin    :: Beam.Columnar f Core.Coin
    , _txCoinDistributionTxId         :: Beam.PrimaryKey TxMetaT f
    } deriving Generic

instance Beamable TxCoinDistributionTableT

-- | The inputs' table.
newtype TxInputsT  f = TxCoinInputsT  { _getTxInputs  :: (TxCoinDistributionTableT f) } deriving Generic

instance Beamable TxInputsT

instance Table TxInputsT where
    data PrimaryKey TxInputsT f = TxInputsPrimKey (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey = TxInputsPrimKey . _txCoinDistributionTxId . _getTxInputs

instance Beamable (PrimaryKey TxInputsT)

-- | The outputs' table.
newtype TxOutputsT f = TxCoinOutputsT { _getTxOutputs :: (TxCoinDistributionTableT f) } deriving Generic

instance Beamable TxOutputsT

instance Table TxOutputsT where
    data PrimaryKey TxOutputsT f = TxOutputsPrimKey (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey = TxOutputsPrimKey . _txCoinDistributionTxId . _getTxOutputs

instance Beamable (PrimaryKey TxOutputsT)


-- Orphans & other boilerplate

instance HasSqlValueSyntax SqliteValueSyntax Core.TxId where
    sqlValueSyntax txId = sqlValueSyntax (sformat hashHexF txId)

instance HasSqlValueSyntax SqliteValueSyntax Core.Coin where
    sqlValueSyntax = sqlValueSyntax . Core.unsafeGetCoin

instance HasSqlValueSyntax SqliteValueSyntax Core.Timestamp where
    sqlValueSyntax ts = sqlValueSyntax (scientific (toMicroseconds . Core.getTimestamp $ ts) 0)


-- | Creates new 'DatabaseSettings' for the 'MetaDB', locking the backend to
-- be 'Sqlite'.
metaDB :: DatabaseSettings Sqlite MetaDB
metaDB = Beam.defaultDbSettings

-- | Opens a new 'Connection' to the @Sqlite@ database identified by the
-- input 'FilePath'.
openMetaDB :: FilePath -> IO MetaDBHandle
openMetaDB fp = MetaDBHandle <$> Sqlite.open fp


-- FIXME(adinapoli): Toggle debug/production with the 'WalletMode'.
putTxMeta :: MetaDBHandle -> Kernel.TxMeta -> IO ()
putTxMeta dbHandle _txMeta =
    let conn = _mDbHandleConnection dbHandle
        tMeta   = mempty @[TxMetaT Identity]
        --inputs  = mempty @[TxInputsT Identity]
        --outputs = mempty @[TxOutputsT Identity]
    in runBeamSqliteDebug putStrLn conn $ SQL.runInsert $ do
        SQL.insert (_mDbMeta metaDB)    $ SQL.insertValues tMeta
        -- SQL.insert (_mDbInputs metaDB)  $ SQL.insertValues inputs
        -- SQL.insert (_mDbOutputs metaDB) $ SQL.insertValues outputs
