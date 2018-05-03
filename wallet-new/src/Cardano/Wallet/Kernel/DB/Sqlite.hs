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
                       , _mDbInputs  :: f (Beam.TableEntity TxInputT)
                       , _mDbOutputs :: f (Beam.TableEntity TxOutputT)
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
data TxMetaT f = TxMeta {
      _txMetaTableId         :: Beam.Columnar f Core.TxId
    , _txMetaTableAmount     :: Beam.Columnar f Core.Coin
    , _txMetaTableCreatedAt  :: Beam.Columnar f Core.Timestamp
    , _txMetaTableIsLocal    :: Beam.Columnar f Bool
    , _txMetaTableIsOutgoing :: Beam.Columnar f Bool
    } deriving Generic

type TxMeta = TxMetaT Identity

-- | Creates a storage-specific 'TxMeta' out of a 'Kernel.TxMeta'.
mkTxMeta :: Kernel.TxMeta -> TxMeta
mkTxMeta txMeta = TxMeta {
                  _txMetaTableId         = txMeta ^. Kernel.txMetaId
                , _txMetaTableAmount     = txMeta ^. Kernel.txMetaAmount
                , _txMetaTableCreatedAt  = txMeta ^. Kernel.txMetaCreationAt
                , _txMetaTableIsLocal    = txMeta ^. Kernel.txMetaIsLocal
                , _txMetaTableIsOutgoing = txMeta ^. Kernel.txMetaIsOutgoing
                }

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

data TxCoinDistributionTableT f = TxCoinDistributionTable {
      _txCoinDistributionTableAddress :: Beam.Columnar f Core.Address
    , _txCoinDistributionTableCoin    :: Beam.Columnar f Core.Coin
    , _txCoinDistributionTxId         :: Beam.PrimaryKey TxMetaT f
    } deriving Generic

instance Beamable TxCoinDistributionTableT

-- | The inputs' table.
newtype TxInputT f = TxInput  { _getTxInput  :: (TxCoinDistributionTableT f) } deriving Generic

type TxInput = TxInputT Identity

-- | Convenient constructor of a list of 'TxInput' from a 'Kernel.TxMeta'.
mkInputs :: Kernel.TxMeta -> [TxInput]
mkInputs txMeta =
    let inputs = txMeta ^. Kernel.txMetaInputs
        txId   = txMeta ^. Kernel.txMetaId
    in map (buildInput txId) (toList inputs)
  where
      buildInput :: Core.TxId -> (Core.Address, Core.Coin) -> TxInput
      buildInput tid (addr, coin) = TxInput (TxCoinDistributionTable addr coin (TxIdPrimKey tid))

instance Beamable TxInputT

instance Table TxInputT where
    data PrimaryKey TxInputT f = TxInputPrimKey (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey = TxInputPrimKey . _txCoinDistributionTxId . _getTxInput

instance Beamable (PrimaryKey TxInputT)

-- | The outputs' table.
newtype TxOutputT f = TxOutput { _getTxOutput :: (TxCoinDistributionTableT f) } deriving Generic

type TxOutput = TxOutputT Identity

-- | Convenient constructor of a list of 'TxOutput from a 'Kernel.TxMeta'.
-- FIXME(adn) Generalise the two smart constructors.
mkOutputs :: Kernel.TxMeta -> [TxOutput]
mkOutputs txMeta =
    let outputs = txMeta ^. Kernel.txMetaOutputs
        txId    = txMeta ^. Kernel.txMetaId
    in map (buildInput txId) (toList outputs)
  where
      buildInput :: Core.TxId -> (Core.Address, Core.Coin) -> TxOutput
      buildInput tid (addr, coin) = TxOutput (TxCoinDistributionTable addr coin (TxIdPrimKey tid))

instance Beamable TxOutputT

instance Table TxOutputT where
    data PrimaryKey TxOutputT f = TxOutputPrimKey (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey = TxOutputPrimKey . _txCoinDistributionTxId . _getTxOutput

instance Beamable (PrimaryKey TxOutputT)


-- Orphans & other boilerplate

instance HasSqlValueSyntax SqliteValueSyntax Core.TxId where
    sqlValueSyntax txId = sqlValueSyntax (sformat hashHexF txId)

instance HasSqlValueSyntax SqliteValueSyntax Core.Coin where
    sqlValueSyntax = sqlValueSyntax . Core.unsafeGetCoin

instance HasSqlValueSyntax SqliteValueSyntax Core.Timestamp where
    sqlValueSyntax ts = sqlValueSyntax (scientific (toMicroseconds . Core.getTimestamp $ ts) 0)

instance HasSqlValueSyntax SqliteValueSyntax Core.Address where
    sqlValueSyntax addr = sqlValueSyntax (sformat Core.addressF addr)


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
putTxMeta dbHandle txMeta =
    let conn = _mDbHandleConnection dbHandle
        tMeta   = mkTxMeta txMeta
        inputs  = mkInputs txMeta
        outputs = mkOutputs txMeta
    -- TODO(adn): Revisit this bit, it doesn't look transactional.
    in runBeamSqliteDebug putStrLn conn $ do
        SQL.runInsert $ SQL.insert (_mDbMeta metaDB)    $ SQL.insertValues [tMeta]
        SQL.runInsert $ SQL.insert (_mDbInputs metaDB)  $ SQL.insertValues inputs
        SQL.runInsert $ SQL.insert (_mDbOutputs metaDB) $ SQL.insertValues outputs
