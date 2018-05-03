{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Sqlite database for the 'TxMeta' portion of the wallet kernel.
module Cardano.Wallet.Kernel.DB.Sqlite (
      openMetaDB
    , MetaDBHandle
    , putTxMeta
    , getTxMeta
    ) where

import           Universum

import           Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax (..))
import           Database.Beam.Query (HasSqlEqualityCheck, (==.))
import qualified Database.Beam.Query as SQL
import           Database.Beam.Schema (Beamable, Database, DatabaseSettings, PrimaryKey, Table)
import qualified Database.Beam.Schema as Beam
import           Database.Beam.Sqlite.Connection (Sqlite, runBeamSqliteDebug)
import           Database.Beam.Sqlite.Syntax (SqliteExpressionSyntax, SqliteValueSyntax)
import qualified Database.SQLite.Simple as Sqlite
import           Database.SQLite.Simple.FromField (FromField (..), returnError)

import           Data.Time.Units (fromMicroseconds, toMicroseconds)
import           Formatting (sformat)
import           GHC.Generics (Generic)

import qualified Cardano.Wallet.Kernel.DB.TxMeta as Kernel
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (decodeAbstractHash, hashHexF)


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

type TxCoinDistributionTable = TxCoinDistributionTableT Identity

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
    data PrimaryKey TxInputT f = TxInputPrimKey (Beam.Columnar f Core.Address) (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey (TxInput i) = TxInputPrimKey (_txCoinDistributionTableAddress i) (_txCoinDistributionTxId i)

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
    data PrimaryKey TxOutputT f = TxOutputPrimKey (Beam.Columnar f Core.Address) (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey (TxOutput o) = TxOutputPrimKey (_txCoinDistributionTableAddress o) (_txCoinDistributionTxId o)

instance Beamable (PrimaryKey TxOutputT)


-- Orphans & other boilerplate

instance HasSqlValueSyntax SqliteValueSyntax Core.TxId where
    sqlValueSyntax txId = sqlValueSyntax (sformat hashHexF txId)

instance HasSqlValueSyntax SqliteValueSyntax Core.Coin where
    sqlValueSyntax = sqlValueSyntax . Core.unsafeGetCoin

-- NOTE(adn) Terribly inefficient, but removes the pain of dealing with
-- marshalling and unmarshalling of 'Integer'(s), as there is no 'HasSqlValueSyntax'
-- defined for them.
-- Using 'Scientific' would be a possibility, but it breaks down further as
-- there is no 'FromField' instance for them.
instance HasSqlValueSyntax SqliteValueSyntax Core.Timestamp where
    sqlValueSyntax ts = sqlValueSyntax (toText @String . show . toMicroseconds . Core.getTimestamp $ ts)

instance HasSqlValueSyntax SqliteValueSyntax Core.Address where
    sqlValueSyntax addr = sqlValueSyntax (sformat Core.addressF addr)


instance HasSqlEqualityCheck SqliteExpressionSyntax Core.TxId

instance FromField Core.TxId where
    fromField f = do
        h <- decodeAbstractHash <$> fromField f
        case h of
             Left _     -> returnError Sqlite.ConversionFailed f "not a valid hex hash"
             Right txid -> pure txid

instance FromBackendRow Sqlite Core.TxId

instance FromField Core.Coin where
    fromField f = Core.Coin <$> fromField f

instance FromBackendRow Sqlite Core.Coin

instance FromField Core.Timestamp where
    fromField f = do
        mbNumber <- readEither @Text @Integer <$> fromField f
        case mbNumber of
           Left _  -> returnError Sqlite.ConversionFailed f "not a valid Integer"
           Right n -> pure . Core.Timestamp . fromMicroseconds $ n

instance FromBackendRow Sqlite Core.Timestamp

instance FromField Core.Address where
    fromField f = do
        addr <- Core.decodeTextAddress <$> fromField f
        case addr of
           Left _  -> returnError Sqlite.ConversionFailed f "not a valid Address"
           Right a -> pure a

instance FromBackendRow Sqlite Core.Address


-- | Creates new 'DatabaseSettings' for the 'MetaDB', locking the backend to
-- be 'Sqlite'.
metaDB :: DatabaseSettings Sqlite MetaDB
metaDB = Beam.defaultDbSettings

-- | Opens a new 'Connection' to the @Sqlite@ database identified by the
-- input 'FilePath'.
openMetaDB :: FilePath -> IO MetaDBHandle
openMetaDB fp = MetaDBHandle <$> Sqlite.open fp

-- | Inserts a new 'Kernel.TxMeta' in the database, given its opaque
-- 'MetaDBHandle'.
-- FIXME(adinapoli): Toggle debug/production with the 'WalletMode'.
-- FIXME(adinapoli): Exception handling.
-- FIXME(adinapoli): Check invariant violated (attempt to insert empty inputs
-- or empty outputs).
putTxMeta :: MetaDBHandle -> Kernel.TxMeta -> IO ()
putTxMeta dbHandle txMeta =
    let conn = _mDbHandleConnection dbHandle
        tMeta   = mkTxMeta txMeta
        inputs  = mkInputs txMeta
        outputs = mkOutputs txMeta
    -- TODO(adn): Revisit this bit, it doesn't look transactional, unless
    -- 'runBeamSqliteDebug' runs the query within a DB transaction.
    in runBeamSqliteDebug putStrLn conn $ do
        SQL.runInsert $ SQL.insert (_mDbMeta metaDB)    $ SQL.insertValues [tMeta]
        SQL.runInsert $ SQL.insert (_mDbInputs metaDB)  $ SQL.insertValues inputs
        SQL.runInsert $ SQL.insert (_mDbOutputs metaDB) $ SQL.insertValues outputs

-- | Converts a database-fetched 'TxMeta' into a domain-specific 'Kernel.TxMeta'.
toTxMeta :: TxMeta -> NonEmpty TxInput -> NonEmpty TxOutput -> Kernel.TxMeta
toTxMeta txMeta inputs outputs = Kernel.TxMeta {
      _txMetaId         = _txMetaTableId txMeta
    , _txMetaAmount     = _txMetaTableAmount txMeta
    , _txMetaInputs     = fmap (reify . _getTxInput) inputs
    , _txMetaOutputs    = fmap (reify . _getTxOutput) outputs
    , _txMetaCreationAt = _txMetaTableCreatedAt txMeta
    , _txMetaIsLocal    = _txMetaTableIsLocal txMeta
    , _txMetaIsOutgoing = _txMetaTableIsOutgoing txMeta
    }
    where
        -- | Reifies the input 'TxCoinDistributionTableT' into a tuple suitable
        -- for a 'Kernel.TxMeta'.
        reify :: TxCoinDistributionTable -> (Core.Address, Core.Coin)
        reify coinDistr = (,) (_txCoinDistributionTableAddress coinDistr)
                              (_txCoinDistributionTableCoin coinDistr)

-- | Fetches a 'Kernel.TxMeta' from the database, given its 'Core.TxId'.
-- FIXME(adinapoli): Toggle debug/production with the 'WalletMode'.
-- FIXME(adinapoli): Exception & error handling.
getTxMeta :: MetaDBHandle -> Core.TxId -> IO (Maybe Kernel.TxMeta)
getTxMeta dbHandle txId =
    let conn = _mDbHandleConnection dbHandle
    in runBeamSqliteDebug putStrLn conn $ do
        metas <- SQL.runSelectReturningList txMetaById
        case metas of
            [txMeta] -> do
                inputs  <- nonEmpty <$> SQL.runSelectReturningList getInputs
                outputs <- nonEmpty <$> SQL.runSelectReturningList getOutputs
                pure $ toTxMeta <$> Just txMeta <*> inputs <*> outputs
            _        -> pure Nothing
    where
        txMetaById = SQL.lookup_ (_mDbMeta metaDB) (TxIdPrimKey txId)
        getInputs  = SQL.select $ do
            coinDistr <- SQL.all_ (_mDbInputs metaDB)
            SQL.guard_ ((_txCoinDistributionTxId . _getTxInput $ coinDistr) ==. (SQL.val_ $ TxIdPrimKey txId))
            pure coinDistr
        getOutputs = SQL.select $ do
            coinDistr <- SQL.all_ (_mDbOutputs metaDB)
            SQL.guard_ ((_txCoinDistributionTxId . _getTxOutput $ coinDistr) ==. (SQL.val_ $ TxIdPrimKey txId))
            pure coinDistr
