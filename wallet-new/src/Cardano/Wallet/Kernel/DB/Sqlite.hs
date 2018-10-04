{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Sqlite database for the 'TxMeta' portion of the wallet kernel.
module Cardano.Wallet.Kernel.DB.Sqlite (

    -- * Resource creation and acquisition
      newConnection
    , closeMetaDB

    -- * Basic API
    , putTxMeta
    , getTxMeta
    , getTxMetas

    -- * Unsafe functions
    , unsafeMigrateMetaDB
    ) where

import qualified Prelude
import           Universum

import           Database.Beam.Backend.SQL (FromBackendRow,
                     HasSqlValueSyntax (..), IsSql92DataTypeSyntax,
                     varCharType)
import           Database.Beam.Backend.SQL.SQL92 (Sql92OrderingExpressionSyntax,
                     Sql92SelectOrderingSyntax)
import           Database.Beam.Query (HasSqlEqualityCheck, (==.))
import qualified Database.Beam.Query as SQL
import qualified Database.Beam.Query.Internal as SQL
import           Database.Beam.Schema (Beamable, Database, DatabaseSettings,
                     PrimaryKey, Table)
import qualified Database.Beam.Schema as Beam
import           Database.Beam.Sqlite.Connection (Sqlite, runBeamSqlite)
import           Database.Beam.Sqlite.Syntax (SqliteCommandSyntax,
                     SqliteDataTypeSyntax, SqliteExpressionSyntax,
                     SqliteSelectSyntax, SqliteValueSyntax, fromSqliteCommand,
                     sqliteBigIntType, sqliteRenderSyntaxScript)
import qualified Database.SQLite.Simple as Sqlite
import           Database.SQLite.Simple.FromField (FromField (..), returnError)
import qualified Database.SQLite.SimpleErrors as Sqlite
import qualified Database.SQLite.SimpleErrors.Types as Sqlite

import           Control.Exception (throwIO, toException)
import           Control.Lens (Getter)
import qualified Data.Foldable as Foldable
import qualified Data.Map as M
import           Data.Time.Units (fromMicroseconds, toMicroseconds)
import           Database.Beam.Migrate (CheckedDatabaseSettings, DataType (..),
                     Migration, MigrationSteps, boolean, createTable,
                     evaluateDatabase, executeMigration, field, migrationStep,
                     notNull, runMigrationSteps, unCheckDatabase, unique)
import           Formatting (sformat)
import           GHC.Generics (Generic)

import           Cardano.Wallet.Kernel.DB.TxMeta.Types (Limit (..), Offset (..),
                     SortCriteria (..), SortDirection (..), Sorting (..))
import qualified Cardano.Wallet.Kernel.DB.TxMeta.Types as Kernel
import qualified Pos.Core as Core
import           Pos.Crypto.Hashing (decodeAbstractHash, hashHexF)


-- | A type modelling the underlying SQL database.
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

deriving instance Eq TxMeta
deriving instance Show TxMeta

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
newtype TxInputT f = TxInput  {
  _getTxInput  :: (TxCoinDistributionTableT f)
  } deriving Generic

type TxInput = TxInputT Identity

instance Beamable TxInputT

instance Table TxInputT where
    data PrimaryKey TxInputT f = TxInputPrimKey (Beam.Columnar f Core.Address) (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey (TxInput i) = TxInputPrimKey (_txCoinDistributionTableAddress i) (_txCoinDistributionTxId i)

instance Beamable (PrimaryKey TxInputT)

-- | Generalisation of 'mkInputs' and 'mkOutputs'.
mkCoinDistribution :: forall a. Kernel.TxMeta
                   -> (Getter Kernel.TxMeta (NonEmpty (Core.Address, Core.Coin)))
                   -> (TxCoinDistributionTable -> a)
                   -> NonEmpty a
mkCoinDistribution txMeta getter builder =
    let distribution  = txMeta ^. getter
        txid          = txMeta ^. Kernel.txMetaId
    in fmap (mk txid) distribution
  where
      mk :: Core.TxId -> (Core.Address, Core.Coin) -> a
      mk tid (addr, amount) = builder (TxCoinDistributionTable addr amount (TxIdPrimKey tid))

-- | Convenient constructor of a list of 'TxInput' from a 'Kernel.TxMeta'.
mkInputs :: Kernel.TxMeta -> NonEmpty TxInput
mkInputs txMeta = mkCoinDistribution txMeta Kernel.txMetaInputs TxInput

-- | The outputs' table.
newtype TxOutputT f = TxOutput {
  _getTxOutput :: (TxCoinDistributionTableT f)
  } deriving Generic

type TxOutput = TxOutputT Identity

instance Beamable TxOutputT

instance Table TxOutputT where
    data PrimaryKey TxOutputT f = TxOutputPrimKey (Beam.Columnar f Core.Address) (Beam.PrimaryKey TxMetaT f) deriving Generic
    primaryKey (TxOutput o) = TxOutputPrimKey (_txCoinDistributionTableAddress o) (_txCoinDistributionTxId o)

instance Beamable (PrimaryKey TxOutputT)

-- | Convenient constructor of a list of 'TxOutput from a 'Kernel.TxMeta'.
mkOutputs :: Kernel.TxMeta -> NonEmpty TxOutput
mkOutputs txMeta = mkCoinDistribution txMeta Kernel.txMetaOutputs TxOutput


-- Orphans & other boilerplate

instance HasSqlValueSyntax SqliteValueSyntax Core.TxId where
    sqlValueSyntax txid = sqlValueSyntax (sformat hashHexF txid)

instance HasSqlValueSyntax SqliteValueSyntax Core.Coin where
    sqlValueSyntax = sqlValueSyntax . Core.unsafeGetCoin

-- NOTE(adn) As reported by our good lad Matt Parsons, 'Word64' has enough
-- precision to justify the downcast:
--
-- >>> λ> import Data.Time
-- >>> λ> import Data.Time.Clock.POSIX
-- >>> λ> import Data.Word
-- >>> λ> :set -XNumDecimals
-- >>> λ> posixSecondsToUTCTime (fromIntegral ((maxBound :: Word64) `div` 1e6))
-- 586524-01-19 08:01:49 UTC
--
instance HasSqlValueSyntax SqliteValueSyntax Core.Timestamp where
    sqlValueSyntax ts = sqlValueSyntax (fromIntegral @Integer @Word64 . toMicroseconds . Core.getTimestamp $ ts)

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
    fromField f = Core.Timestamp . fromMicroseconds . toInteger @Word64 <$> fromField f

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
metaDB = unCheckDatabase (evaluateDatabase migrateMetaDB)

-- | 'DataType' declaration to convince @Beam@ treating 'Core.Address'(es) as
-- varchars of arbitrary length.
address :: DataType SqliteDataTypeSyntax Core.Address
address = DataType (varCharType Nothing Nothing)

-- | 'DataType' declaration to convince @Beam@ treating 'Core.Timestamp'(s) as
-- SQLite BIG INTEGER.
timestamp :: DataType SqliteDataTypeSyntax Core.Timestamp
timestamp = DataType sqliteBigIntType

-- | 'DataType' declaration to convince @Beam@ treating 'Core.TxId(s) as
-- varchars of arbitrary length.
txId :: IsSql92DataTypeSyntax syntax => DataType syntax Core.TxId
txId = DataType (varCharType Nothing Nothing)

-- | 'DataType' declaration to convince @Beam@ treating 'Core.Coin(s) as
-- SQLite BIG INTEGER.
coin :: DataType SqliteDataTypeSyntax Core.Coin
coin = DataType sqliteBigIntType

-- | Beam's 'Migration' to create a new 'MetaDB' Sqlite database.
initialMigration :: () -> Migration SqliteCommandSyntax (CheckedDatabaseSettings Sqlite MetaDB)
initialMigration () = do
    MetaDB <$> createTable "tx_metas"
                 (TxMeta (field "meta_id" txId notNull unique)
                         (field "meta_amount" coin notNull)
                         (field "meta_created_at" timestamp notNull)
                         (field "meta_is_local" boolean notNull)
                         (field "meta_is_outgoing" boolean notNull))
           <*> createTable "tx_metas_inputs"
                 (TxInput (TxCoinDistributionTable (field "input_address" address notNull)
                                                   (field "input_coin" coin notNull)
                                                   (TxIdPrimKey (field "meta_id" txId notNull))
                          ))
           <*> createTable "tx_metas_outputs"
                 (TxOutput (TxCoinDistributionTable (field "output_address" address notNull)
                                                    (field "output_coin" coin notNull)
                                                    (TxIdPrimKey (field "meta_id" txId notNull))
                           ))

--- | The full list of migrations available for this 'MetaDB'.
-- For a more interesting migration, see: https://github.com/tathougies/beam/blob/d3baf0c77b76b008ad34901b47a818ea79439529/beam-postgres/examples/Pagila/Schema.hs#L17-L19
migrateMetaDB :: MigrationSteps SqliteCommandSyntax () (CheckedDatabaseSettings Sqlite MetaDB)
migrateMetaDB = migrationStep "Initial migration" initialMigration


-- | Migrates the 'MetaDB', potentially mangling the input database.
-- TODO(adinapoli): Make it safe.
unsafeMigrateMetaDB :: Sqlite.Connection -> IO ()
unsafeMigrateMetaDB conn =
    void $ runMigrationSteps 0 Nothing migrateMetaDB (\_ _ -> executeMigration (Sqlite.execute_ conn . newSqlQuery))
    where
        newSqlQuery :: SqliteCommandSyntax -> Sqlite.Query
        newSqlQuery syntax =
            let sqlFragment = sqliteRenderSyntaxScript . fromSqliteCommand $ syntax
                in Sqlite.Query (decodeUtf8 sqlFragment)

-- | Simply a conveniency wrapper to avoid 'Kernel.TxMeta' to explicitly
-- import Sqlite modules.
newConnection :: FilePath -> IO Sqlite.Connection
newConnection = Sqlite.open

-- | Closes an open 'Connection' to the @Sqlite@ database stored in the
-- input 'MetaDBHandle'.
closeMetaDB :: Sqlite.Connection -> IO ()
closeMetaDB = Sqlite.close

-- | Inserts a new 'Kernel.TxMeta' in the database, given its opaque
-- 'MetaDBHandle'.
putTxMeta :: Sqlite.Connection -> Kernel.TxMeta -> IO ()
putTxMeta conn txMeta =
    let tMeta   = mkTxMeta txMeta
        inputs  = mkInputs txMeta
        outputs = mkOutputs txMeta
    in do
        res <- Sqlite.withTransaction conn $ Sqlite.runDBAction $ runBeamSqlite conn $ do
            SQL.runInsert $ SQL.insert (_mDbMeta metaDB)    $ SQL.insertValues [tMeta]
            SQL.runInsert $ SQL.insert (_mDbInputs metaDB)  $ SQL.insertValues (toList inputs)
            SQL.runInsert $ SQL.insert (_mDbOutputs metaDB) $ SQL.insertValues (toList outputs)
        case res of
             Left e   -> handleResponse e
             Right () -> return ()
    where
        -- Handle the 'SQLiteResponse', rethrowing the exception or turning
        -- \"controlled failures\" (like the presence of a duplicated
        -- transaction) in a no-op.
        handleResponse :: Sqlite.SQLiteResponse -> IO ()
        handleResponse e =
            let txid    = txMeta ^. Kernel.txMetaId
            in case e of
                -- NOTE(adinapoli): It's probably possible to make this match the
                -- Beam schema by using something like 'IsDatabaseEntity' from
                -- 'Database.Beam.Schema.Tables', but we have a test to catch
                -- regression in this area.
                (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas_inputs.input_address, tx_metas_inputs.meta_id") -> do
                   let err = Kernel.DuplicatedInputIn txid
                   throwIO $ Kernel.InvariantViolated err

                (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas_outputs.output_address, tx_metas_outputs.meta_id") -> do
                   let err = Kernel.DuplicatedOutputIn txid
                   throwIO $ Kernel.InvariantViolated err

                (Sqlite.SQLConstraintError Sqlite.Unique "tx_metas.meta_id") -> do
                    -- Check if the 'TxMeta' already present is a @different@
                    -- one, in which case this is a proper bug there is no
                    -- recover from. If the 'TxMeta' is the same, this is effectively
                    -- a no-op.
                    -- NOTE: We use a shallow equality check here because if the
                    -- input 'TxMeta' has the same inputs & outputs but in a different
                    -- order, the 'consistencyCheck' would yield 'False', when in
                    -- reality should be virtually considered the same 'TxMeta'.
                    consistencyCheck <- fmap (Kernel.isomorphicTo txMeta) <$> getTxMeta conn txid
                    case consistencyCheck of
                         Nothing    ->
                             throwIO $ Kernel.InvariantViolated (Kernel.UndisputableLookupFailed "consistencyCheck" txid)
                         Just False ->
                             throwIO $ Kernel.InvariantViolated (Kernel.DuplicatedTransactionWithDifferentHash txid)
                         Just True  ->
                             -- both "hashes" matched, this is genuinely the
                             -- same 'Tx' being inserted twice, probably as
                             -- part of a rollback.
                             return ()

                _ -> throwIO $ Kernel.StorageFailure (toException e)


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
getTxMeta :: Sqlite.Connection -> Core.TxId -> IO (Maybe Kernel.TxMeta)
getTxMeta conn txid = do
    res <- Sqlite.runDBAction $ runBeamSqlite conn $ do
        metas <- SQL.runSelectReturningList txMetaById
        case metas of
            [txMeta] -> do
                inputs  <- nonEmpty <$> SQL.runSelectReturningList getInputs
                outputs <- nonEmpty <$> SQL.runSelectReturningList getOutputs
                pure $ toTxMeta <$> Just txMeta <*> inputs <*> outputs
            _        -> pure Nothing
    case res of
         Left e  -> throwIO $ Kernel.StorageFailure (toException e)
         Right r -> return r
    where
        txMetaById = SQL.lookup_ (_mDbMeta metaDB) (TxIdPrimKey txid)
        getInputs  = SQL.select $ do
            coinDistr <- SQL.all_ (_mDbInputs metaDB)
            SQL.guard_ ((_txCoinDistributionTxId . _getTxInput $ coinDistr) ==. (SQL.val_ $ TxIdPrimKey txid))
            pure coinDistr
        getOutputs = SQL.select $ do
            coinDistr <- SQL.all_ (_mDbOutputs metaDB)
            SQL.guard_ ((_txCoinDistributionTxId . _getTxOutput $ coinDistr) ==. (SQL.val_ $ TxIdPrimKey txid))
            pure coinDistr


newtype OrdByCreationDate = OrdByCreationDate { _ordByCreationDate :: TxMeta } deriving (Show, Eq)

instance Ord OrdByCreationDate where
    compare a b = compare (_txMetaTableCreatedAt . _ordByCreationDate $ a)
                          (_txMetaTableCreatedAt . _ordByCreationDate $ b)

getTxMetas :: Sqlite.Connection
           -> Offset
           -> Limit
           -> Maybe Sorting
           -> IO [Kernel.TxMeta]
getTxMetas conn (Offset offset) (Limit limit) mbSorting = do
    res <- Sqlite.runDBAction $ runBeamSqlite conn $ do
        metasWithInputs  <- nonEmpty <$> SQL.runSelectReturningList paginatedInputs
        metasWithOutputs <- nonEmpty <$> SQL.runSelectReturningList paginatedOutputs
        return $ liftM2 (,) metasWithInputs metasWithOutputs
    case res of
        Left e  -> throwIO $ Kernel.StorageFailure (toException e)
        Right Nothing -> return []
        Right (Just (inputs, outputs)) ->
          let mapWithInputs  = transform inputs
              mapWithOutputs = transform outputs
          -- Do a final round of in-memory sorting after folding the
          -- results. Note how this is unavoidable (without complicating
          -- the implementation consistently) due to the fact we get
          -- sorted data out of the DB but we need to post-process it to
          -- construct a real 'Kernel.TxMeta'.
          -- In practice, this is not as bad as it sounds: even though
          -- the 'Limit' is basically unbounded as part of this function,
          -- there exist a hard-limit as part of the wallet API, which would
          -- ensure that the in-memory sorting will never be too expensive.
          in return $ maybe identity (sortBy . toOrdering) mbSorting
                    $ Foldable.foldl' (toValidKernelTxMeta mapWithInputs) mempty (M.toList mapWithOutputs)
    where
        getTx selector = _txCoinDistributionTxId . selector

        metaQuery = case mbSorting of
            Nothing  -> SQL.all_ (_mDbMeta metaDB)
            Just (Sorting SortByCreationAt dir) ->
                SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableCreatedAt) $ SQL.all_ (_mDbMeta metaDB)
            Just (Sorting SortByAmount     dir) ->
                SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableAmount) $ SQL.all_ (_mDbMeta metaDB)

        -- The following two queries are disjointed and both fetches, respectively,
        -- a list of tuples of type @(TxMeta, TxInput)@ and @(TxMeta, TxOutput)@.
        -- The rationale behind doing two separate queries is that there is no elegant
        -- way to express a 3-table join without having 'Maybe's cropping up in the
        -- Haskell result sets, and furthermore we would have to deal with duplicates.
        -- Doing two separate queries is yes more I/O taxing, but spares us from doing
        -- duplicate filtering on the Haskell side.
        paginatedInputs = SQL.select $ do
            meta   <- SQL.limit_ limit (SQL.offset_ offset metaQuery)
            inputs  <- SQL.oneToMany_ (_mDbInputs metaDB)  (getTx _getTxInput) meta
            pure (meta, inputs)

        paginatedOutputs = SQL.select $ do
            meta   <- SQL.limit_ limit (SQL.offset_ offset metaQuery)
            outputs <- SQL.oneToMany_ (_mDbOutputs metaDB) (getTx _getTxOutput) meta
            pure (meta, outputs)

        -- | Groups the inputs or the outputs under the same 'TxMeta'.
        transform :: NonEmpty (TxMeta, a) -> M.Map OrdByCreationDate (NonEmpty a)
        transform = Foldable.foldl' updateFn M.empty

        updateFn :: M.Map OrdByCreationDate (NonEmpty a)
                 -> (TxMeta, a)
                 -> M.Map OrdByCreationDate (NonEmpty a)
        updateFn acc (txMeta, new) =
            M.insertWith (<>) (OrdByCreationDate txMeta) (new :| []) acc

        toValidKernelTxMeta :: M.Map OrdByCreationDate (NonEmpty TxInput)
                            -> [Kernel.TxMeta]
                            -> (OrdByCreationDate, NonEmpty TxOutput)
                            -> [Kernel.TxMeta]
        toValidKernelTxMeta inputMap acc (OrdByCreationDate t, outputs) =
            case M.lookup (OrdByCreationDate t) inputMap of
                 Nothing     -> acc
                 Just inputs -> toTxMeta t inputs outputs : acc

-- | Generates a Beam's AST fragment for use within a SQL query, to order
-- the results of a @SELECT@.
toBeamSortDirection :: SortDirection
                    -> SQL.QExpr (Sql92OrderingExpressionSyntax (Sql92SelectOrderingSyntax SqliteSelectSyntax)) s a
                    -> SQL.QOrd (Sql92SelectOrderingSyntax SqliteSelectSyntax) s a
toBeamSortDirection Ascending  = SQL.asc_
toBeamSortDirection Descending = SQL.desc_

-- | Generates a function suitable for 'sortBy' out of a 'Sorting'.
toOrdering :: Sorting -> (Kernel.TxMeta -> Kernel.TxMeta -> Ordering)
toOrdering (Sorting criteria dir) =
    let comparator Ascending  = compare
        comparator Descending = flip compare
    in case criteria of
            SortByCreationAt ->
                \t1 t2 -> (comparator dir) (t1 ^. Kernel.txMetaCreationAt)
                                           (t2 ^. Kernel.txMetaCreationAt)
            SortByAmount ->
                \t1 t2 -> (comparator dir) (t1 ^. Kernel.txMetaAmount)
                                           (t2 ^. Kernel.txMetaAmount)

