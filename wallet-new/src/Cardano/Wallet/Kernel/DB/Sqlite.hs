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
                     HasSqlValueSyntax (..), IsSql92DataTypeSyntax, valueE,
                     varCharType)
import           Database.Beam.Backend.SQL.SQL92 (Sql92OrderingExpressionSyntax,
                     Sql92SelectOrderingSyntax)
import           Database.Beam.Query (HasSqlEqualityCheck, between_, in_, (&&.),
                     (<.), (<=.), (==.), (>.), (>=.))
import qualified Database.Beam.Query as SQL
import qualified Database.Beam.Query.Internal as SQL
import           Database.Beam.Schema (Beamable, Database, DatabaseSettings,
                     PrimaryKey, Table)
import qualified Database.Beam.Schema as Beam
import           Database.Beam.Sqlite.Connection (Sqlite, runBeamSqlite,
                     runBeamSqlite)
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
import           Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty (toList)
import qualified Data.Map as M
import           Data.Time.Units (Second, fromMicroseconds, toMicroseconds)
import           Database.Beam.Migrate (CheckedDatabaseSettings, DataType (..),
                     Migration, MigrationSteps, boolean, createTable,
                     evaluateDatabase, executeMigration, field, migrationStep,
                     notNull, runMigrationSteps, unCheckDatabase, unique)
import           Formatting (sformat)
import           GHC.Generics (Generic)

import           Cardano.Wallet.Kernel.DB.TxMeta.Types (AccountFops (..),
                     FilterOperation (..), Limit (..), Offset (..),
                     SortCriteria (..), SortDirection (..), Sorting (..))
import qualified Cardano.Wallet.Kernel.DB.TxMeta.Types as Kernel
-- execution time should move out of WalletLayer so that we don`t depend on it.
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp
import           Pos.Crypto.Hashing (decodeAbstractHash, hashHexF)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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

| tx_meta_id | tx_meta_amount | tx_meta_created_at | tx_meta_is_local | tx_meta_is_outgoing | tx_meta_account | tx_meta_wallet
|------------+----------------+--------------------+------------------+---------------------|-----------------|-----------------
| Txp.TxId   | Core.Coin      | Core.Timestamp     | Bool             | Bool                | Word32          | Core.Address

--}
data TxMetaT f = TxMeta {
      _txMetaTableId         :: Beam.Columnar f Txp.TxId
    , _txMetaTableAmount     :: Beam.Columnar f Core.Coin
    , _txMetaTableCreatedAt  :: Beam.Columnar f Core.Timestamp
    , _txMetaTableIsLocal    :: Beam.Columnar f Bool
    , _txMetaTableIsOutgoing :: Beam.Columnar f Bool
    , _txMetaTableWalletId   :: Beam.Columnar f Core.Address
    , _txMetaTableAccountId  :: Beam.Columnar f Word32
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
                , _txMetaTableWalletId   = txMeta ^. Kernel.txMetaWalletId
                , _txMetaTableAccountId  = txMeta ^. Kernel.txMetaAccountId
                }

instance Beamable TxMetaT

instance Table TxMetaT where
    data PrimaryKey TxMetaT f = TxIdPrimKey (Beam.Columnar f Txp.TxId) deriving Generic
    primaryKey = TxIdPrimKey . _txMetaTableId

instance Beamable (PrimaryKey TxMetaT)

{--

* Table 2: ~tx_meta_inputs~
** Primary Index: ~tx_meta_input_address~
** Secondary Indexes: ~tx_meta_id~

| tx_meta_input_address | tx_meta_coin | tx_meta_id |
|-----------------------+--------------+------------|
| Core.Address          | Core.Coin    | Txp.TxId  |

** Table 3: ~tx_meta_outputs~
** Primary Index: ~tx_meta_output_address~
** Secondary Indexes: ~tx_meta_id~

| tx_meta_output_address | tx_meta_coin | tx_meta_id |
|------------------------+--------------+------------|
| Core.Address           | Core.Coin    | Txp.TxId  |

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
      mk :: Txp.TxId -> (Core.Address, Core.Coin) -> a
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

instance HasSqlValueSyntax SqliteValueSyntax Txp.TxId where
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


instance HasSqlEqualityCheck SqliteExpressionSyntax Txp.TxId

instance FromField Txp.TxId where
    fromField f = do
        h <- decodeAbstractHash <$> fromField f
        case h of
             Left _     -> returnError Sqlite.ConversionFailed f "not a valid hex hash"
             Right txid -> pure txid

instance FromBackendRow Sqlite Txp.TxId

instance HasSqlEqualityCheck SqliteExpressionSyntax Core.Address

instance HasSqlEqualityCheck SqliteExpressionSyntax Core.Timestamp

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

-- | 'DataType' declaration to convince @Beam@ treating 'Txp.TxId(s) as
-- varchars of arbitrary length.
txId :: IsSql92DataTypeSyntax syntax => DataType syntax Txp.TxId
txId = DataType (varCharType Nothing Nothing)

-- | 'DataType' declaration to convince @Beam@ treating 'Core.Coin(s) as
-- SQLite BIG INTEGER.
coin :: DataType SqliteDataTypeSyntax Core.Coin
coin = DataType sqliteBigIntType

walletid :: DataType SqliteDataTypeSyntax Core.Address
walletid = DataType (varCharType Nothing Nothing)

accountid :: DataType SqliteDataTypeSyntax Word32
accountid = DataType sqliteBigIntType

unlockTxId :: (TxCoinDistributionTableT f) -> Beam.Columnar f Txp.TxId
unlockTxId x =
    let TxIdPrimKey txid = _txCoinDistributionTxId x
    in txid

-- | Beam's 'Migration' to create a new 'MetaDB' Sqlite database.
initialMigration :: () -> Migration SqliteCommandSyntax (CheckedDatabaseSettings Sqlite MetaDB)
initialMigration () = do
    MetaDB <$> createTable "tx_metas"
                 (TxMeta (field "meta_id" txId notNull unique)
                         (field "meta_amount" coin notNull)
                         (field "meta_created_at" timestamp notNull)
                         (field "meta_is_local" boolean notNull)
                         (field "meta_is_outgoing" boolean notNull)
                         (field "meta_wallet_id" walletid notNull)
                         (field "meta_account_id" accountid notNull))
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
unsafeMigrateMetaDB conn = do
    void $ runMigrationSteps 0 Nothing migrateMetaDB (\_ _ -> executeMigration (Sqlite.execute_ conn . newSqlQuery))
    -- We don`t add Indexes on tx_metas (meta_id) because it`s unecessary for the PrimaryKey.
    Sqlite.execute_ conn "CREATE INDEX meta_created_at ON tx_metas (meta_created_at)"
    Sqlite.execute_ conn "CREATE INDEX meta_query ON tx_metas (meta_wallet_id, meta_account_id, meta_id, meta_created_at)"
    Sqlite.execute_ conn "CREATE INDEX inputs_id ON tx_metas_inputs (meta_id)"
    Sqlite.execute_ conn "CREATE INDEX outputs_id ON tx_metas_outputs (meta_id)"
    where
        newSqlQuery :: SqliteCommandSyntax -> Sqlite.Query
        newSqlQuery syntax =
            let sqlFragment = sqliteRenderSyntaxScript . fromSqliteCommand $ syntax
                in Sqlite.Query (decodeUtf8 sqlFragment)

-- | Simply a conveniency wrapper to avoid 'Kernel.TxMeta' to explicitly
-- import Sqlite modules.
newConnection :: FilePath -> IO Sqlite.Connection
newConnection path = Sqlite.open path
-- TODO: add a debug option for this: Sqlite.setTrace conn (Just putStrLn)

-- | Closes an open 'Connection' to the @Sqlite@ database stored in the
-- input 'MetaDBHandle'.
-- Even if open failed with error, this function should be called http://www.sqlite.org/c3ref/open.html
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
            let txid  = txMeta ^. Kernel.txMetaId
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
    , _txMetaAccountId  = _txMetaTableAccountId txMeta
    , _txMetaWalletId   = _txMetaTableWalletId txMeta
    }
    where
        -- | Reifies the input 'TxCoinDistributionTableT' into a tuple suitable
        -- for a 'Kernel.TxMeta'.
        reify :: TxCoinDistributionTable -> (Core.Address, Core.Coin)
        reify coinDistr = (,) (_txCoinDistributionTableAddress coinDistr)
                              (_txCoinDistributionTableCoin coinDistr)

-- | Fetches a 'Kernel.TxMeta' from the database, given its 'Txp.TxId'.
getTxMeta :: Sqlite.Connection -> Txp.TxId -> IO (Maybe Kernel.TxMeta)
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
           -> AccountFops
           -> Maybe Core.Address
           -> FilterOperation Txp.TxId
           -> FilterOperation Core.Timestamp
           -> Maybe Sorting
           -> IO ([Kernel.TxMeta], Maybe Int)
getTxMetas conn (Offset offset) (Limit limit) accountFops mbAddress fopTxId fopTimestamp mbSorting = do
    res <- Sqlite.runDBAction $ runBeamSqlite conn $ do

        -- The following 3 queries are disjointed and both fetches, respectively,
        -- @TxMeta@ @TxInput@ and @TxOutput@.
        -- The rationale behind doing three separate queries is that SQlite does not support
        -- array types, neither array aggregations (https://www.sqlite.org/lang_aggfunc.html).
        -- This in particular means the list of Inputs and Outputs for each TxId must be assembled
        -- in memory. One workarroun is to use group_concat(see link above), but this would return
        -- the array in a string format, separated by commas.

        -- The length of meta list is bounded by 50 in each realistic senario.
        -- So the marshalling between Haskell types - UTF8 - binary SQlite types shouldn`t
        -- be costly.
        meta <- SQL.runSelectReturningList $ SQL.select $ do
            case mbAddress of
                    Nothing   -> SQL.limit_ limit $ SQL.offset_ offset $ metaQuery
                    Just addr -> SQL.limit_ limit $ SQL.offset_ offset $ metaQueryWithAddr addr
        input <- SQL.runSelectReturningList $ SQL.select $ do
                input <- SQL.all_ $ _mDbInputs metaDB
                let TxIdPrimKey txid = _txCoinDistributionTxId $ _getTxInput input
                SQL.guard_ $ in_ txid (map (SQL.val_ . _txMetaTableId) meta)
                pure input
        output <- SQL.runSelectReturningList $ SQL.select $ do
                output <- SQL.all_ $ _mDbOutputs metaDB
                let TxIdPrimKey txid = _txCoinDistributionTxId $ _getTxOutput output
                SQL.guard_ $ in_ txid (map (SQL.val_ . _txMetaTableId) meta)
                pure output
        return $ do
             mt  <- nonEmpty meta
             inp <- nonEmpty input
             out <- nonEmpty output
             return (mt, inp, out)

    case res of
        Left e -> throwIO $ Kernel.StorageFailure (toException e)
        Right Nothing -> return ([], Just 0)
        Right (Just (meta, inputs, outputs)) ->  do
            eiCount <- limitExecutionTimeTo (25 :: Second) (\ _ -> ()) $ mapLeft $ Sqlite.runDBAction $ runBeamSqlite conn $
                case mbAddress of
                    Nothing   -> SQL.runSelectReturningOne $ SQL.select metaQueryC
                    Just addr -> SQL.runSelectReturningOne $ SQL.select $ metaQueryWithAddrC addr
            let mapWithInputs  = transform $ map (\inp -> (unlockTxId $ _getTxInput inp, inp)) inputs
            let mapWithOutputs = transform $ map (\out -> (unlockTxId $ _getTxOutput out, out)) outputs
            let txMeta = toValidKernelTxMeta mapWithInputs mapWithOutputs $ Data.List.NonEmpty.toList meta
                count = case eiCount of
                    Left _  -> Nothing
                    Right c -> c
            return (txMeta, count)

    where
        mapLeft :: IO (Either a b) -> IO (Either () b)
        mapLeft m  = do
            x <- m
            case x of
                Left _  -> return $ Left ()
                Right r -> return $ Right r

        filters meta = do
            SQL.guard_ $ filterAccs meta accountFops
            SQL.guard_ $ applyFilter (_txMetaTableId meta) fopTxId
            SQL.guard_ $ applyFilter (_txMetaTableCreatedAt meta) fopTimestamp
            pure ()

        filtersC meta = do
            SQL.guard_ $ filterAccs meta accountFops
            SQL.guard_ $ applyFilter (_txMetaTableId meta) fopTxId
            SQL.guard_ $ applyFilter (_txMetaTableCreatedAt meta) fopTimestamp
            pure ()

        metaQuery = do
            let query = SQL.all_ $ _mDbMeta metaDB
            meta <- case mbSorting of
                    Nothing -> query
                    Just (Sorting SortByCreationAt dir) ->
                        SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableCreatedAt) query
                    Just (Sorting SortByAmount     dir) ->
                        SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableAmount) query
            filters meta
            return meta

        metaQueryC = SQL.aggregate_ (\_ -> SQL.as_ SQL.countAll_) $ do
            meta <-  SQL.all_ $ _mDbMeta metaDB
            filtersC meta
            return meta

        findAndUnion addr = do
                let input = do
                        inp <- SQL.all_ $ _mDbInputs metaDB
                        SQL.guard_ $ ((_txCoinDistributionTableAddress . _getTxInput $ inp) ==. (SQL.val_ addr))
                        pure $ _txCoinDistributionTxId $ _getTxInput inp
                let output = do
                        out <- SQL.all_ $ _mDbOutputs metaDB
                        SQL.guard_ $ ((_txCoinDistributionTableAddress . _getTxOutput $ out) ==. (SQL.val_ addr))
                        pure $ _txCoinDistributionTxId $ _getTxOutput out
                -- union removes txId duplicates.
                txid <- SQL.union_ input output
                meta <- SQL.join_ (_mDbMeta metaDB)
                        (\ mt -> ((TxIdPrimKey $ _txMetaTableId mt) ==. txid))
                pure meta

        metaQueryWithAddr addr = do
            meta <- case mbSorting of
                Nothing -> findAndUnion addr
                Just (Sorting SortByCreationAt dir) ->
                    SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableCreatedAt) (findAndUnion addr)
                Just (Sorting SortByAmount     dir) ->
                    SQL.orderBy_ (toBeamSortDirection dir . _txMetaTableAmount) (findAndUnion addr)
            filters meta
            return meta

        metaQueryWithAddrC addr = SQL.aggregate_ (\_ -> SQL.as_ SQL.countAll_) $ do
            meta <- findAndUnion addr
            filtersC meta
            pure meta

        filterAccs _ Everything = SQL.QExpr (pure (valueE (sqlValueSyntax True)))
        filterAccs meta (AccountFops rootAddr (mbAccountId)) =
            (_txMetaTableWalletId meta ==. SQL.val_ rootAddr) &&.
                case mbAccountId of
                    Nothing -> SQL.QExpr (pure (valueE (sqlValueSyntax True)))
                    Just accountId -> _txMetaTableAccountId meta ==. SQL.val_ accountId

        applyFilter inputData fop =
            let byPredicate o i = case o of
                    Kernel.Equal            -> inputData ==. i
                    Kernel.LesserThan       -> inputData <. i
                    Kernel.GreaterThan      -> inputData >. i
                    Kernel.LesserThanEqual  -> inputData <=. i
                    Kernel.GreaterThanEqual -> inputData >=. i
            in case fop of
                NoFilterOp -> SQL.val_ True
                FilterByIndex a -> byPredicate Kernel.Equal (SQL.val_ a)
                FilterByPredicate ford a -> byPredicate ford (SQL.val_ a)
                FilterByRange from to -> between_ inputData (SQL.val_ from) (SQL.val_ to)
                FilterIn ls -> in_ inputData (map SQL.val_ ls)

        transform :: NonEmpty (Txp.TxId, a) -> M.Map Txp.TxId (NonEmpty a)
        transform = Foldable.foldl' updateFn M.empty

        updateFn :: M.Map Txp.TxId (NonEmpty a)
                -> (Txp.TxId, a)
                -> M.Map Txp.TxId (NonEmpty a)
        updateFn acc (txid, new) =
            M.insertWith (<>) txid (new :| []) acc

        toValidKernelTxMeta :: M.Map Txp.TxId (NonEmpty TxInput)
                             -> M.Map Txp.TxId (NonEmpty TxOutput)
                             -> [TxMeta]
                             -> [Kernel.TxMeta]
        toValidKernelTxMeta _ _ [] = []
        toValidKernelTxMeta inputMap outputMap (m : meta) =
            let txid = _txMetaTableId m
                io = do
                    inp <- M.lookup txid inputMap
                    out <- M.lookup txid outputMap
                    pure (inp, out)
            in case io of
                Nothing -> toValidKernelTxMeta inputMap outputMap meta
                Just (inputs, outputs) -> toTxMeta m inputs outputs : toValidKernelTxMeta inputMap outputMap meta

-- | Generates a Beam's AST fragment for use within a SQL query, to order
-- the results of a @SELECT@.
toBeamSortDirection :: SortDirection
                    -> SQL.QExpr (Sql92OrderingExpressionSyntax (Sql92SelectOrderingSyntax SqliteSelectSyntax)) s a
                    -> SQL.QOrd (Sql92SelectOrderingSyntax SqliteSelectSyntax) s a
toBeamSortDirection Ascending  = SQL.asc_
toBeamSortDirection Descending = SQL.desc_
