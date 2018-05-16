{-# LANGUAGE RankNTypes #-}
module Migrations
 ( migrationsSpec
 ) where

import Universum
import qualified Database.SQLite.Simple as Sqlite
import Util.Buildable.Hspec (Spec, shouldBe, it, describe)

import qualified Cardano.Wallet.Kernel.DB.Sqlite

--------------------------------------------------------------------------------
migrationsSpec :: Spec
migrationsSpec =
  describe "migrations" $ do
    it "Sqlite migration0 is as expected" $ do
      -- This test is here to prevent changes to 'migration0' to
      -- accidentally change SQL expectations we have had in the past.
      let actual = Cardano.Wallet.Kernel.DB.Sqlite.rawMigrationSql
                      Cardano.Wallet.Kernel.DB.Sqlite.migration0
      shouldBe (Sqlite.fromQuery actual)
               (Sqlite.fromQuery expected_sqlite_migration0)


expected_sqlite_migration0 :: Sqlite.Query
expected_sqlite_migration0 =
  -- WARNING: This blob is here to ensure backwards compatibility.
  -- You don't ever want to modify this. This SQL has already been deployed
  -- to users. Change your Haskell stuff instead.
  "CREATE TABLE \"tx_metas\"(\"meta_id\" VARCHAR NOT NULL  UNIQUE \
  \, \"meta_amount\" BIGINT NOT NULL , \"meta_created_at\" BIGINT NOT NULL \
  \, \"meta_is_local\" BOOLEAN NOT NULL , \"meta_is_outgoing\" BOOLEAN NOT \
  \NULL , PRIMARY KEY(\"meta_id\"))CREATE TABLE \"tx_metas_inputs\"\
  \(\"input_address\" VARCHAR NOT NULL , \"input_coin\" BIGINT NOT NULL \
  \, \"meta_id\" VARCHAR NOT NULL , PRIMARY KEY(\"input_address\"\
  \, \"meta_id\"))CREATE TABLE \"tx_metas_outputs\"(\"output_address\" \
  \VARCHAR NOT NULL , \"output_coin\" BIGINT NOT NULL , \"meta_id\" VARCHAR \
  \NOT NULL , PRIMARY KEY(\"output_address\", \"meta_id\"))"

