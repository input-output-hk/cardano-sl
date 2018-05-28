{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Migrations where


import Universum
import Data.Traversable (for)
import qualified Control.Exception as Ex
import Control.Lens (review)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text.Encoding as T
import qualified Database.SQLite.Simple as Sqlite
import qualified System.Wlog as Wlog

import qualified Pos.Client.Txp.History as TxHist
import qualified Pos.Core.Txp as Txp
import qualified Pos.Core as Core
import Pos.Crypto.Hashing (decodeAbstractHash)
import qualified Pos.Wallet.Web.State.Storage as WS
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import qualified Cardano.Wallet.Kernel.DB.TxMeta.Types as TxMeta

import qualified Cardano.Wallet.Kernel.DB.Sqlite

--------------------------------------------------------------------------------

txMetaFromWalletStorage :: WS.WalletStorage -> Either String [TxMeta.TxMeta]
txMetaFromWalletStorage ws = fmap join $ do
  for (HM.toList (WS._wsTxHistory ws)) $ \(walId, mMeta) -> do
      mHist <- case HM.lookup walId (WS._wsHistoryCache ws) of
          Nothing -> Left "WalId missing from history cache"
          Just x -> Right (x :: Map Core.TxId TxHist.TxHistoryEntry)
      for (HM.toList mMeta) $ \(cTxId, meta) -> do
          txId <- maybe (Left "Invalid CTxId") Right (fromCTxId cTxId)
          hist <- case Map.lookup txId mHist of
              Nothing -> Left "TxId missing from metadata"
              Just x -> Right (x :: TxHist.TxHistoryEntry)
          ins :: NEL.NonEmpty (Core.Address, Core.Coin) <-
              case NEL.nonEmpty (TxHist._thInputs hist) of
                  Nothing -> Left "No transaction inputs"
                  Just x -> Right (fmap (\(Txp.TxOut a c) -> (a,c)) x)
          let outs :: NEL.NonEmpty (Core.Address, Core.Coin)
              outs = fmap (\(Txp.TxOut a c) -> (a,c))
                          (Txp._txOutputs (TxHist._thTx hist))
          let inouts :: Set.Set Core.Address
              inouts = Set.fromList (toList (fmap fst (ins <> outs)))
          let ours :: Set.Set Core.Address
              ours = Set.fromList (HM.keys (WS._wsUsedAddresses ws <>
                                            WS._wsChangeAddresses ws))
          pure (TxMeta.TxMeta
              { TxMeta._txMetaId = TxHist._thTxId hist
              , TxMeta._txMetaCreationAt =
                   review Core.timestampSeconds (WebTypes.ctmDate meta)
              , TxMeta._txMetaInputs = ins
              , TxMeta._txMetaOutputs = outs
              , TxMeta._txMetaAmount = error "What is this?"
              , TxMeta._txMetaIsLocal = inouts `Set.isSubsetOf` ours
              , TxMeta._txMetaIsOutgoing =
                   -- At least one of `ins` is in `ours`
                   not (Set.null (Set.intersection
                      ours (Set.fromList (toList (fmap fst ins)))))
              })

fromCTxId :: WebTypes.CTxId -> Maybe Core.TxId
fromCTxId (WebTypes.CTxId (WebTypes.CHash t0)) = do
   bs0 <- decodeBase16 (T.encodeUtf8 t0)
   t1 <- either (const Nothing) Just (T.decodeUtf8' bs0)
   either (const Nothing) Just (decodeAbstractHash t1)

decodeBase16 :: B.ByteString -> Maybe B.ByteString
decodeBase16 = \x ->
   let (y, z) = Base16.decode x
   in if B.null z then Just y else Nothing

-- --------------------------------------------------------------------------------
-- -- Sqlite support
--
-- runSqliteMigration
--   :: Sqlite.Connection -> Migration SqliteCommandSyntax a -> IO ()
-- runSqliteMigration conn mig = Sqlite.execute_ conn
--   (Cardano.Wallet.Kernel.DB.Sqlite.rawMigrationSql mig)
