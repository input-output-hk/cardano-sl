-- | Higher-level DB functionality.

module Pos.Modern.DB.DB
       ( openNodeDBs
       ) where

import           Control.Monad.Trans.Resource (MonadResource)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Universum

import           Pos.Modern.DB.Functions      (openDB)
import           Pos.Modern.DB.Holder         (runDBHolder)
import           Pos.Modern.DB.Types          (NodeDBs (..))
import           Pos.Modern.DB.Utxo           (prepareUtxoDB)

-- | Open all DBs stored on disk.
openNodeDBs :: MonadResource m => FilePath -> m (NodeDBs ssc)
openNodeDBs fp = do
    let blockPath = fp </> "blocks"
    let utxoPath = fp </> "utxo"
    mapM_ ensureDirectoryExists [blockPath, utxoPath]
    res <- NodeDBs <$> openDB (fp </> "blocks") <*> openDB (fp </> "utxo")
    res <$ (runDBHolder res prepareUtxoDB)
  where
    ensureDirectoryExists :: MonadIO m => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True
