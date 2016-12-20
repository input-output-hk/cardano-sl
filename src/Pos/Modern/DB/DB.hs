-- | Higher-level DB functionality.

module Pos.Modern.DB.DB
       ( openNodeDBs
       , getTipBlock
       ) where

import           Control.Monad.Trans.Resource (MonadResource)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Universum

import           Pos.Modern.DB.Block          (getBlock)
import           Pos.Modern.DB.Class          (MonadDB)
import           Pos.Modern.DB.Error          (DBError (DBMalformed))
import           Pos.Modern.DB.Functions      (openDB)
import           Pos.Modern.DB.Holder         (runDBHolder)
import           Pos.Modern.DB.Types          (NodeDBs (..))
import           Pos.Modern.DB.Utxo           (getTip, prepareUtxoDB)
import           Pos.Ssc.Class.Types          (Ssc)
import           Pos.Types                    (Block)

-- | Open all DBs stored on disk.
openNodeDBs :: MonadResource m => FilePath -> m (NodeDBs ssc)
openNodeDBs fp = do
    let blockPath = fp </> "blocks"
    let utxoPath = fp </> "utxo"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists [blockPath, utxoPath, miscPath]
    res <- NodeDBs <$> openDB blockPath
                   <*> openDB utxoPath
                   <*> openDB miscPath
    res <$ (runDBHolder res prepareUtxoDB)
  where
    ensureDirectoryExists :: MonadIO m => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True

getTipBlock
    :: (Ssc ssc, MonadDB ssc m, MonadThrow m)
    => m (Block ssc)
getTipBlock = maybe onFailure pure =<< getBlock =<< getTip
  where
    onFailure = throwM $ DBMalformed "there is no block corresponding to tip"
