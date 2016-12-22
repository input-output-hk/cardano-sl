-- | Higher-level DB functionality.

module Pos.DB.DB
       ( openNodeDBs
       , getTipBlock
       ) where

import           Control.Monad.Trans.Resource (MonadResource)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Universum

import           Pos.DB.Block                 (getBlock)
import           Pos.DB.Class                 (MonadDB)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (openDB)
import           Pos.DB.Holder                (runDBHolder)
import           Pos.DB.Types                 (NodeDBs (..))
import           Pos.DB.Utxo                  (getTip, prepareUtxoDB)
import           Pos.Ssc.Class.Types          (Ssc)
import           Pos.Types                    (Block, Utxo)

-- | Open all DBs stored on disk.
openNodeDBs :: MonadResource m => FilePath -> Utxo -> m (NodeDBs ssc)
openNodeDBs fp customUtxo = do
    let blockPath = fp </> "blocks"
    let utxoPath = fp </> "utxo"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists [blockPath, utxoPath, miscPath]
    res <- NodeDBs <$> openDB blockPath
                   <*> openDB utxoPath
                   <*> openDB miscPath
    res <$ (runDBHolder res $ prepareUtxoDB customUtxo)
  where
    ensureDirectoryExists :: MonadIO m => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True

getTipBlock
    :: (Ssc ssc, MonadDB ssc m)
    => m (Block ssc)
getTipBlock = maybe onFailure pure =<< getBlock =<< getTip
  where
    onFailure = throwM $ DBMalformed "there is no block corresponding to tip"
