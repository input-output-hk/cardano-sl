{-- | Plugin submodule specialised in operating on the AcidState database.
--}

module Cardano.Wallet.Server.Plugins.AcidState (
    createAndArchiveCheckpoints
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           System.Directory (getModificationTime, listDirectory,
                     removeFile)
import           System.FilePath ((</>))

import           Data.Acid (AcidState, createArchive, createCheckpoint)
import           Data.Time.Units (Minute, toMicroseconds)

import           Pos.Util.Wlog (logError, logInfo)

import qualified Cardano.Wallet.Kernel.Mode as Kernel


-- | Creates a new acid-state @checkpoint@ every 'Minute' minutes, also
-- deleting old ones expect for the most recent one.
createAndArchiveCheckpoints :: AcidState db -> Minute -> Kernel.WalletMode ()
createAndArchiveCheckpoints dbRef delay = do
    logInfo "createAndArchiveCheckpoints is starting..."

    res <- liftIO $ try (createCheckpoint dbRef >> createArchive dbRef)
    case res of
         Left (err :: SomeException) -> logError (show err)
         Right ()                    -> pruneOldArchives

    --  Wait for the next compaction cycle.
    liftIO $ threadDelay (fromInteger $ toMicroseconds delay)
    createAndArchiveCheckpoints dbRef delay

  where
    -- Prunes old acid-state archives.
    pruneOldArchives :: Kernel.WalletMode ()
    pruneOldArchives = liftIO $ do
        let dbPath = undefined
        let archiveDir = dbPath </> "Archive"
        archiveCheckpoints <- map (archiveDir </>) <$> listDirectory archiveDir
        -- same files, but newest first
        newestFirst <-
            map fst . reverse . sortWith snd <$>
            mapM (\f -> (f,) <$> liftIO (getModificationTime f)) archiveCheckpoints
        let oldFiles = drop 10 newestFirst
        forM_ oldFiles removeFile

        logInfo ("pruneOldArchives pruned " <> show (length oldFiles) <> " old archives.")

