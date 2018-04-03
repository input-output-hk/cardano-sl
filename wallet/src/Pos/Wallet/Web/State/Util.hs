-- | Utility functions

module Pos.Wallet.Web.State.Util
    ( cleanupAcidStatePeriodically
    ) where

import           Universum

import           Data.Acid (createArchive, createCheckpoint)
import           Data.Time.Units (TimeUnit, Second)
import           Formatting (sformat, shown, (%))
import           Mockable (Delay, Mockable, delay)
import           Serokell.AcidState.ExtendedState (ExtendedState (..), extendedStateToAcid)
import           System.Directory (getModificationTime, listDirectory, removeFile)
import           System.FilePath ((</>))
import           System.Wlog (WithLogger, logDebug, logError)

import           Pos.Wallet.Web.State.State (WalletDB)

type MonadAcidCleanup ctx m =
    ( MonadIO m
    , MonadMask m
    , WithLogger m
    , Mockable Delay m
    )


-- | This worker does acid cleanup action every (passed)
-- interval. Action itself consists of two steps:
--
-- * Create checkpoint and archive.
-- * Delete all files in /Archive except for newest one.
cleanupAcidStatePeriodically ::
       forall m ctx t. (MonadAcidCleanup ctx m, TimeUnit t)
    => WalletDB
    -> t
    -> m ()
cleanupAcidStatePeriodically db interval = perform
  where
    perform = cleanupAction `catchAny` handler

    cleanupAction = forever $ do
        logDebug "Starting cleanup"
        let st = extendedStateToAcid db

        -- checkpoint/archive
        liftIO $ createCheckpoint st >> createArchive st
        logDebug "Created checkpoint/archived"

        -- cleanup old archive data
        let dbPathM = case db of
                         ESLocal _ p -> Just p
                         _           -> Nothing
        void $ flip catchAny (\e -> logError $ "Got error while cleaning up archive: " <> show e) $
            whenJust dbPathM $ \dbp -> do
                removed <- liftIO $ cleanupOld dbp
                logDebug $ "Removed " <> pretty removed <> " old archive files"

        delay interval

    handler :: SomeException -> m ()
    handler e = do
        let report = do
                logError $ sformat ("acidCleanupWorker failed with error: "%shown%
                                    " restarting in 1m")
                                   e
                delay (60 :: Second)
        report `finally` perform

    -- Returns how many files were deleted
    cleanupOld :: FilePath -> IO Int
    cleanupOld dbPath = do
        let archiveDir = dbPath </> "Archive"
        archiveCheckpoints <- map (archiveDir </>) <$> listDirectory archiveDir
        -- same files, but newest first
        newestFirst <-
            map fst . reverse . sortWith snd <$>
            mapM (\f -> (f,) <$> liftIO (getModificationTime f)) archiveCheckpoints
        let oldFiles = drop 10 newestFirst
        forM_ oldFiles removeFile
        pure $ length oldFiles
