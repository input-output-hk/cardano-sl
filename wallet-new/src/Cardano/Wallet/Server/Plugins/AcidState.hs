{-- | Plugin submodule specialised in operating on the AcidState database.
--}

module Cardano.Wallet.Server.Plugins.AcidState
    ( createAndArchiveCheckpoints
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Data.Acid (AcidState, createArchive, createCheckpoint)
import qualified Data.ByteString.Lazy as B
import           Data.List (isInfixOf)
import           Data.Time (defaultTimeLocale, formatTime, getCurrentTime,
                     iso8601DateFormat)
import           Data.Time.Units (Minute, toMicroseconds)
import           System.Directory (getModificationTime, listDirectory,
                     removeFile)
import           System.FilePath ((</>))

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import           Pos.Util.Wlog (logError, logInfo)

import           Cardano.Wallet.Kernel (DatabaseMode (..), DatabaseOptions (..))
import qualified Cardano.Wallet.Kernel.Mode as Kernel


-- | Creates a new acid-state @checkpoint@ every 'Minute' minutes, also
-- deleting old ones expect for the most recent one.
createAndArchiveCheckpoints
    :: AcidState db
    -> Minute
    -> DatabaseMode
    -- ^ Path for the acid-state database.
    -> Kernel.WalletMode ()
createAndArchiveCheckpoints dbRef delay dbMode =
    case dbMode of
        UseInMemory         -> pure ()
        UseFilePath dbPaths -> forever (go (dbPathAcidState dbPaths))
  where
    go :: FilePath -> Kernel.WalletMode ()
    go dbPath = do
        logInfo "createAndArchiveCheckpoints is starting..."

        res <- try $ do
            liftIO (createCheckpoint dbRef >> createArchive dbRef)
            pruneAndCompress 3 dbPath
        case res of
             Left (err :: SomeException) -> logError (show err)
             Right ()                    -> return ()

        --  Wait for the next compaction cycle.
        liftIO . threadDelay . fromInteger $ toMicroseconds delay

-- | Prunes old acid-state archives, keeping only the most @n@ recent of them.
-- After the clean-up it tar and gzip compressed them.
pruneAndCompress :: Int
                 -- ^ How many to keep.
                 -> FilePath
                 -- ^ The path to the database folder
                 -> Kernel.WalletMode ()
pruneAndCompress n dbPath = liftIO $ do
    let archiveDir  = dbPath </> "Archive"
        fullRelPath = (archiveDir </>)
    archives <- listDirectory archiveDir

    newestFirst <-
        map fst . reverse . sortWith snd <$>
        sequence [(f,) <$> getModificationTime (fullRelPath f) | f <- archives]

    -- Partition the files into @toPrune@ and @toCompress@ (the @n@ newest).
    -- Filter from the second subset any previously-created tarball.
    let (toCompress, toPrune) = first (filter (not . isTarball))
                              . splitAt n
                              $ newestFirst

    -- Prune the old archives (including tarballs, if necessary).
    mapM_ (removeFile . fullRelPath) toPrune
    logInfo ("pruneAndCompress pruned " <> show (length toPrune) <> " old archives.")

    now <- getCurrentTime
    let tarName = "archive_"
                <> formatTime defaultTimeLocale (iso8601DateFormat (Just "%H_%M_%S")) now
                <> ".tar.gz"

    -- Compress and archive (no pun intended) the rest.
    -- As per documentation, 'Tar.pack' wants @toCompress@ to be relative paths
    -- to @archiveDir@.
    B.writeFile (archiveDir </> tarName) . GZip.compress
                                         . Tar.write =<< Tar.pack archiveDir toCompress

    logInfo ("pruneAndCompress compressed " <> show (length toCompress) <> " archives.")

    -- Remove the archived files.
    mapM_ (removeFile . fullRelPath) toCompress
  where
      isTarball :: FilePath -> Bool
      isTarball fp = ".tar.gz" `isInfixOf` fp
