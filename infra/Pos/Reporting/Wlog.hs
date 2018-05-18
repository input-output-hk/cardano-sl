-- | Log file retrieval using log-warper, for the purpose of reporting.

module Pos.Reporting.Wlog
    ( withWlogTempFile
    , readWlogFile
    , retrieveLogFiles
    , compressLogs
    , withTempLogFile
    , LoggerConfig (..)
    ) where

import           Universum

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           System.Wlog (LoggerConfig (..), hwFilePath, lcTree, ltFiles,
                              ltSubloggers, retrieveLogContent)
import           Control.Lens (each, to)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit (runConduitRes, yield, (.|))
import           Data.Conduit.List (consume)
import qualified Data.Conduit.Lzma as Lzma
import qualified Data.HashMap.Strict as HM
import           Data.List (isSuffixOf)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (canonicalizePath, doesFileExist, getTemporaryDirectory,
                                   removeFile)
import           System.FilePath (takeFileName)
import           System.IO (IOMode (WriteMode), hClose, hFlush, withFile)

-- FIXME we get PackingError from here, but it should defined locally, since
-- it's log-warper specific.
import           Pos.Reporting.Exceptions (ReportingError (..))
import           Pos.Util.Filesystem (withSystemTempFile)
import           Pos.Util.Util ((<//>))


-- | Use a 'LoggerConfig' to get logs, write them to a temporary file,
-- tar and compress that file, canonicalize its path, run the continuation
-- with that path (or 'Nothing' if there are no logs), then clean up the
-- temporary files after the continuation has finished.
withWlogTempFile :: LoggerConfig -> (Maybe FilePath -> IO t) -> IO t
withWlogTempFile logConfig k = do
    mRawLogs <- readWlogFile logConfig
    case mRawLogs of
        Nothing -> k Nothing
        Just rawLogs -> withTempLogFile rawLogs $ \fp -> k (Just fp)

-- | Use a 'LoggerConfig' to get logs.
readWlogFile :: LoggerConfig -> IO (Maybe Text)
readWlogFile logConfig = case mLogFile of
    Nothing -> pure Nothing
    Just logFile -> do
        -- TBD will 'retrieveLogContent' fail if the file doesn't
        -- exist?
        logContent <-
            takeGlobalSize charsConst <$>
            retrieveLogContent logFile (Just 5000)
        pure (Just (unlines (reverse logContent)))
  where
    -- Grab all public log files, using the 'LoggerConfig', and take the
    -- first one.
    allFiles = map snd $ retrieveLogFiles logConfig
    mLogFile = head $ filter (".pub" `isSuffixOf`) allFiles
    -- 2 megabytes, assuming we use chars which are ASCII mostly
    charsConst :: Int
    charsConst = 1024 * 1024 * 2
    takeGlobalSize :: Int -> [Text] -> [Text]
    takeGlobalSize _ [] = []
    takeGlobalSize curLimit (t:xs) =
        let delta = curLimit - length t
        in bool [] (t : (takeGlobalSize delta xs)) (delta > 0)

-- | Given logger config, retrieves all (logger name, filepath) for
-- every logger that has file handle. Filepath inside does __not__
-- contain the common logger config prefix.
retrieveLogFiles :: LoggerConfig -> [([Text], FilePath)]
retrieveLogFiles lconfig = fromLogTree $ lconfig ^. lcTree
  where
    fromLogTree lt =
        let curElems = map ([],) (lt ^.. ltFiles . each . hwFilePath)
            iterNext (part, node) = map (first (part :)) $ fromLogTree node
        in curElems ++ concatMap iterNext (lt ^. ltSubloggers . to HM.toList)

-- | Pass a list of absolute paths to log files. This function will
-- archive and compress these files and put resulting file into log
-- directory (returning filepath is absolute).
--
-- It will throw a PackingError in case:
--   - Any of the file paths given does not point to an existing file.
--   - Any of the file paths could not be converted to a tar path, for instance
--     because it is too long.
compressLogs :: [FilePath] -> IO FilePath
compressLogs files = do
    tar <- tarPackIndependently files
    tarxz <-
        BS.concat <$>
        runConduitRes (yield tar .| Lzma.compress (Just 0) .| consume)
    aName <- getArchiveName
    withFile aName WriteMode $ \handle -> do
        BS.hPut handle tarxz
        hFlush handle
    pure aName
  where
    tarPackIndependently :: [FilePath] -> IO ByteString
    tarPackIndependently paths = do
        entries <- forM paths $ \p -> do
            unlessM (doesFileExist p) $ throwM $
                PackingError $ "can't pack log file " <> fromString p <>
                               " because it doesn't exist or it's not a file"
            tPath <- either (throwM . PackingError . fromString)
                            pure
                            (Tar.toTarPath False $ takeFileName p)
            pabs <- canonicalizePath p
            Tar.packFileEntry pabs tPath
        pure $ BSL.toStrict $ Tar.write entries
    getArchiveName = do
        curTime <- formatTime defaultTimeLocale "%q" <$> getCurrentTime
        tempDir <- getTemporaryDirectory
        pure $ tempDir <//> ("report-" <> curTime <> ".tar.lzma")

-- | Creates a temp file from given text
withTempLogFile :: Text -> (FilePath -> IO a) -> IO a
withTempLogFile rawLogs action = do
    withSystemTempFile "main.log" $ \tempFp tempHandle -> do
        let getArchivePath = do
                TIO.hPutStrLn tempHandle rawLogs
                hClose tempHandle
                archivePath <- compressLogs [tempFp]
                canonicalizePath archivePath
            removeArchive = removeFile
        bracket getArchivePath removeArchive action
