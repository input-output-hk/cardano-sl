{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- | monitor log files for max age and max size

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Pos.Util.Log.Rotator
       ( cleanupRotator
       , evalRotator
       , initializeRotator
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..), catchIO)
import qualified Data.List.NonEmpty as NE
import           Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime,
                     parseTimeM)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (listDirectory, removeFile)
import           System.FilePath ((</>))
import           System.IO (BufferMode (LineBuffering), Handle,
                     IOMode (WriteMode), hFileSize, hSetBuffering, stdout)

import           Pos.Util.Log.Internal (FileDescription (..))
import           Pos.Util.Log.LoggerConfig

#ifdef POSIX
import           System.Directory (createFileLink)
import           System.FilePath (takeFileName)
#endif

-- Because of the System.Directory and System.FilePath
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

-- | format of a timestamp
tsformat :: String
tsformat = "%Y%m%d%H%M%S"

-- | get file path to a log file with current time
nameLogFile :: FileDescription -> IO FilePath
nameLogFile FileDescription{..} = do
    now <- getCurrentTime
    let tsnow = formatTime defaultTimeLocale tsformat now
    return $ prefixpath </> filename ++ "-" ++ tsnow

-- | open a new log file
evalRotator :: RotationParameters -> FileDescription -> IO (Handle, Integer, UTCTime)
evalRotator rotation fdesc@FileDescription{..} = do
    let maxAge   = toInteger $ rotation ^. rpMaxAgeHours
        maxSize  = toInteger $ rotation ^. rpLogLimitBytes

    -- open new log file
    fpath <- nameLogFile fdesc
    hdl <- catchIO (openFile fpath WriteMode) $
               \e -> do
                   prtoutException fpath e
                   return stdout    -- fallback to standard output in case of exception
    hSetBuffering hdl LineBuffering

#ifdef POSIX
    -- restrict symbolic links only for unix-like OS
    let symLinkPath = prefixpath </> filename
    -- delete a symlink if already exists and create a new
    -- one that points to the correct file.
    (removeFile symLinkPath) `catchIO`
        \e -> case isDoesNotExistError e of
            True  -> putStrLn ("No other symlinks already existed. Creating a new one, named:"
                        ++ show symLinkPath)
            False -> prtoutException symLinkPath e
    catchIO (createFileLink fpath symLinkPath) (\e ->
        do
            putStrLn $ "error while creating symlink: " ++ symLinkPath ++ " for " ++ fpath
            putStrLn $ "exception: " ++ displayException e)
#endif

    -- compute next rotation time
    now <- getCurrentTime
    let rottime = addUTCTime (fromInteger $ maxAge * 3600) now

    return (hdl, maxSize, rottime)

prtoutException :: Exception e => FilePath -> e -> IO ()
prtoutException fp e = do
    putStrLn $ "error while opening log @ " ++ fp
    putStrLn $ "exception: " ++ displayException e

-- | list filenames in prefix dir which match 'filename'
listLogFiles :: FileDescription -> IO (Maybe (NonEmpty FilePath))
listLogFiles FileDescription{..} = do
    -- find files in bp which begin with fp
    files <- listDirectory $ prefixpath
    return $ nonEmpty $ sort $ filter fpredicate files
  where
    tslen = 14  -- length of a timestamp
    fplen = length filename
    fpredicate path = take fplen path == filename
                      && take 1 (drop fplen path) == "-"
                      && length (drop (fplen + 1) path) == tslen

-- | latest log file in prefix dir which matches 'filename'
latestLogFile :: FileDescription -> IO (Maybe FilePath)
latestLogFile fdesc =
    listLogFiles fdesc >>= \fs -> return $ latestLogFile' fs
  where
    latestLogFile' :: Maybe (NonEmpty FilePath) -> Maybe FilePath
    latestLogFile' Nothing      = Nothing
    latestLogFile' (Just flist) = Just $ last flist

-- | initialize log file at startup
--   may append to existing file
initializeRotator :: RotationParameters -> FileDescription -> IO (Handle, Integer, UTCTime)
initializeRotator rotation fdesc = do
    let maxAge   = toInteger $ rotation ^. rpMaxAgeHours
        maxSize  = toInteger $ rotation ^. rpLogLimitBytes

    latest <- latestLogFile fdesc
    case latest of
        Nothing -> -- no file to append, return new
            evalRotator rotation fdesc
        Just fname -> do
            -- check date
            now <- getCurrentTime
            tsfp <- parseTimeM True defaultTimeLocale tsformat $ drop (fplen + 1) fname
            if (round $ diffUTCTime now tsfp) > (3600 * maxAge)
               then do  -- file is too old, return new
                  evalRotator rotation fdesc
               else do
                  hdl <- catchIO (openFile (prefixpath fdesc </> fname) AppendMode) $
                             \e -> do
                                 prtoutException fname e
                                 return stdout    -- fallback to standard output in case of exception
                  hSetBuffering hdl LineBuffering
                  cursize <- hFileSize hdl
                  let rottime = addUTCTime (fromInteger $ maxAge * 3600) tsfp
                  return (hdl, (maxSize - cursize), rottime)
  where
    fplen = length $ filename fdesc

-- | remove old files; count them and only keep n (from config)
cleanupRotator :: RotationParameters -> FileDescription -> IO ()
cleanupRotator rotation fdesc = do
    let keepN0 = fromIntegral (rotation ^. rpKeepFilesNum) :: Int
        keepN = max 1 $ min keepN0 99
    listLogFiles fdesc >>= removeOldFiles keepN
  where
    removeOldFiles :: Int -> Maybe (NonEmpty FilePath) -> IO ()
    removeOldFiles _ Nothing = return ()
    removeOldFiles n (Just flist) = do
        removeFiles $ reverse $ NE.drop n $ NE.reverse flist
    removeFiles [] = return ()
    removeFiles (fp : fps) = do
        let bp = prefixpath fdesc
            filepath = bp </> fp
        removeFile filepath   -- destructive
        removeFiles fps

{-

  testing:

  lc0 <- parseLoggerConfig "../log-configs/testing.yaml"
  lc <- setLogPrefix (Just "/tmp/testlog/") lc0
  lh <- setupLogging lc

  usingLoggerName lh "testing" $ do { forM_ [1..299] (\n -> logDebug $ T.pack $ "hello world " ++ (show n)) }
-}
