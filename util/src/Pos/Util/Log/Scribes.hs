{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | provide backends for `katip`

module Pos.Util.Log.Scribes
    ( mkStdoutScribe
    , mkStderrScribe
    , mkDevNullScribe
    , mkTextFileScribe
    , mkJsonFileScribe
    ) where

import           Universum hiding (whenM)

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent.MVar (modifyMVar_)
import           Control.Exception.Safe (catchIO)

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time (diffUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Katip.Core
import           Katip.Scribes.Handle (brackets)

import qualified Pos.Util.Log.Internal as Internal
import           Pos.Util.Log.LoggerConfig (NamedSeverity,
                     RotationParameters (..))
import           Pos.Util.Log.LoggerName (LoggerName)
import           Pos.Util.Log.Rotator (cleanupRotator, evalRotator,
                     initializeRotator)
import qualified Pos.Util.Log.Severity as Log (Severity (..))

import           System.Directory (createDirectoryIfMissing)
import           System.IO (BufferMode (LineBuffering), Handle,
                     IOMode (WriteMode), hClose, hSetBuffering, stderr, stdout)

-- | create a katip scribe for logging to a file in JSON representation
mkJsonFileScribe :: RotationParameters -> NamedSeverity -> Internal.FileDescription -> Log.Severity -> Verbosity -> IO Scribe
mkJsonFileScribe rot sevfilter fdesc s v = do
    mkFileScribe rot sevfilter fdesc formatter False s v
  where
    formatter :: (LogItem a) => Handle -> Bool -> Verbosity -> Item a -> IO Int
    formatter hdl _ v' item = do
        let tmsg = encodeToLazyText $ itemJson v' item
        TIO.hPutStrLn hdl tmsg
        return $ length tmsg

-- | create a katip scribe for logging to a file in textual representation
mkTextFileScribe :: RotationParameters -> NamedSeverity -> Internal.FileDescription -> Bool -> Log.Severity -> Verbosity -> IO Scribe
mkTextFileScribe rot sevfilter fdesc colorize s v = do
    mkFileScribe rot sevfilter fdesc formatter colorize s v
  where
    formatter :: Handle -> Bool -> Verbosity -> Item a -> IO Int
    formatter hdl colorize' v' item = do
        let tmsg = toLazyText $ formatItem colorize' v' item
        TIO.hPutStrLn hdl tmsg
        return $ length tmsg

-- | create a katip scribe for logging to a file
--   and handle file rotation within the katip-invoked logging function
mkFileScribe
    :: RotationParameters
    -> NamedSeverity
    -> Internal.FileDescription
    -> (forall a . LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO Int)      -- format and output function, returns written bytes
    -> Bool                              -- whether the output is colourized
    -> Log.Severity -> Verbosity
    -> IO Scribe
mkFileScribe rot sevfilter fdesc formatter colorize s v = do
    let prefixDir = Internal.prefixpath fdesc
    (createDirectoryIfMissing True prefixDir)
        `catchIO` (Internal.prtoutException ("cannot log prefix directory: " ++ prefixDir))
    trp <- initializeRotator rot fdesc
    scribestate <- newMVar trp    -- triple of (handle), (bytes remaining), (rotate time)
    -- sporadically remove old log files - every 10 seconds
    cleanup <- mkAutoUpdate defaultUpdateSettings { updateAction = cleanupRotator rot fdesc, updateFreq = 10000000 }
    let finalizer :: IO ()
        finalizer = do
            modifyMVar_ scribestate $ \(hdl, b, t) -> do
                hClose hdl
                return (hdl, b, t)
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item =
          whenM (checkItem s sevfilter item) $
              modifyMVar_ scribestate $ \(hdl, bytes, rottime) -> do
                  byteswritten <- formatter hdl colorize v item
                  -- remove old files
                  cleanup
                  -- detect log file rotation
                  let bytes' = bytes - (toInteger $ byteswritten)
                  let tdiff' = round $ diffUTCTime rottime (_itemTime item)
                  if bytes' < 0 || tdiff' < (0 :: Integer)
                     then do   -- log file rotation
                        hClose hdl
                        (hdl2, bytes2, rottime2) <- evalRotator rot fdesc
                        return (hdl2, bytes2, rottime2)
                     else
                        return (hdl, bytes', rottime)

    return $ Scribe logger finalizer (const $ pure True)

-- | create a katip scribe for logging to a file
mkFileScribeH :: Handle -> Bool -> NamedSeverity -> Log.Severity -> Verbosity -> IO Scribe
mkFileScribeH h colorize sevfilter s v = do
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: Item a -> IO ()
        logger item = whenM (checkItem s sevfilter item) $
            bracket_ (takeMVar locklocal) (putMVar locklocal ()) $
                TIO.hPutStrLn h $! toLazyText $ formatItem colorize v item
    pure $ Scribe logger (hClose h) (const $ pure True)

-- | create a katip scribe for logging to the console
mkStdoutScribe :: NamedSeverity -> Log.Severity -> Verbosity -> IO Scribe
mkStdoutScribe = mkFileScribeH stdout True

-- | create a katip scribe for logging to stderr
mkStderrScribe :: NamedSeverity -> Log.Severity -> Verbosity -> IO Scribe
mkStderrScribe = mkFileScribeH stderr True

-- | @Scribe@ that outputs to '/dev/null' without locking
mkDevNullScribe :: Internal.LoggingHandler -> NamedSeverity -> Log.Severity -> Verbosity -> IO Scribe
mkDevNullScribe lh sevfilter s v = do
    h <- openFile "/dev/null" WriteMode
    let colorize = False
    hSetBuffering h LineBuffering
    let logger :: Item a -> IO ()
        logger item = whenM (checkItem s sevfilter item) $
            Internal.incrementLinesLogged lh
              >> (TIO.hPutStrLn h $! toLazyText $ formatItem colorize v item)
    pure $ Scribe logger (hClose h) (const $ pure True)

-- | check if item passes severity filter
checkItem :: Monad m => Log.Severity -> NamedSeverity -> Item a -> m Bool
checkItem s sevfilter item@Item{..} =
    permitItem (Internal.sev2klog severity) item
  where
    severity :: Log.Severity
    severity = fromMaybe s $ HM.lookup namedcontext sevfilter
    namedcontext :: LoggerName
    namedcontext = mconcat $ intercalateNs _itemNamespace

-- | format a @Item@
formatItem :: Bool -> Verbosity -> Item a -> Builder
formatItem withColor _verb Item{..} =
    fromText header <>
    fromText " " <>
    brackets (fromText timestamp) <>
    fromText " " <>
    unLogStr _itemMessage
  where
    header = colorBySeverity _itemSeverity $
             "[" <> mconcat namedcontext <> ":" <> severity <> ":" <> threadid <> "]"
    namedcontext = intercalateNs _itemNamespace
    severity = renderSeverity _itemSeverity
    threadid = getThreadIdText _itemThread
    timestamp = T.pack $ formatTime defaultTimeLocale tsformat _itemTime
    tsformat :: String
    tsformat = "%F %T%2Q %Z"
    colorBySeverity s m = case s of
      EmergencyS -> red m
      AlertS     -> red m
      CriticalS  -> red m
      ErrorS     -> red m
      NoticeS    -> magenta m
      WarningS   -> yellow m
      InfoS      -> blue m
      _          -> m
    red = colorize "31"
    yellow = colorize "33"
    magenta = colorize "35"
    blue = colorize "34"
    colorize c m
      | withColor = "\ESC["<> c <> "m" <> m <> "\ESC[0m"
      | otherwise = m
