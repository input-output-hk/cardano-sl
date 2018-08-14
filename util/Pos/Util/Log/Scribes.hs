-- | provide backends for `katip`
module Pos.Util.Log.Scribes
    ( mkStdoutScribe
    , mkStderrScribe
    , mkDevNullScribe
    , mkFileScribe
    , mkJsonFileScribe
    ) where

import           Universum hiding (fromString)

import           Control.Exception.Safe (Exception (..), catchIO)

import           Data.Aeson.Text (encodeToLazyText)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as T
import           Katip.Core
import           Katip.Format.Time (formatAsIso8601)
import           Katip.Scribes.Handle (brackets, getKeys)
import qualified Pos.Util.Log.Internal as Internal
import           System.FilePath ((</>))
import           System.IO (BufferMode (LineBuffering), Handle,
                     IOMode (WriteMode), hFlush, hSetBuffering, stderr, stdout)
import           System.IO.Unsafe (unsafePerformIO)

-- | global lock for file Scribes
{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

prtoutException :: Exception e => FilePath -> e -> IO ()
prtoutException fp e = do
    putStrLn $ "error while opening log @ " ++ fp
    putStrLn $ "exception: " ++ displayException e

-- | create a katip scribe for logging to a file (JSON)
mkJsonFileScribe :: FilePath -> FilePath -> Severity -> Verbosity -> IO Scribe
mkJsonFileScribe bp fp s v = do
    h <- catchIO (openFile (bp </> fp) WriteMode) $
            \e -> do
                prtoutException (bp </> fp) e
                return stdout    -- fallback to standard output in case of exception
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item =
          when (permitItem s item) $
              bracket_ (takeMVar locklocal) (putMVar locklocal ()) $
                T.hPutStrLn h $ encodeToLazyText $ itemJson v item
    pure $ Scribe logger (hFlush h)

-- | create a katip scribe for logging to a file
mkFileScribe :: FilePath -> FilePath -> Bool -> Severity -> Verbosity -> IO Scribe
mkFileScribe bp fp colorize s v = do
    h <- catchIO (openFile (bp </> fp) WriteMode) $
            \e -> do
                prtoutException (bp </> fp) e
                return stdout    -- fallback to standard output in case of exception
    mkFileScribeH h colorize s v

-- | internal: return scribe on file handle
-- thread safe by MVar
-- formatting done with 'formatItem'
mkFileScribeH :: Handle -> Bool -> Severity -> Verbosity -> IO Scribe
mkFileScribeH h colorize s v = do
    hSetBuffering h LineBuffering
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item = when (permitItem s item) $
            bracket_ (takeMVar lock) (putMVar lock ()) $
                T.hPutStrLn h $! toLazyText $ formatItem colorize v item
    pure $ Scribe logger (hFlush h)

-- | create a katip scribe for logging to the console
mkStdoutScribe :: Severity -> Verbosity -> IO Scribe
mkStdoutScribe = mkFileScribeH stdout True

-- | create a katip scribe for logging to stderr
mkStderrScribe :: Severity -> Verbosity -> IO Scribe
mkStderrScribe = mkFileScribeH stderr True

-- | @Scribe@ that outputs to '/dev/null' without locking
mkDevNullScribe :: Internal.LoggingHandler -> Severity -> Verbosity -> IO Scribe
mkDevNullScribe lh s v = do
    h <- openFile "/dev/null" WriteMode
    let colorize = False
    hSetBuffering h LineBuffering
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item = when (permitItem s item) $
            Internal.incrementLinesLogged lh
              >> (T.hPutStrLn h $! toLazyText $ formatItem colorize v item)
    pure $ Scribe logger (hFlush h)


-- | format a @LogItem@ with subsecond precision (ISO 8601)
formatItem :: LogItem a => Bool -> Verbosity -> Item a -> Builder
formatItem withColor verb Item{..} =
    brackets nowStr <>
    brackets (mconcat $ map fromText $ intercalateNs _itemNamespace) <>
    brackets (fromText (renderSeverity' _itemSeverity)) <>
    brackets (fromString _itemHost) <>
    brackets (fromString (show _itemProcess)) <>
    brackets (fromText (getThreadIdText _itemThread)) <>
    mconcat ks <>
    maybe mempty (brackets . fromString . locationToString) _itemLoc <>
    fromText " " <>
    unLogStr _itemMessage
  where
    nowStr = fromText (formatAsIso8601 _itemTime)
    ks = map brackets $ getKeys verb _itemPayload
    renderSeverity' s = case s of
      EmergencyS -> red $ renderSeverity s
      AlertS     -> red $ renderSeverity s
      CriticalS  -> red $ renderSeverity s
      ErrorS     -> red $ renderSeverity s
      NoticeS    -> magenta $ renderSeverity s
      WarningS   -> yellow $ renderSeverity s
      InfoS      -> blue $ renderSeverity s
      _          -> renderSeverity s
    red = colorize "31"
    yellow = colorize "33"
    magenta = colorize "35"
    blue = colorize "34"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s
