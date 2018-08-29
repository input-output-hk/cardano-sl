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

import           Universum hiding (fromString)

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent.MVar (modifyMVar_)

import           Data.Aeson.Text (encodeToLazyText)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time (diffUTCTime)
import           Katip.Core
import           Katip.Format.Time (formatAsIso8601)
import           Katip.Scribes.Handle (brackets, getKeys)

import qualified Pos.Util.Log.Internal as Internal
import           Pos.Util.Log.LoggerConfig (RotationParameters (..))
import           Pos.Util.Log.Rotator (cleanupRotator, evalRotator,
                     initializeRotator)

import           System.IO (BufferMode (LineBuffering), Handle,
                     IOMode (WriteMode), hClose, hSetBuffering, stderr, stdout)

-- | create a katip scribe for logging to a file in JSON representation
mkJsonFileScribe :: RotationParameters -> Internal.FileDescription -> Severity -> Verbosity -> IO Scribe
mkJsonFileScribe rot fdesc s v = do
    mkFileScribe rot fdesc formatter False s v
  where
    formatter :: LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO Int
    formatter hdl _ v' item = do
        let tmsg = encodeToLazyText $ itemJson v' item
        TIO.hPutStrLn hdl tmsg
        return $ length tmsg

-- | create a katip scribe for logging to a file in textual representation
mkTextFileScribe :: RotationParameters -> Internal.FileDescription -> Bool -> Severity -> Verbosity -> IO Scribe
mkTextFileScribe rot fdesc colorize s v = do
    mkFileScribe rot fdesc formatter colorize s v
  where
    formatter :: LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO Int
    formatter hdl colorize' v' item = do
        let tmsg = toLazyText $ formatItem colorize' v' item
        TIO.hPutStrLn hdl tmsg
        return $ length tmsg

-- | create a katip scribe for logging to a file
--   and handle file rotation within the katip-invoked logging function
mkFileScribe
    :: RotationParameters
    -> Internal.FileDescription
    -> (forall a . LogItem a => Handle -> Bool -> Verbosity -> Item a -> IO Int)      -- format and output function, returns written bytes
    -> Bool                              -- whether the output is colourized
    -> Severity -> Verbosity
    -> IO Scribe
mkFileScribe rot fdesc formatter colorize s v = do
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
          when (permitItem s item) $
              modifyMVar_ scribestate $ \(hdl, bytes, rottime) -> do
                  byteswritten <- formatter hdl colorize v item
                  -- remove old files
                  cleanup
                  -- detect log file rotation
                  let bytes' = bytes - (toInteger $ byteswritten)
                  let tdiff' = round $ diffUTCTime rottime (_itemTime item)
                  if bytes' < 0 || tdiff' < (0 :: Integer)
                     then do   -- log file rotation
                        putStrLn $ "rotate! bytes=" ++ (show bytes') ++ " tdiff=" ++ (show tdiff')
                        hClose hdl
                        (hdl2, bytes2, rottime2) <- evalRotator rot fdesc
                        return (hdl2, bytes2, rottime2)
                     else
                        return (hdl, bytes', rottime)

    return $ Scribe logger finalizer

-- | create a katip scribe for logging to a file
mkFileScribeH :: Handle -> Bool -> Severity -> Verbosity -> IO Scribe
mkFileScribeH h colorize s v = do
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item = when (permitItem s item) $
            bracket_ (takeMVar locklocal) (putMVar locklocal ()) $
                TIO.hPutStrLn h $! toLazyText $ formatItem colorize v item
    pure $ Scribe logger (hClose h)

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
              >> (TIO.hPutStrLn h $! toLazyText $ formatItem colorize v item)
    pure $ Scribe logger (hClose h)


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
