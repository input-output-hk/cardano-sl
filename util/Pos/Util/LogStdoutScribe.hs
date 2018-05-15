module Pos.Util.LogStdoutScribe
       ( mkStdoutScribe
       ) where

import           Universum hiding (fromString)

import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO          as T
import           System.IO

import           Katip.Core
import           Katip.Scribes.Handle (getKeys, brackets)
import           Katip.Format.Time (formatAsIso8601)


-------------------------------------------------------------------------------
-- | create a katip scribe for stdout logging
--   (following default scribe in katip source code)

mkStdoutScribe :: Severity -> Verbosity -> IO Scribe
mkStdoutScribe s v = do
    let h = stdout
        colorize = True
    hSetBuffering h LineBuffering
    lock <- newMVar ()
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger i@Item{..} = do
          when (permitItem s i) $ bracket_ (takeMVar lock) (putMVar lock ()) $
            T.hPutStrLn h $ toLazyText $ formatItem colorize v i
    pure $ Scribe logger (hFlush h)

-- | format a log item with subsecond precision
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
    fromText " " <> (unLogStr _itemMessage)
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




