module Pos.Util.LogJsonScribe
       ( mkJsonScribe
       ) where

import           Universum

import           Data.Aeson.Text
import qualified Data.Text.Lazy.IO          as T
import           System.IO

import           Katip.Core


-------------------------------------------------------------------------------
  -- | create a katip scribe for JSON logging

mkJsonScribe :: Handle -> Severity -> Verbosity -> IO Scribe
mkJsonScribe h s v = do
    hSetBuffering h LineBuffering
    lock <- newMVar ()
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger i@Item{..} =
          when (permitItem s i) $
              bracket_ (takeMVar lock) (putMVar lock ()) $
                T.hPutStrLn h $ encodeToLazyText $ itemJson v i
    pure $ Scribe logger (hFlush h)

