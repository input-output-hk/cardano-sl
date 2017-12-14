-- | Modular wrapper around Posix signal handlers
--
-- This module is only included in the .cabal file when we are not on Windows.
module Pos.Util.SigHandler
       ( Signal(..)
       , installHandler
       , uninstallAllHandlers
       ) where

import           Universum

import           Control.Concurrent (modifyMVar_, withMVar)
import qualified Data.Map.Strict as Map
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Posix.Signals as Posix

{-------------------------------------------------------------------------------
  Enumeratate signals

  (The unix package doesn't use an enumeration but just uses numbers.)
  We don't list all supported signals because these are not available on
  all platforms; instead, we just introduce them when we need them.
-------------------------------------------------------------------------------}

-- | POSIX signal
data Signal = SigHUP deriving (Show, Eq, Ord)

toPosixSignal :: Signal -> Posix.Signal
toPosixSignal SigHUP = Posix.sigHUP

{-------------------------------------------------------------------------------
  Internal but global state
-------------------------------------------------------------------------------}

-- | The old Posix handlers (before we installed our own)
regOldHandlers :: MVar (Map Signal Posix.Handler)
{-# NOINLINE regOldHandlers #-}
regOldHandlers = unsafePerformIO $ newMVar Map.empty

-- | All globally installed handlers (the ones we call from our Posix handler)
regAllHandlers :: MVar (Map Signal [IO ()])
{-# NOINLINE regAllHandlers #-}
regAllHandlers = unsafePerformIO $ newMVar Map.empty

-- | The actual Posix handler that gets installed and calls all other handlers
delegationHandler :: Signal -> IO ()
delegationHandler signal =
    sequence_ =<< Map.findWithDefault [] signal <$> readMVar regAllHandlers

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Install handler for given signal
--
-- When multiple handlers are registered they will be called in order of
-- invocation.
installHandler :: Signal -> IO () -> IO ()
installHandler signal handler = do
    -- Install Posix handler if we need to
    modifyMVar_ regOldHandlers $ \oldHandlers ->
      if Map.member signal oldHandlers then
        -- already installed, nothing to do
        return oldHandlers
      else do
        oldHandler <- Posix.installHandler
          (toPosixSignal signal)
          (Posix.Catch $ delegationHandler signal)
          Nothing
        return $ Map.insert signal oldHandler oldHandlers

    -- Register the new handler
    modifyMVar_ regAllHandlers $ return . Map.alter aux signal
  where
    aux :: Maybe [IO ()] -> Maybe [IO ()]
    aux Nothing         = Just [handler]
    aux (Just handlers) = Just (handlers ++ [handler])

-- | Restore all the old Posix handlers
--
-- Any registered handlers will simply not be called anymore
uninstallAllHandlers :: IO ()
uninstallAllHandlers =
    withMVar regOldHandlers $ \oldHandlers ->
        forM_ (Map.toList oldHandlers) $ \(signal, oldHandler) ->
            void $
            Posix.installHandler (toPosixSignal signal) oldHandler Nothing
