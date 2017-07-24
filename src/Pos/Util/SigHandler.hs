-- | Modular wrapper around Posix signal handlers
module Pos.Util.SigHandler (
    Signal(..)
  , installHandler
  , uninstallAllHandlers
  ) where

import           Universum
import           Control.Concurrent   (modifyMVar_, withMVar)
import           System.IO.Unsafe     (unsafePerformIO)
import qualified Data.Map.Strict      as Map
import qualified System.Posix.Signals as Posix

data Signal =
      SigABRT
    | SigALRM
    | SigBUS
    | SigCHLD
    | SigCONT
    | SigFPE
    | SigHUP
    | SigILL
    | SigINT
    | SigKILL
    | SigPIPE
    | SigQUIT
    | SigSEGV
    | SigSTOP
    | SigTERM
    | SigTSTP
    | SigTTIN
    | SigTTOU
    | SigUSR1
    | SigUSR2
    | SigPOLL
    | SigPROF
    | SigSYS
    | SigTRAP
    | SigURG
    | SigVTALRM
    | SigXCPU
    | SigXFSZ
  deriving (Show, Eq, Ord)

toPosixSignal :: Signal -> Posix.Signal
toPosixSignal SigABRT   = Posix.sigABRT
toPosixSignal SigALRM   = Posix.sigALRM
toPosixSignal SigBUS    = Posix.sigBUS
toPosixSignal SigCHLD   = Posix.sigCHLD
toPosixSignal SigCONT   = Posix.sigCONT
toPosixSignal SigFPE    = Posix.sigFPE
toPosixSignal SigHUP    = Posix.sigHUP
toPosixSignal SigILL    = Posix.sigILL
toPosixSignal SigINT    = Posix.sigINT
toPosixSignal SigKILL   = Posix.sigKILL
toPosixSignal SigPIPE   = Posix.sigPIPE
toPosixSignal SigQUIT   = Posix.sigQUIT
toPosixSignal SigSEGV   = Posix.sigSEGV
toPosixSignal SigSTOP   = Posix.sigSTOP
toPosixSignal SigTERM   = Posix.sigTERM
toPosixSignal SigTSTP   = Posix.sigTSTP
toPosixSignal SigTTIN   = Posix.sigTTIN
toPosixSignal SigTTOU   = Posix.sigTTOU
toPosixSignal SigUSR1   = Posix.sigUSR1
toPosixSignal SigUSR2   = Posix.sigUSR2
toPosixSignal SigPOLL   = Posix.sigPOLL
toPosixSignal SigPROF   = Posix.sigPROF
toPosixSignal SigSYS    = Posix.sigSYS
toPosixSignal SigTRAP   = Posix.sigTRAP
toPosixSignal SigURG    = Posix.sigURG
toPosixSignal SigVTALRM = Posix.sigVTALRM
toPosixSignal SigXCPU   = Posix.sigXCPU
toPosixSignal SigXFSZ   = Posix.sigXFSZ

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
        void $ Posix.installHandler
          (toPosixSignal signal)
          oldHandler
          Nothing
