{-# LANGUAGE RankNTypes, RecordWildCards, GADTs, ScopedTypeVariables #-}
module Lib (serv) where

import GHCi.Run
import GHCi.TH
import GHCi.Message

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary

import System.Environment (getProgName)
import Debug.Trace (traceIO)

type MessageHook = Msg -> IO Msg

trace :: String -> IO ()
trace s = getProgName >>= \name -> traceIO ("[" ++ name ++ "] " ++ s)

serv :: Bool -> MessageHook -> Pipe -> (forall a .IO a -> IO a) -> IO ()
serv verbose hook pipe@Pipe{..} restore = loop
 where
  loop = do
    when verbose $ trace ("iserv: reading pipe...")
    Msg msg <- readPipe pipe getMessage >>= hook
    discardCtrlC verbose

    when verbose $ trace ("iserv: " ++ show msg)
    case msg of
      Shutdown -> return ()
      RunTH st q ty loc -> wrapRunTH verbose $ runTH pipe st q ty loc
      RunModFinalizers st qrefs -> wrapRunTH verbose $ runModFinalizerRefs pipe st qrefs
      _other -> run msg >>= reply

  reply :: forall a. (Binary a, Show a) => a -> IO ()
  reply r = do
    when verbose $ trace ("iserv: return: " ++ show r)
    writePipe pipe (put r)
    when verbose $ trace ("iserv: wrote pipe")
    loop

  -- Run some TH code, which may interact with GHC by sending
  -- THMessage requests, and then finally send RunTHDone followed by a
  -- QResult.  For an overview of how TH works with Remote GHCi, see
  -- Note [Remote Template Haskell] in libraries/ghci/GHCi/TH.hs.
  wrapRunTH :: forall a. (Binary a, Show a) => Bool -> IO a -> IO ()
  wrapRunTH verbose io = do
    when verbose $ trace "iserv: wrapRunTH..."
    r <- try io
    writePipe pipe (putTHMessage RunTHDone)
    case r of
      Left e
        | Just (GHCiQException _ err) <- fromException e  ->
           reply (QFail err :: QResult a)
        | otherwise -> do
           str <- showException verbose e
           reply (QException str :: QResult a)
      Right a -> do
        when verbose $ trace "iserv: QDone"
        reply (QDone a)

  -- carefully when showing an exception, there might be other exceptions
  -- lurking inside it.  If so, we return the inner exception instead.
  showException :: Bool -> SomeException -> IO String
  showException verbose e0 = do
    when verbose $ trace "iserv: showException ..."
    r <- try $ evaluate (force (show (e0::SomeException)))
    case r of
       Left e -> showException verbose e
       Right str -> return str

  -- throw away any pending ^C exceptions while we're not running
  -- interpreted code.  GHC will also get the ^C, and either ignore it
  -- (if this is GHCi), or tell us to quit with a Shutdown message.
  discardCtrlC verbose = do
    when verbose $ trace ("iserv: received ctrl-c")
    r <- try $ restore $ return ()
    case r of
      Left UserInterrupt -> return () >> discardCtrlC verbose
      Left e -> throwIO e
      _ -> return ()
