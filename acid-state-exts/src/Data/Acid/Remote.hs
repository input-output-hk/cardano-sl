{-# LANGUAGE CPP, DeriveDataTypeable, RankNTypes, RecordWildCards, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
{- |
 Module      :  Data.Acid.Remote
 Copyright   :  PublicDomain

 Maintainer  :  lemmih@gmail.com
 Portability :  non-portable (uses GHC extensions)

 This module provides the ability perform 'update' and 'query' calls
from a remote process.

On the server-side you:

 1. open your 'AcidState' normally

 2. then use 'acidServer' to share the state

On the client-side you:

 1. use 'openRemoteState' to connect to the remote state

 2. use the returned 'AcidState' like any other 'AcidState' handle

'openRemoteState' and 'acidServer' communicate over an unencrypted
socket. If you need an encrypted connection, see @acid-state-tls@.

On Unix®-like systems you can use 'UnixSocket' to create a socket file for
local communication between the client and server. Access can be
controlled by setting the permissions of the parent directory
containing the socket file.

It is also possible to perform some simple authentication using
'sharedSecretCheck' and 'sharedSecretPerform'. Keep in mind that
secrets will be sent in plain-text if you do not use
@acid-state-tls@. If you are using a 'UnixSocket' additional
authentication may not be required, so you can use
'skipAuthenticationCheck' and 'skipAuthenticationPerform'.

Working with a remote 'AcidState' is nearly identical to working with
a local 'AcidState' with a few important differences.

The connection to the remote 'AcidState' can be lost. The client will
automatically attempt to reconnect every second. Because 'query'
events do not affect the state, an aborted 'query' will be retried
automatically after the server is reconnected.

If the connection was lost during an 'update' event, the event will
not be retried. Instead 'RemoteConnectionError' will be raised. This
is because it is impossible for the client to know if the aborted
update completed on the server-side or not.

When using a local 'AcidState', an update event in one thread does not
block query events taking place in other threads. With a remote
connection, all queries and requests are channeled over a single
connection. As a result, updates and queries are performed in the
order they are executed and do block each other. In the rare case
where this is an issue, you could create one remote connection per
thread.

When working with local state, a query or update which returns the
whole state is not usually a problem due to memory sharing. The
update/query event basically just needs to return a pointer to the
data already in memory. But, when working remotely, the entire result
will be serialized and sent to the remote client. Hence, it is good
practice to create queries and updates that will only return the
required data.

This module is designed to be extenible. You can easily add your own
authentication methods by creating a suitable pair of functions and
passing them to 'acidServer' and 'openRemoteState'.

It is also possible to create alternative communication layers using
'CommChannel', 'process', and 'processRemoteState'.

-}
module Data.Acid.Remote
    (
    -- * Server/Client
      acidServer
    , acidServer'
    , openRemoteState
    -- * Authentication
    , skipAuthenticationCheck
    , skipAuthenticationPerform
    , sharedSecretCheck
    , sharedSecretPerform
    -- * Exception type
    , AcidRemoteException(..)
    -- * Low-Level functions needed to implement additional communication channels
    , CommChannel(..)
    , process
    , processRemoteState
    ) where

import Prelude                                hiding ( catch )
import Control.Concurrent.STM                        ( atomically )
import Control.Concurrent.STM.TMVar                  ( newEmptyTMVar, readTMVar, takeTMVar, tryTakeTMVar, putTMVar )
import Control.Concurrent.STM.TQueue
import Control.Exception                             ( AsyncException(ThreadKilled)
                                                     , Exception(fromException), IOException, Handler(..)
                                                     , SomeException, catch, catches, throw )
import Control.Exception                             ( throwIO, finally )
import Control.Monad                                 ( forever, liftM, join, when )
import Control.Concurrent                            ( ThreadId, forkIO, threadDelay, killThread, myThreadId )
import Control.Concurrent.MVar                       ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.Chan                       ( newChan, readChan, writeChan )
import Data.Acid.Abstract
import Data.Acid.Core
import Data.Acid.Common
import qualified Data.ByteString                     as Strict
import Data.ByteString.Char8                         ( pack )
import qualified Data.ByteString.Lazy                as Lazy
import Data.IORef                                    ( newIORef, readIORef, writeIORef )
import Data.Serialize
import Data.SafeCopy                                 ( SafeCopy, safeGet, safePut )
import Data.Set                                      ( Set, member )
import Data.Typeable                                 ( Typeable )
import GHC.IO.Exception                              ( IOErrorType(..) )
import Network                                       ( HostName, PortID(..), connectTo, listenOn, withSocketsDo )
import Network.Socket                                ( Socket, accept, sClose )
import Network.Socket.ByteString                     ( recv, sendAll )
import System.Directory                              ( removeFile )
import System.IO                                     ( Handle, hPrint, hFlush, hClose, stderr )
import System.IO.Error                               ( ioeGetErrorType, isFullError, isDoesNotExistError )

debugStrLn :: String -> IO ()
debugStrLn s =
    do -- putStrLn s -- uncomment to enable debugging
       return ()

-- | 'CommChannel' is a record containing the IO functions we need for communication between the server and client.
--
-- We abstract this out of the core processing function so that we can easily add support for SSL/TLS and Unit testing.
data CommChannel = CommChannel
    { ccPut     :: Strict.ByteString -> IO ()
    , ccGetSome :: Int -> IO (Strict.ByteString)
    , ccClose   :: IO ()
    }

data AcidRemoteException
    = RemoteConnectionError
    | AcidStateClosed
    | SerializeError String
    | AuthenticationError String
      deriving (Eq, Show, Typeable)
instance Exception AcidRemoteException

-- | create a 'CommChannel' from a 'Handle'. The 'Handle' should be
-- some two-way communication channel, such as a socket
-- connection. Passing in a 'Handle' to a normal is file is unlikely
-- to do anything useful.
handleToCommChannel :: Handle -> CommChannel
handleToCommChannel handle =
    CommChannel { ccPut     = \bs -> Strict.hPut handle bs >> hFlush handle
                , ccGetSome = Strict.hGetSome handle
                , ccClose   = hClose handle
                }

{- | create a 'CommChannel' from a 'Socket'. The 'Socket' should be
     an accepted socket, not a listen socket.
-}
socketToCommChannel :: Socket -> CommChannel
socketToCommChannel socket =
    CommChannel { ccPut     = sendAll socket
                , ccGetSome = recv    socket
                , ccClose   = sClose  socket
                }

{- | skip server-side authentication checking entirely. -}
skipAuthenticationCheck :: CommChannel -> IO Bool
skipAuthenticationCheck _ = return True

{- | skip client-side authentication entirely. -}
skipAuthenticationPerform :: CommChannel -> IO ()
skipAuthenticationPerform _ = return ()

{- | check that the client knows a shared secret.

The function takes a 'Set' of shared secrets. If a client knows any
of them, it is considered to be trusted.

The shared secret is any 'ByteString' of your choice.

If you give each client a different shared secret then you can
revoke access individually.

see also: 'sharedSecretPerform'
-}
sharedSecretCheck :: Set Strict.ByteString -- ^ set of shared secrets
                  -> (CommChannel -> IO Bool)
sharedSecretCheck secrets cc =
    do bs <- ccGetSome cc 1024
       if member bs secrets
          then do ccPut cc (pack "OK")
                  return True
          else do ccPut cc (pack "FAIL")
                  return False

-- | attempt to authenticate with the server using a shared secret.
sharedSecretPerform :: Strict.ByteString -- ^ shared secret
                    -> (CommChannel -> IO ())
sharedSecretPerform pw cc =
    do ccPut cc pw
       r <- ccGetSome cc 1024
       if r == (pack "OK")
          then return ()
          else throwIO (AuthenticationError "shared secret authentication failed.")

{- | Accept connections on @port@ and handle requests using the given 'AcidState'.
     This call doesn't return.

     On Unix®-like systems you can use 'UnixSocket' to communicate
     using a socket file. To control access, you can set the permissions of
     the parent directory which contains the socket file.

     see also: 'openRemoteState' and 'sharedSecretCheck'.
 -}
acidServer :: SafeCopy st =>
              (CommChannel -> IO Bool) -- ^ check authentication, see 'sharedSecretPerform'
           -> PortID                   -- ^ Port to listen on
           -> AcidState st             -- ^ state to serve
           -> IO ()
acidServer checkAuth port acidState
  = withSocketsDo $
    do listenSocket <- listenOn port
       (acidServer' checkAuth listenSocket acidState) `finally` (cleanup listenSocket)
    where
      cleanup socket =
          do sClose socket
             case port of
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
               UnixSocket path -> removeFile path
#endif
               _               -> return ()

{- | Works the same way as 'acidServer', but uses pre-binded socket @listenSocket@.

     Can be useful when fine-tuning of socket binding parameters is needed
     (for example, listening on a particular network interface, IPv4/IPv6 options).
 -}
acidServer' :: SafeCopy st =>
              (CommChannel -> IO Bool) -- ^ check authentication, see 'sharedSecretPerform'
           -> Socket                   -- ^ binded socket to accept connections from
           -> AcidState st             -- ^ state to serve
           -> IO ()
acidServer' checkAuth listenSocket acidState
  = do
       let loop = forever $
             do (socket, _sockAddr) <- accept listenSocket
                let commChannel = socketToCommChannel socket
                forkIO $ do authorized <- checkAuth commChannel
                            when authorized $
                                 process commChannel acidState
                            ccClose commChannel -- FIXME: `finally` ?
           infi = loop `catchSome` logError >> infi
       infi
    where
      logError :: (Show e) => e -> IO ()
      logError e = hPrint stderr e

      isResourceVanishedError :: IOException -> Bool
      isResourceVanishedError = isResourceVanishedType . ioeGetErrorType

      isResourceVanishedType :: IOErrorType -> Bool
      isResourceVanishedType ResourceVanished = True
      isResourceVanishedType _                = False

      catchSome :: IO () -> (Show e => e -> IO ()) -> IO ()
      catchSome op _h =
          op `catches` [ Handler $ \(e :: IOException)    ->
                           if isFullError e || isDoesNotExistError e || isResourceVanishedError e
                            then return () -- h (toException e) -- we could log the exception, but there could be thousands of them
                            else throw e
                       ]

data Command = RunQuery (Tagged Lazy.ByteString)
             | RunUpdate (Tagged Lazy.ByteString)
             | CreateCheckpoint
             | CreateArchive

instance Serialize Command where
  put cmd = case cmd of
              RunQuery query   -> do putWord8 0; put query
              RunUpdate update -> do putWord8 1; put update
              CreateCheckpoint ->    putWord8 2
              CreateArchive    ->    putWord8 3
  get = do tag <- getWord8
           case tag of
             0 -> liftM RunQuery get
             1 -> liftM RunUpdate get
             2 -> return CreateCheckpoint
             3 -> return CreateArchive
             _ -> error $ "Serialize.get for Command, invalid tag: " ++ show tag

data Response = Result Lazy.ByteString | Acknowledgement | ConnectionError

instance Serialize Response where
  put resp = case resp of
               Result result -> do putWord8 0; put result
               Acknowledgement -> putWord8 1
               ConnectionError -> putWord8 2
  get = do tag <- getWord8
           case tag of
             0 -> liftM Result get
             1 -> return Acknowledgement
             2 -> return ConnectionError
             _ -> error $ "Serialize.get for Response, invalid tag: " ++ show tag

{- | Server inner-loop

     This function is generally only needed if you are adding a new communication channel.
-}
process :: SafeCopy st =>
           CommChannel  -- ^ a connected, authenticated communication channel
        -> AcidState st -- ^ state to share
        -> IO ()
process CommChannel{..} acidState
  = do chan <- newChan
       forkIO $ forever $ do response <- join (readChan chan)
                             ccPut (encode response)
       worker chan (runGetPartial get Strict.empty)
  where worker chan inp
          = case inp of
              Fail msg _    -> throwIO (SerializeError msg)
              Partial cont  -> do bs <- ccGetSome 1024
                                  if Strict.null bs then
                                     return ()
                                  else
                                     worker chan (cont bs)
              Done cmd rest -> do processCommand chan cmd; worker chan (runGetPartial get rest)
        processCommand chan cmd =
          case cmd of
            RunQuery query -> do result <- queryCold acidState query
                                 writeChan chan (return $ Result result)
            RunUpdate update -> do result <- scheduleColdUpdate acidState update
                                   writeChan chan (liftM Result $ takeMVar result)
            CreateCheckpoint -> do createCheckpoint acidState
                                   writeChan chan (return Acknowledgement)
            CreateArchive -> do createArchive acidState
                                writeChan chan (return Acknowledgement)

data RemoteState st = RemoteState (Command -> IO (MVar Response)) (IO ())
                    deriving (Typeable)

{- | Connect to an acid-state server which is sharing an 'AcidState'. -}
openRemoteState :: IsAcidic st =>
                   (CommChannel -> IO ()) -- ^ authentication function, see 'sharedSecretPerform'
                -> HostName               -- ^ remote host to connect to (ignored when 'PortID' is 'UnixSocket')
                -> PortID                 -- ^ remote port to connect to
                -> IO (AcidState st)
openRemoteState performAuthorization host port
  = withSocketsDo $
    do processRemoteState reconnect
    where
      -- | reconnect
      reconnect :: IO CommChannel
      reconnect
          = (do debugStrLn "Reconnecting."
                handle <- connectTo host port
                let cc = handleToCommChannel handle
                performAuthorization cc
                debugStrLn "Reconnected."
                return cc
            )
            `catch`
            ((\_ -> threadDelay 1000000 >> reconnect) :: IOError -> IO CommChannel)


{- | Client inner-loop

     This function is generally only needed if you are adding a new communication channel.
-}
processRemoteState :: IsAcidic st =>
                      IO CommChannel -- ^ (re-)connect function
                   -> IO (AcidState st)
processRemoteState reconnect
  = do cmdQueue    <- atomically newTQueue
       ccTMV       <- atomically newEmptyTMVar
       isClosed    <- newIORef False

       let actor :: Command -> IO (MVar Response)
           actor command =
               do debugStrLn "actor: begin."
                  readIORef isClosed >>= flip when (throwIO AcidStateClosed)
                  ref <- newEmptyMVar
                  atomically $ writeTQueue cmdQueue (command, ref)
                  debugStrLn "actor: end."
                  return ref

           expireQueue listenQueue =
               do mCallback <- atomically $ tryReadTQueue listenQueue
                  case mCallback of
                    Nothing         -> return ()
                    (Just callback) ->
                        do callback ConnectionError
                           expireQueue listenQueue

           handleReconnect :: SomeException -> IO ()
           handleReconnect e
             = case fromException e of
                 (Just ThreadKilled) ->
                     do debugStrLn "handleReconnect: ThreadKilled. Not attempting to reconnect."
                        return ()
                 _ ->
                   do debugStrLn $ "handleReconnect begin."
                      tmv <- atomically $ tryTakeTMVar ccTMV
                      case tmv of
                        Nothing ->
                            do debugStrLn $ "handleReconnect: error handling already in progress."
                               debugStrLn $ "handleReconnect end."
                               return ()
                        (Just (oldCC, oldListenQueue, oldListenerTID)) ->
                            do thisTID <- myThreadId
                               when (thisTID /= oldListenerTID) (killThread oldListenerTID)
                               ccClose oldCC
                               expireQueue oldListenQueue
                               cc <- reconnect
                               listenQueue <- atomically $ newTQueue
                               listenerTID <- forkIO $ listener cc listenQueue
                               atomically $ putTMVar ccTMV (cc, listenQueue, listenerTID)
                               debugStrLn $ "handleReconnect end."
                               return ()

           listener :: CommChannel -> TQueue (Response -> IO ()) -> IO ()
           listener cc listenQueue
             = getResponse Strict.empty `catch` handleReconnect
               where
                 getResponse leftover =
                     do debugStrLn $ "listener: listening for Response."
                        let go inp = case inp of
                                   Fail msg _     -> error msg
                                   Partial cont   -> do debugStrLn $ "listener: ccGetSome"
                                                        bs <- ccGetSome cc 1024
                                                        go (cont bs)
                                   Done resp rest -> do debugStrLn $ "listener: getting callback"
                                                        callback <- atomically $ readTQueue listenQueue
                                                        debugStrLn $ "listener: passing Response to callback"
                                                        callback (resp :: Response)
                                                        return rest
                        rest <- go (runGetPartial get leftover) -- `catch` (\e -> do handleReconnect e
                                                                --                   throwIO e
                                                                 --        )
                        getResponse rest

           actorThread :: IO ()
           actorThread = forever $
             do debugStrLn "actorThread: waiting for something to do."
                (cc, cmd) <- atomically $
                  do (cmd, ref)        <- readTQueue cmdQueue
                     (cc, listenQueue, _) <- readTMVar ccTMV
                     writeTQueue listenQueue (putMVar ref)
                     return (cc, cmd)
                debugStrLn "actorThread: sending command."
                ccPut cc (encode cmd) `catch` handleReconnect
                debugStrLn "actorThread: sent."
                return ()

           shutdown :: ThreadId -> IO ()
           shutdown actorTID =
               do debugStrLn "shutdown: update isClosed IORef to True."
                  writeIORef isClosed True
                  debugStrLn "shutdown: killing actor thread."
                  killThread actorTID
                  debugStrLn "shutdown: taking ccTMV."
                  (cc, listenQueue, listenerTID) <- atomically $ takeTMVar ccTMV -- FIXME: or should this by tryTakeTMVar
                  debugStrLn "shutdown: killing listener thread."
                  killThread listenerTID
                  debugStrLn "shutdown: expiring listen queue."
                  expireQueue  listenQueue
                  debugStrLn "shutdown: closing connection."
                  ccClose cc
                  return ()

       cc <- reconnect
       listenQueue <- atomically $ newTQueue

       actorTID    <- forkIO $ actorThread
       listenerTID <- forkIO $ listener cc listenQueue

       atomically $ putTMVar ccTMV (cc, listenQueue, listenerTID)

       return (toAcidState $ RemoteState actor (shutdown actorTID))

remoteQuery :: QueryEvent event => RemoteState (EventState event) -> event -> IO (EventResult event)
remoteQuery acidState event
  = do let encoded = runPutLazy (safePut event)
       resp <- remoteQueryCold acidState (methodTag event, encoded)
       return (case runGetLazyFix safeGet resp of
                 Left msg -> error msg
                 Right result -> result)

remoteQueryCold :: RemoteState st -> Tagged Lazy.ByteString -> IO Lazy.ByteString
remoteQueryCold rs@(RemoteState fn _shutdown) event
  = do resp <- takeMVar =<< fn (RunQuery event)
       case resp of
         (Result result) -> return result
         ConnectionError -> do debugStrLn "retrying query event."
                               remoteQueryCold rs event
         Acknowledgement    -> error "remoteQueryCold got Acknowledgement. That should never happen."

scheduleRemoteUpdate :: UpdateEvent event => RemoteState (EventState event) -> event -> IO (MVar (EventResult event))
scheduleRemoteUpdate (RemoteState fn _shutdown) event
  = do let encoded = runPutLazy (safePut event)
       parsed <- newEmptyMVar
       respRef <- fn (RunUpdate (methodTag event, encoded))
       forkIO $ do Result resp <- takeMVar respRef
                   putMVar parsed (case runGetLazyFix safeGet resp of
                                      Left msg -> error msg
                                      Right result -> result)
       return parsed

scheduleRemoteColdUpdate :: RemoteState st -> Tagged Lazy.ByteString -> IO (MVar Lazy.ByteString)
scheduleRemoteColdUpdate (RemoteState fn _shutdown) event
  = do parsed <- newEmptyMVar
       respRef <- fn (RunUpdate event)
       forkIO $ do Result resp <- takeMVar respRef
                   putMVar parsed resp
       return parsed

closeRemoteState :: RemoteState st -> IO ()
closeRemoteState (RemoteState _fn shutdown) = shutdown

createRemoteCheckpoint :: RemoteState st -> IO ()
createRemoteCheckpoint (RemoteState fn _shutdown)
  = do Acknowledgement <- takeMVar =<< fn CreateCheckpoint
       return ()

createRemoteArchive :: RemoteState st -> IO ()
createRemoteArchive (RemoteState fn _shutdown)
  = do Acknowledgement <- takeMVar =<< fn CreateArchive
       return ()

toAcidState :: IsAcidic st => RemoteState st -> AcidState st
toAcidState remote
  = AcidState { _scheduleUpdate    = scheduleRemoteUpdate remote
              , scheduleColdUpdate = scheduleRemoteColdUpdate remote
              , _query             = remoteQuery remote
              , queryCold          = remoteQueryCold remote
              , createCheckpoint   = createRemoteCheckpoint remote
              , createArchive      = createRemoteArchive remote
              , closeAcidState     = closeRemoteState remote
              , acidSubState       = mkAnyState remote
              }

