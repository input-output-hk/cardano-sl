{-# LANGUAGE ForeignFunctionInterface, GADTs, LambdaCase #-}
module Remote.Slave where

import Network.Socket

import Lib (serv)
import Remote.Message

import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when, forever)
import System.Directory
import System.FilePath (takeDirectory, (</>), dropTrailingPathSeparator,
                        isAbsolute, joinPath, splitPath)
import GHCi.ResolvedBCO

import Data.IORef
import GHCi.Message (Pipe(..), Msg(..), Message(..), readPipe, writePipe)

import Foreign.C.String

import Data.Binary
import GHC.Fingerprint (getFileHash)

import qualified Data.ByteString as BS

import System.Environment (getProgName)
import Debug.Trace (traceIO)

trace :: String -> IO ()
trace s = getProgName >>= \name -> traceIO ("[" ++ name ++ "] " ++ s)

dropLeadingPathSeparator :: FilePath -> FilePath
dropLeadingPathSeparator p | isAbsolute p = joinPath (drop 1 (splitPath p))
                           | otherwise    = p

-- | Path concatenation that prevents a double path separator to appear in the
-- final path. "/foo/bar/" <//> "/baz/quux" == "/foo/bar/baz/quux"
(<//>) :: FilePath -> FilePath -> FilePath
lhs <//> rhs = dropTrailingPathSeparator lhs </> dropLeadingPathSeparator rhs
infixr 5 <//>

foreign export ccall startSlave :: Bool -> Int -> CString -> IO ()

-- | @startSlave@ is the exported slave function, that the
-- hosting application on the target needs to invoce to
-- start the slave process, and runs iserv.
startSlave :: Bool -> Int -> CString -> IO ()
startSlave verbose port s = do
  base_path <- peekCString s
  trace $ "DocRoot: " ++ base_path
  _ <- forkIO $ startSlave' verbose base_path (toEnum port)
  return ()

-- | @startSlave'@ provdes a blocking haskell interface, that
-- the hosting application on the target can use to start the
-- slave process.
startSlave' :: Bool -> String -> PortNumber -> IO ()
startSlave' verbose base_path port = do

  sock <- openSocket port

  forever $ do
    when verbose $ trace "Opening socket"
    pipe <- acceptSocket sock >>= socketToPipe
    trace $ "Listening on port " ++ show port
    when verbose $ trace "Starting serv"
    uninterruptibleMask $ serv verbose (hook verbose base_path pipe) pipe
    when verbose $ trace "serv ended"
    return ()

-- | The iserv library may need access to files, specifically
-- archives and object files to be linked. If ghc and the slave
-- are on the same host, this is trivial, as the underlying
-- filestorage is the same.  If however the slave does not run
-- on the same host, the filestorage is not identical and we
-- need to request data from the host where ghc runs on.
--
-- If we however already have the requested file we need to make
-- sure that this file is the same one ghc sees. Hence we
-- calculate the Fingerprint of the file and send it back to the
-- host for comparison. The proxy will then send back either @Nothing@
-- indicating that the file on the host has the same Fingerprint, or
-- Maybe ByteString containing the payload to replace the existing
-- file with.
handleLoad :: Pipe -> FilePath -> FilePath -> IO ()
handleLoad pipe path localPath = do
  exists <- doesFileExist localPath
  if exists
    then getFileHash localPath >>= \hash -> proxyCall (Have path hash) >>= \case
      Nothing -> return ()
      Just bs -> BS.writeFile localPath bs
    else do
      createDirectoryIfMissing True (takeDirectory localPath)
      resp <- proxyCall (Missing path)
      BS.writeFile localPath resp

  proxyCall Done
  where
    proxyCall :: (Binary a, Show a) => SlaveMessage a -> IO a
    proxyCall msg = do
      writePipe pipe (putSlaveMessage msg)
      readPipe pipe get

-- | The hook we install in the @serv@ function from the
-- iserv library, to request archives over the wire.
hook :: Bool -> String -> Pipe -> Msg -> IO Msg
hook verbose base_path pipe m = case m of
  Msg (AddLibrarySearchPath p) -> do
    when verbose $ trace ("Need Path: " ++ (base_path <//> p))
    createDirectoryIfMissing True (base_path <//> p)
    return $ Msg (AddLibrarySearchPath (base_path <//> p))
  Msg (LoadObj path) -> do
    when verbose $ trace ("Need Obj: " ++ (base_path <//> path))
    handleLoad pipe path (base_path <//> path)
    return $ Msg (LoadObj (base_path <//> path))
  Msg (LoadArchive path) -> do
    when verbose $ trace ("Need Archieve: " ++ path ++ " at " ++ (base_path <//> path))
    handleLoad pipe path (base_path <//> path)
    return $ Msg (LoadArchive (base_path <//> path))
  -- when loading DLLs (.so, .dylib, .dll, ...) and these are provided
  -- as relative paths, the intention is to load a pre-existing system library,
  -- therefore we hook the LoadDLL call only for absolute paths to ship the
  -- dll from the host to the target.
  Msg (LoadDLL path@('C':':':_)) -> do
    when verbose $ trace ("Need DLL: " ++ path)
    return $ Msg (LoadDLL path)
  Msg (LoadDLL path) | isAbsolute path -> do
    when verbose $ trace ("Need DLL: " ++ path ++ " at " ++ (base_path <//> path))
    handleLoad pipe path (base_path <//> path)
    return $ Msg (LoadDLL (base_path <//> path))
  _other -> return m

--------------------------------------------------------------------------------
-- socket to pipe briding logic.
socketToPipe :: Socket -> IO Pipe
socketToPipe sock = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  lo_ref <- newIORef Nothing
  pure Pipe{ pipeRead = hdl, pipeWrite = hdl, pipeLeftovers = lo_ref }

openSocket :: PortNumber -> IO Socket
openSocket port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 1
  return sock

acceptSocket :: Socket -> IO Socket
acceptSocket = fmap fst . accept
