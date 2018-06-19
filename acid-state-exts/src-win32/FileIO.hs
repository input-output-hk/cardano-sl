module FileIO(FHandle,open,write,flush,close) where
import System.Win32(HANDLE,
                    createFile,
                    gENERIC_WRITE,
                    fILE_SHARE_NONE,
                    cREATE_ALWAYS,
                    fILE_ATTRIBUTE_NORMAL,
                    win32_WriteFile,
                    flushFileBuffers,
                    closeHandle)
import Data.Word(Word8,Word32)
import Foreign(Ptr)
import System.IO
import System.Directory(createDirectoryIfMissing,removeFile)
import Control.Exception.Extensible(try,throw)
import Control.Exception(SomeException,IOException)
import qualified Control.Exception as E

data FHandle = FHandle HANDLE

tryE :: IO a -> IO (Either SomeException a)
tryE = try

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = E.catch

open :: FilePath -> IO FHandle
open filename =
    fmap FHandle $ createFile filename gENERIC_WRITE fILE_SHARE_NONE Nothing cREATE_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing

write :: FHandle -> Ptr Word8 -> Word32 -> IO Word32
write (FHandle handle) data' length = win32_WriteFile handle data' length Nothing

flush :: FHandle -> IO ()
flush (FHandle handle) = flushFileBuffers handle

close :: FHandle -> IO ()
close (FHandle handle) = closeHandle handle

-- Windows opens files for exclusive writing by default
openExclusively :: FilePath -> IO Handle
openExclusively fp = openFile fp ReadWriteMode
