{-# LANGUAGE ForeignFunctionInterface #-}
module FileIO(FHandle,open,write,flush,close) where
import System.Posix(Fd(Fd),
                    openFd,
                    fdWriteBuf,
                    closeFd,
                    OpenMode(WriteOnly),
                    defaultFileFlags,
                    stdFileMode
                   )
import Data.Word(Word8,Word32)
import Foreign(Ptr)
import Foreign.C(CInt(..))

data FHandle = FHandle Fd

-- should handle opening flags correctly
open :: FilePath -> IO FHandle
open filename = fmap FHandle $ openFd filename WriteOnly (Just stdFileMode) defaultFileFlags

write :: FHandle -> Ptr Word8 -> Word32 -> IO Word32
write (FHandle fd) data' length = fmap fromIntegral $ fdWriteBuf fd data' $ fromIntegral length

-- Handle error values?
flush :: FHandle -> IO ()
flush (FHandle (Fd c_fd)) = c_fsync c_fd >> return ()

foreign import ccall "fsync" c_fsync :: CInt -> IO CInt

close :: FHandle -> IO ()
close (FHandle fd) = closeFd fd
