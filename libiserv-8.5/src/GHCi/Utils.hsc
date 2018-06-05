{-# LANGUAGE CPP #-}
module GHCi.Utils
    ( getGhcHandle
    ) where

import Foreign.C
import GHC.IO.Handle (Handle())
#ifdef mingw32_HOST_OS
import GHC.IO.Handle.FD (fdToHandle)
#else
import System.Posix
#endif

#include <fcntl.h>     /* for _O_BINARY */

-- | Gets a GHC Handle File description from the given OS Handle or POSIX fd.
getGhcHandle :: CInt -> IO Handle
#ifdef mingw32_HOST_OS
getGhcHandle handle = _open_osfhandle handle (#const _O_BINARY) >>= fdToHandle

foreign import ccall "io.h _open_osfhandle" _open_osfhandle ::
    CInt -> CInt -> IO CInt
#else
getGhcHandle fd     = fdToHandle $ Fd fd
#endif
