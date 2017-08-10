{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#if defined(mingw32_HOST_OS)
#define WINDOWS
#endif

module Pos.Network.Windows.DnsDomains (
#ifdef WINDOWS
    getWindowsDefaultDnsServer
#endif
    ) where

#ifdef WINDOWS
import           Universum

import           Foreign.C            (CString, peekCString)

foreign import ccall "getWindowsDefDnsServer" getWindowsDefDnsServer :: IO CString

getWindowsDefaultDnsServer :: IO String
getWindowsDefaultDnsServer = getWindowsDefDnsServer >>= peekCString
#endif
