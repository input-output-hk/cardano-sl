{-# LANGUAGE ForeignFunctionInterface #-}

module Pos.Network.Windows.DnsDomains (
    getWindowsDefaultDnsServer
    ) where

import           Universum

import           Foreign.C            (CString, peekCString)

foreign import ccall "getWindowsDefDnsServer" getWindowsDefDnsServer :: IO CString

-- | Gets one of the default DNS servers the current machine is using. To be used
-- on Windows systems only. On failure (i.e. when there are no DNS servers available,
-- or when the operation is not supported by the operating system), returns @""@.
--
-- >>> getWindowsDefaultDnsServer
-- "8.8.8.8"
-- >>> getWindowsDefaultDnsServer
-- ""
getWindowsDefaultDnsServer :: IO String
getWindowsDefaultDnsServer = getWindowsDefDnsServer >>= peekCString
