{-# LANGUAGE ForeignFunctionInterface #-}

module Pos.Network.Windows.DnsDomains
       ( getWindowsDefaultDnsServer
       ) where

import           Universum

import           Foreign.C (CString, peekCString)
import           Foreign.Marshal.Utils (maybePeek)


-- | Returns @nullPtr@ on failure.
foreign import ccall "getWindowsDefDnsServer" getWindowsDefDnsServer :: IO CString

-- | Gets one of the default DNS servers the current machine is using. To be used
-- on Windows systems only. On failure (i.e. when there are no DNS servers available,
-- or when the operation is not supported by the operating system), returns @Nothing@.
--
-- >>> getWindowsDefaultDnsServer
-- Just "8.8.8.8"
-- >>> getWindowsDefaultDnsServer
-- Nothing
getWindowsDefaultDnsServer :: IO (Maybe String)
getWindowsDefaultDnsServer = getWindowsDefDnsServer >>= maybePeek peekCString
