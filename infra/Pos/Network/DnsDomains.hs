module Pos.Network.DnsDomains (
    DnsDomains(..)
  , resolveDnsDomains
    -- * Re-exports
  , DNSError
  ) where

import           Universum
import           Data.IP     (IPv4)
import           Network.Broadcast.OutboundQueue.Types (Alts, AllOf)
import           Network.DNS (DNSError)
import qualified Network.DNS as DNS

-- | DNS domains for relay discovery
--
-- We provide a list of list of domain names to query. The outer list
-- corresponds to backup DNS servers; the inner lists provide multiple
-- domain names which serve different purposes (e.g., the first might be
-- configured to return geolocated hosts, with the second a load-balancing
-- fall-back). The idea is that querying each of the elements of the inner
-- lists provides a complete set of relay nodes to be tried, using the
-- backup domain names only when one or more of the primary ones fail.
data DnsDomains = DnsDomains {
      dnsDomains :: !(Alts (AllOf DNS.Domain))
    }
  deriving (Show)

-- | Resolve 'DnsDomains'
--
-- See 'DnsDomains' for an explanation of the logic. This only returns a set
-- of errors if _all_ alternatives fail; in that case, it returns the (first)
-- error that occurred for all alternatives (we don't continue trying a set of
-- domains after the first error).
resolveDnsDomains :: DnsDomains -> IO (Either [DNSError] [IPv4])
resolveDnsDomains (DnsDomains{..}) = do
    resolvSeed <- DNS.makeResolvSeed DNS.defaultResolvConf
    DNS.withResolver resolvSeed $ \resolver ->
      findAlts resolver [] dnsDomains
  where
    -- Find a set of DNS names all of which we can resolve together
    findAlts :: DNS.Resolver
             -> [DNSError]
             -> Alts (AllOf DNS.Domain)
             -> IO (Either [DNSError] [IPv4])
    findAlts _        errs []           = return $ Left (reverse errs)
    findAlts resolver errs (alts:altss) = do
      mAddrs <- tryAlts resolver alts
      case mAddrs of
        Left  err   -> findAlts resolver (err:errs) altss
        Right addrs -> return $ Right addrs

    -- Resolve a set of domains names. If they can all be resolved, return
    -- the set; if an error occurs, return Nothing.
    tryAlts :: DNS.Resolver -> AllOf DNS.Domain -> IO (Either DNSError [IPv4])
    tryAlts resolver = go []
      where
        go :: [IPv4] -> [DNS.Domain] -> IO (Either DNSError [IPv4])
        go acc []         = return $ Right (reverse acc)
        go acc (dom:doms) = do
          mAddrs <- DNS.lookupA resolver dom
          case mAddrs of
            Left  err   -> return $ Left err
            Right addrs -> go (reverse addrs ++ acc) doms
