{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Network.Discovery.Abstract (

      NetworkDiscovery(..)
    , DiscoveryError(..)

    ) where

import           Control.Exception (Exception)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Network.Transport (EndPointAddress)

-- Note: we'll need a way to be notified of new peers.

data NetworkDiscovery err = NetworkDiscovery {
      knownPeers     :: IO (Set EndPointAddress)
    , discoverPeers  :: IO (Either (DiscoveryError err) (Set EndPointAddress))
    -- ^ The set is the newly discovered peers (disjoint from 'knownPeers' at
    -- the time it's used). Subsequent uses of 'knownPeers' should include these
    -- new endpoints.
    , closeDiscovery :: IO ()
    }

data DiscoveryError error = DiscoveryError error String
    deriving (Show, Typeable, Generic)

instance (Typeable error, Show error) => Exception (DiscoveryError error)
