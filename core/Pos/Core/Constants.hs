{-# LANGUAGE CPP #-}

-- | Non-configurable constants
--   For configurable constants, see Pos.Core.Configuration.

module Pos.Core.Constants
       ( sharedSeedLength
       , genesisDataDigest
       , isDevelopment
       ) where

import           Crypto.Hash            (Blake2b_256, Digest, digestFromByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Universum

----------------------------------------------------------------------------
-- Constants which are not configurable
----------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32

-- | Digest of the canonical JSON of the mainnet 'GenesisData'
--
-- FIXME avieth
-- This is the blake2b_256 digest of an empty file (touch <file name>).
-- Put in the right value once we know it (digest of actual canonical JSON
-- of genesis data.
--
-- It is also the predefined 'Hash' of the parent of the 0-th genesis block
-- (which is the only block without a real parent). But that's coincidental
-- rather than essential.
-- It doesn't have to be the digest of the canonical JSON; it could be any
-- Blake2b_256 digest so long as it's determined by the 'GenesisData'. This is
-- just a very convenient choice because it means we don't have to parse a
-- digest from the 'GensisData' canonical JSON.
genesisDataDigest :: Digest Blake2b_256
Just genesisDataDigest = digestFromByteString @_ @BS.ByteString bytesDigest
  where
    hexDigest = "bcfbfcad6cf78f2363568e76af9d6e927f71c3683aafe5e289796579792dccdb"
    (bytesDigest, _) = B16.decode hexDigest

{-# INLINE genesisDataDigest #-}

-- | @True@ if current mode is 'Development'.
--
-- FIXME put it in Pos.Core.Configuration and don't use CPP.
isDevelopment :: Bool
#ifdef DEV_MODE
isDevelopment = True
#else
isDevelopment = False
#endif
