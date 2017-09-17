{-# LANGUAGE CPP #-}

-- | Non-configurable constants
--   For configurable constants, see Pos.Core.Configuration.

module Pos.Core.Constants
    ( sharedSeedLength
    , genesisDataDigest
    , isDevelopment
    ) where

import           Crypto.Hash
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
    hexDigest = "1b570b6e6dd992889ba1e67a4bd802df19edb9e9a51fb0772ca2016bff979068"
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
