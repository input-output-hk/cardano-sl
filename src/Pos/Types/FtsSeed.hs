-- | Functions and other stuff related to FtsSeed type.

module Pos.Types.FtsSeed
       ( ftsSeedLength
       , genSharedFtsSeed
       ) where

import           Control.Lens    (over, _1)
import           Universum

import           Pos.Crypto      (EncShare, Secret (..), SecretSharingExtra, Threshold,
                                  VssPublicKey, genSharedSecret, runSecureRandom)
import           Pos.Types.Types (FtsSeed (..))

-- | Length of FtsSeed which is currently constant known in compile
-- time.
ftsSeedLength :: Integral a => a
ftsSeedLength = 32

-- | Generate securely random FtsSeed.
genSharedFtsSeed
    :: MonadIO m
    => Threshold -> [VssPublicKey] -> m (FtsSeed, SecretSharingExtra, [EncShare])
genSharedFtsSeed n =
    liftIO .
    runSecureRandom . fmap (over _1 (FtsSeed . getSecret)) . genSharedSecret n
