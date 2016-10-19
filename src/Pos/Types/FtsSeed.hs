-- | Functions and other stuff related to FtsSeed type.

module Pos.Types.FtsSeed
       ( ftsSeedLength
       , genSharedFtsSeed
       , secretToFtsSeed
       ) where

import           Control.Lens    (over, _1)
import qualified Data.Binary     as Bin
import           Universum

import           Pos.Crypto      (EncShare, Secret, SecretSharingExtra, Threshold,
                                  VssPublicKey, genSharedSecret, runSecureRandom)
import           Pos.Types.Types (FtsSeed (..))

-- | Length of FtsSeed which is currently constant known in compile
-- time.
ftsSeedLength :: Integral a => a
ftsSeedLength = 40

-- | Convert Secret to FtsSeed.
-- FIXME: result is 33 bytes long!
secretToFtsSeed :: Secret -> FtsSeed
secretToFtsSeed = FtsSeed . toS . Bin.encode

-- | Generate securely random FtsSeed.
genSharedFtsSeed
    :: MonadIO m
    => Threshold -> [VssPublicKey] -> m (FtsSeed, SecretSharingExtra, [EncShare])
genSharedFtsSeed n =
    liftIO .
    runSecureRandom . fmap (over _1 secretToFtsSeed) . genSharedSecret n
