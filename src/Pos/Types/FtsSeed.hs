-- | Functions and other stuff related to FtsSeed type.

module Pos.Types.FtsSeed
       ( ftsSeedLength
       , genFtsSeed
       , shareFtsSeed
       ) where

import           Universum

import           Pos.Crypto      (EncShare, Secret (..), SecretProof, VssPublicKey,
                                  secureRandomBS, shareSecret)
import           Pos.Types.Types (FtsSeed (..))

-- | Length of FtsSeed which is currently constant known in compile
-- time.
ftsSeedLength :: Integral a => a
ftsSeedLength = 40

-- | Generate securely random FtsSeed.
genFtsSeed :: MonadIO m => m FtsSeed
genFtsSeed = FtsSeed <$> secureRandomBS ftsSeedLength

shareFtsSeed :: [VssPublicKey] -> Word -> FtsSeed -> (SecretProof, [EncShare])
shareFtsSeed keys n = shareSecret keys n . Secret . getFtsSeed
