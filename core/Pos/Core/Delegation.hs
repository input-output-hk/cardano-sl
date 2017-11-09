-- | Delegation payload types.

module Pos.Core.Delegation
       ( DlgPayload (..)
       , mkDlgPayload
       ) where

import           Universum

import           Control.Monad.Except   (MonadError (throwError))
import           Data.Default           (Default (def))
import           Data.List              (groupBy)
import qualified Data.Text.Buildable
import           Formatting             (bprint, int, sformat, (%))
import           Serokell.Util          (listJson)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Types         (ProxySKHeavy)
import           Pos.Crypto             (ProxySecretKey (..), verifyPsk)

-- Consider making this a set.
-- | 'DlgPayload' is put into 'MainBlock' and consists of a list of
-- heavyweight proxy signing keys. There must be no duplicates
-- (comparing by issuer) in this list. The order of PSKs doesn't
-- matter, as it's checked for cycles after bulk application. So it's
-- technically a set.
newtype DlgPayload = UnsafeDlgPayload
    { getDlgPayload :: [ProxySKHeavy]
    } deriving (Show, Eq, Generic, NFData)

instance Default DlgPayload where
    def = UnsafeDlgPayload []

instance Buildable DlgPayload where
    build (UnsafeDlgPayload psks) =
        bprint
            ("proxy signing keys ("%int%" items): "%listJson%"\n")
            (length psks) psks

-- | Constructor of 'DlgPayload' which ensures absence of duplicates.
mkDlgPayload :: (HasConfiguration, MonadError Text m) => [ProxySKHeavy] -> m DlgPayload
mkDlgPayload proxySKs = do
    unless (null duplicates) $
        throwError "Some of block's PSKs have the same issuer, which is prohibited"
    unless (null wrongPSKs) $ throwError $
        sformat ("At least some PSKs in the block are corrupted/broken: "%listJson)
                (take 5 wrongPSKs)

    return $ UnsafeDlgPayload proxySKs
  where
    proxySKsDups psks =
        filter (\x -> length x > 1) $
        groupBy ((==) `on` pskIssuerPk) $ sortOn pskIssuerPk psks
    duplicates = proxySKsDups proxySKs
    wrongPSKs = filter (not . verifyPsk) proxySKs

