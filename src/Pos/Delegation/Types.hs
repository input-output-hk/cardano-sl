-- | Delegation-related local types.

module Pos.Delegation.Types
       ( DlgPayload (..)
       , mkDlgPayload
       , ProxySKLightConfirmation
       , DlgUndo (..)
       , DlgMemPool
       , ProxySKBlockInfo
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Default         (Default (def))
import           Data.List            (groupBy)
import qualified Data.Text.Buildable
import           Formatting           (bprint, int, sformat, (%))
import           Serokell.Util        (listJson)

import           Pos.Binary.Core      ()
import           Pos.Core             (ProxySKHeavy, ProxySKLight, ProxySigLight,
                                       StakeholderId)
import           Pos.Crypto           (ProxySecretKey (..), PublicKey, verifyPsk)

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

-- | Constructor of 'DlgPaylod' which ensures absence of duplicates.
mkDlgPayload :: MonadError Text m => [ProxySKHeavy] -> m DlgPayload
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

-- | Undo for the delegation component.
data DlgUndo = DlgUndo
    { duPsks            :: [ProxySKHeavy]
      -- ^ PSKs we've modified when applying the block (by deleting or
      -- overwriting).
    , duPrevEpochPosted :: HashSet StakeholderId
      -- ^ Set of stakeholders that posted in epoch i. This field
      -- should be present only for genesis block of epoch i+1.
    } deriving (Generic)

instance NFData DlgUndo

instance Buildable DlgUndo where
    build DlgUndo{..} =
        bprint ("DlgUndo:"%
                "\n  duPsks: "%listJson%
                "\n  duPrevEpochPosted: "%listJson)
               duPsks duPrevEpochPosted

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | Confirmation of light cert type.
type ProxySKLightConfirmation = (ProxySKLight, ProxySigLight ProxySKLight)

-- | Lightweight PSK or heavyweight PSK with real leader public key
-- (because heavyweight psks have redelegation feature, so pskIssuerPk
-- hPsk /= leader in general case). This is used to create a block
-- header only.
type ProxySKBlockInfo = Maybe (Either ProxySKLight (ProxySKHeavy, PublicKey))
