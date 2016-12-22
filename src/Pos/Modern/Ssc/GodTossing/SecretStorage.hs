module Pos.Modern.Ssc.GodTossing.SecretStorage
       (
         GtSecret
       ) where

import           Data.Default                           (Default (..))
import           Universum

import           Pos.Crypto                             (PublicKey)
import           Pos.Ssc.GodTossing.Types.Base          (Opening, SignedCommitment)
import           Pos.Types                              (SlotId (..), unflattenSlotId)
import Pos.Modern.DB.Misc (getSecretStorage)

type GtSecret = (PublicKey, SignedCommitment, Opening)

data GtSecretStorage = GtSecretStorage
    {
      -- | Secret that we are using for the current epoch.
      _dsCurrentSecret     :: !(Maybe GtSecret)
    , -- | Last slot we are aware of.
      _dsLastProcessedSlot :: !SlotId
    }

instance Default GtSecretStorage where
    def =
        GtSecretStorage
        {
          _dsCurrentSecret = Nothing
        , _dsLastProcessedSlot = unflattenSlotId 0
        }

getSecret :: SConstraint (Maybe GtSecret)
getSecret = sscStorage >>= flip queryExtended GetS

setSecret :: GtSecret -> SConstraint ()
setSecret secret = sscStorage >>= flip updateExtended (SetS secret)

prepareSecretToNewSlot :: SlotId -> SConstraint ()
prepareSecretToNewSlot  slotId = sscStorage >>= flip updateExtended (Prepare slotId)
