module Pos.Ssc.GodTossing.Secret.Types
       ( GtSecret
       , GtSecretStorage (..)
       , defSecretForTip
       ) where

import           Data.Default                  (Default (..))
import           Universum

import           Pos.Crypto                    (PublicKey, unsafeHash)
import           Pos.Ssc.GodTossing.Types.Base (Opening, SignedCommitment)
import           Pos.Ssc.GodTossing.Types.Type (SscGodTossing)
import           Pos.Types                     (HeaderHash, SlotId, unflattenSlotId)

type GtSecret = (PublicKey, SignedCommitment, Opening)

defSecretForTip :: HeaderHash SscGodTossing
defSecretForTip = unsafeHash ("dratuti" :: Text)

data GtSecretStorage = GtSecretStorage
    {
      -- | Secret that we are using for the current epoch.
      _dsCurrentSecret     :: !(Maybe GtSecret)
    , -- | Last slot we are aware of.
      _dsLastProcessedSlot :: !SlotId
      -- | Tip corresponding to the latter generated secret
    , _dsSecretForTip      :: !(HeaderHash SscGodTossing)
    } deriving (Show, Eq)

instance Default GtSecretStorage where
    def =
        GtSecretStorage
        {
          _dsCurrentSecret = Nothing
        , _dsLastProcessedSlot = unflattenSlotId 0
        , _dsSecretForTip = defSecretForTip
        }
