{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Simplified NIST beacon implementation of SSC.

module Pos.Ssc.NistBeacon
       ( SscNistBeacon
       ) where

import           Control.Monad.State     (put)
import           Crypto.Hash             (SHA256)
import qualified Crypto.Hash             as Hash
import qualified Data.ByteArray          as ByteArray (convert)
import           Data.Coerce             (coerce)
import           Data.Default            (Default (def))
import           Data.Tagged             (Tagged (..))
import           Data.Text.Buildable     (Buildable (build))
import           Serokell.Util.Verify    (VerificationRes (..))
import           Universum

import           Pos.Binary.Relay        ()
import           Pos.Binary.Class        (encode)
import           Pos.Slotting            (onNewSlot)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass (..))
import           Pos.Ssc.Class.Listeners (SscListenersClass (..), sscStubListeners)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage   (SscStorageClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Class.Workers   (SscWorkersClass (..))
import           Pos.Ssc.Extra           (sscRunLocalUpdate)
import           Pos.Types               (SharedSeed (..), SlotId, unflattenSlotId)

-- | Data type tag for Nist Beacon implementation of Shared Seed Calculation.
data SscNistBeacon
    deriving (Generic)

deriving instance Show SscNistBeacon
deriving instance Eq SscNistBeacon

instance Buildable () where
    build _ = "()"

newtype LocalData = LocalData
    { getLocalData :: SlotId
    }

instance Default LocalData where
    def = LocalData $ unflattenSlotId 0

instance Ssc SscNistBeacon where
    type SscLocalData   SscNistBeacon = LocalData
    type SscPayload     SscNistBeacon = ()
    type SscProof       SscNistBeacon = ()
    type SscSeedError   SscNistBeacon = ()
    type SscGlobalState SscNistBeacon = ()
    type SscNodeContext SscNistBeacon = ()
    type SscParams      SscNistBeacon = ()

    mkSscProof = Tagged $ const ()
    sscCreateNodeContext _ = return ()

instance SscHelpersClass SscNistBeacon where
    sscVerifyPayload = Tagged $ const $ const VerSuccess

instance SscWorkersClass SscNistBeacon where
    sscWorkers = Tagged
        [ \_ -> onNewSlot True $ \s -> sscRunLocalUpdate (put $ LocalData s)
        ]
    sscLrcConsumers = Tagged []

instance SscListenersClass SscNistBeacon where
    sscListeners = Tagged []
    sscStubListeners _ = []

instance SscLocalDataClass SscNistBeacon where
    sscGetLocalPayloadQ = (,()) . getLocalData <$> ask
    sscApplyGlobalStateU _ _ = pure ()

instance SscStorageClass SscNistBeacon where
    sscLoadGlobalState _ = pure ()
    sscApplyBlocksM _ = pure ()
    sscRollbackM _ = pure ()
    sscVerifyBlocksM _ _ _ = pure mempty
    sscCalculateSeedM =
        pure . Right . coerce . ByteArray.convert @_ @ByteString .
            Hash.hashlazy @SHA256 . encode
