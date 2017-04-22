{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Simplified NIST beacon implementation of SSC.

module Pos.Ssc.NistBeacon
       ( SscNistBeacon
       ) where

import           Crypto.Hash             (SHA256)
import qualified Crypto.Hash             as Hash
import qualified Data.ByteArray          as ByteArray (convert)
import           Data.Coerce             (coerce)
import           Data.Tagged             (Tagged (..))
import           Data.Text.Buildable     (Buildable (build))
import           Universum

import           Pos.Binary.Class        (encode)
import           Pos.Binary.Relay        ()
import           Pos.Ssc.Class.Helpers   (SscHelpersClass (..))
import           Pos.Ssc.Class.Listeners (SscListenersClass (..), sscStubListeners)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage   (SscGStateClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Class.Workers   (SscWorkersClass (..))
import           Pos.Types               (SharedSeed (..))

-- | Data type tag for Nist Beacon implementation of Shared Seed Calculation.
data SscNistBeacon
    deriving (Generic)

deriving instance Show SscNistBeacon
deriving instance Eq SscNistBeacon

instance Buildable () where
    build _ = "()"

instance Ssc SscNistBeacon where
    type SscLocalData   SscNistBeacon = ()
    type SscPayload     SscNistBeacon = ()
    type SscProof       SscNistBeacon = ()
    type SscSeedError   SscNistBeacon = ()
    type SscGlobalState SscNistBeacon = ()
    type SscNodeContext SscNistBeacon = ()
    type SscParams      SscNistBeacon = ()
    type SscVerifyError SscNistBeacon = ()

    mkSscProof = Tagged $ const ()
    sscCreateNodeContext = Tagged $ const (return ())

instance SscHelpersClass SscNistBeacon where
    sscVerifyPayload = Tagged $ const $ const $ Right ()

instance SscWorkersClass SscNistBeacon where
    sscWorkers = const (Tagged ([], mempty))
    sscLrcConsumers = Tagged []

instance SscListenersClass SscNistBeacon where
    sscListeners = return $ Tagged ([], mempty)
    sscStubListeners = Tagged ([], mempty)

instance SscLocalDataClass SscNistBeacon where
    sscGetLocalPayloadQ _ = pure ()
    sscNormalizeU _ _ _ = pass
    sscNewLocalData = pure ()

instance SscGStateClass SscNistBeacon where
    sscLoadGlobalState = pure ()
    sscRollbackU _ = pure ()
    sscVerifyAndApplyBlocks _ _ = pass
    sscCalculateSeedQ =
        pure . Right . coerce . ByteArray.convert @_ @ByteString .
            Hash.hashlazy @SHA256 . encode
