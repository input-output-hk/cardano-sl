{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Simplified NIST beacon implementation of SSC.

module Pos.Ssc.NistBeacon
       ( SscNistBeacon
       ) where

import           Crypto.Hash             (SHA256)
import qualified Crypto.Hash             as Hash
import qualified Data.ByteArray          as ByteArray (convert)
import           Data.Tagged             (Tagged (..))
import           Data.Text.Buildable     (Buildable (build))
import           Universum

import           Pos.Binary.Class        (encodeLazy)
import           Pos.Core                (SharedSeed (..))
import           Pos.Ssc.Class.Helpers   (SscHelpersClass (..))
import           Pos.Ssc.Class.Listeners (SscListenersClass (..))
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage   (SscGStateClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Class.Workers   (SscWorkersClass (..))

-- | Data type tag for Nist Beacon implementation of Shared Seed Calculation.
data SscNistBeacon
    deriving (Generic)

deriving instance Show SscNistBeacon
deriving instance Eq SscNistBeacon

-- FIXME this is needed because 'Ssc' puts the Buildable constraint on
-- things. However, this instance should really be in Pos.Util.Util instead.
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
    sscCreateNodeContext = Tagged $ const (pure ())

instance SscHelpersClass SscNistBeacon where
    sscVerifyPayload = const $ const $ Right ()
    sscStripPayload _ () = Just ()
    sscDefaultPayload _ = ()

instance SscWorkersClass SscNistBeacon where
    sscWorkers = ([], mempty)
    sscLrcConsumers = []

instance SscListenersClass SscNistBeacon where
    sscRelays = Tagged []

instance SscLocalDataClass SscNistBeacon where
    sscGetLocalPayloadQ _ = pure ()
    sscNormalizeU _ _ _ = pass
    sscNewLocalData = pure ()

instance SscGStateClass SscNistBeacon where
    sscLoadGlobalState = pass
    sscGlobalStateToBatch _ = Tagged []
    sscRollbackU _ = pass
    sscVerifyAndApplyBlocks _ _ _ = pass
    sscCalculateSeedQ i _ = do
        let h :: ByteString
            h = ByteArray.convert $ Hash.hashlazy @SHA256 (encodeLazy i)
        return $ Right (SharedSeed h)
