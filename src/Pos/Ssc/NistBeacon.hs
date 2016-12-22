{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Simplified NIST beacon implementation of SSC.

module Pos.Ssc.NistBeacon
       ( SscNistBeacon
       ) where

import           Crypto.Hash             (SHA256)
import qualified Crypto.Hash             as Hash
import qualified Data.ByteArray          as ByteArray (convert)
import           Data.Coerce             (coerce)
import qualified Data.HashMap.Strict     as HM
import           Data.SafeCopy           (SafeCopy)
import           Data.Serialize          (Serialize (..))
import           Data.Tagged             (Tagged (..))
import           Data.Text.Buildable     (Buildable (build))
import           Serokell.Util.Verify    (VerificationRes (..))
import           Universum

import           Pos.Binary.Class        (encode)
import           Pos.Crypto              (Threshold, deterministicVssKeyGen,
                                          toVssPublicKey)
import           Pos.FollowTheSatoshi    (followTheSatoshi)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass (..))
import           Pos.Ssc.Class.Listeners (SscListenersClass (..))
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..), SscLocalDataClassM (..))
import           Pos.Ssc.Class.Storage   (HasSscStorage (..), SscQuery,
                                          SscStorageClass (..), SscStorageClassM (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Class.Workers   (SscWorkersClass (..))
import           Pos.Types               (EpochIndex, SharedSeed (..), SlotLeaders, Utxo)
import           Pos.Util                (asBinary)

-- | Data type tag for Nist Beacon implementation of Shared Seed Calculation.
data SscNistBeacon
    deriving (Generic)

-- acid-state requires this instance because of a bug
instance SafeCopy SscNistBeacon
instance Serialize SscNistBeacon where
    put = panic "put@SscNistBeacon: can't happen"
    get = panic "get@SscNistBeacon: can't happen"

instance Buildable () where
    build _ = "()"

instance Ssc SscNistBeacon where
    type SscLocalData   SscNistBeacon = ()
    type SscStorage     SscNistBeacon = ()
    type SscPayload     SscNistBeacon = ()
    type SscProof       SscNistBeacon = ()
    type SscSeedError   SscNistBeacon = ()
    type SscGlobalState SscNistBeacon = ()
    type SscNodeContext SscNistBeacon = ()
    type SscParams      SscNistBeacon = ()

    mkSscProof = Tagged $ const ()
    sscFilterPayload _ _ = ()
    sscCreateNodeContext _ = return ()

    -- Modern
    type SscGlobalStateM SscNistBeacon = ()
    type SscLocalDataM SscNistBeacon = ()

instance SscStorageClass SscNistBeacon where
    sscApplyBlocks _ = pass
    sscRollback _ = pass
    sscGetGlobalState = pure ()
    sscGetGlobalStateByDepth _ = pure Nothing
    sscVerifyBlocks _ _ = pure VerSuccess

    sscGetOurShares _ = pure HM.empty

    sscGetParticipants _ _  =
        pure $ Just $ pure $ asBinary $ toVssPublicKey $ deterministicVssKeyGen ""
    sscCalculateLeaders = calculateLeaders

instance SscHelpersClass SscNistBeacon where
    sscVerifyPayload = Tagged $ const $ const VerSuccess

type Query a = SscQuery SscNistBeacon a

instance (SscStorage ssc ~ ()) => HasSscStorage ssc () where
    sscStorage = identity

-- | Calculate leaders for the next epoch as hash epoch.
calculateLeaders
    :: EpochIndex
    -> Utxo            -- ^ Utxo (k slots before the end of epoch)
    -> Threshold
    -> Query (Either () SlotLeaders)
calculateLeaders epoch utxo _ = do
    let seed = coerce . ByteArray.convert @_ @ByteString .
               Hash.hashlazy @SHA256 . encode $ epoch
    return $ Right $ followTheSatoshi seed utxo

instance SscWorkersClass SscNistBeacon where
    sscWorkers = Tagged []

instance SscListenersClass SscNistBeacon where
    sscListeners = Tagged []

instance SscLocalDataClass SscNistBeacon where
    sscEmptyLocalData = ()
    sscGetLocalPayloadQ _ = pure ()
    sscApplyGlobalStateU _ = pure ()

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

instance SscLocalDataClassM SscNistBeacon where
    sscEmptyLocalDataM = ()
    sscGetLocalPayloadMQ _ = pure ()
    sscApplyGlobalStateMU _ = pure ()

instance SscStorageClassM SscNistBeacon where
    sscLoadGlobalState _ = pure ()
    sscApplyBlocksM _ = pure ()
    sscRollbackM _ = pure ()
    sscVerifyBlocksM _ _ = pure mempty
    sscCalculateSeedM =
        pure . Right . coerce . ByteArray.convert @_ @ByteString .
            Hash.hashlazy @SHA256 . encode
