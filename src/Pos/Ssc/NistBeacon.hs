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
import qualified Data.Binary             as Binary
import qualified Data.ByteArray          as ByteArray (convert)
import           Data.Coerce             (coerce)
import qualified Data.HashMap.Strict     as HM
import           Data.SafeCopy           (SafeCopy)
import           Data.Serialize          (Serialize (..))
import           Data.Tagged             (Tagged (..))
import           Data.Text.Buildable     (Buildable (build))
import           Pos.Crypto              (Threshold, deterministicVssKeyGen,
                                          toVssPublicKey)
import           Pos.FollowTheSatoshi    (followTheSatoshi)
import           Pos.Ssc.Class.Listeners (SscListenersClass (..))
import           Pos.Ssc.Class.Storage   (SscQuery)
import           Pos.Ssc.Class.Storage   (HasSscStorage (..), SscStorageClass (..))
import           Pos.Ssc.Class.Types     (SscTypes (..))
import           Pos.Ssc.Class.Workers   (SscWorkersClass (..))
import           Pos.Types               (EpochIndex, FtsSeed (..), SlotLeaders, Utxo,
                                          getAddress)
import           Serokell.Util.Verify    (VerificationRes (..))
import           Universum

data SscNistBeacon
    deriving (Generic)

-- acid-state requires this instance because of a bug
instance SafeCopy SscNistBeacon
instance Serialize SscNistBeacon where
    put = panic "put@SscNistBeacon: can't happen"
    get = panic "get@SscNistBeacon: can't happen"

instance Buildable () where
    build _ = "()"

instance SscTypes SscNistBeacon where
    type SscStorage   SscNistBeacon = ()
    type SscPayload   SscNistBeacon = ()
    type SscProof     SscNistBeacon = ()
    type SscMessage   SscNistBeacon = ()
    type SscSeedError SscNistBeacon = ()
    type SscToken     SscNistBeacon = ()

    mkSscProof = Tagged $ const ()

instance SscStorageClass SscNistBeacon where
    sscApplyBlocks _ = pass
    sscPrepareToNewSlot _ = pass
    sscProcessMessage _ = pure Nothing
    sscRollback _ = pass
    sscGetLocalPayload _ = pure ()
    sscGetGlobalPayload = pure ()
    sscGetGlobalPayloadByDepth _ = pure Nothing
    sscVerifyBlocks _ _ = pure VerSuccess

    sscGetToken = pure Nothing
    sscSetToken _ = pass
    sscGetOurShares _ _ = pure HM.empty

    sscGetParticipants _ _  = pure $ Just $ pure $ toVssPublicKey $ deterministicVssKeyGen ""
    sscCalculateLeaders = calculateLeaders

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
               Hash.hashlazy @SHA256 . Binary.encode $ epoch
    return $ Right $ fmap getAddress $ followTheSatoshi seed utxo

instance SscWorkersClass SscNistBeacon where
    sscOnNewSlot = Tagged $ const $ return ()
    sscWorkers = Tagged []

instance SscListenersClass SscNistBeacon where
    sscListeners = Tagged []
