-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), getWord8, label, putConst, putField)
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc                   ()
import           Pos.Binary.Update                ()
import           Pos.Communication.Types.Relay    (DataMsg (..), InvMsg (..),
                                                   MempoolMsg (..), ReqMsg (..))
import           Pos.Crypto                       (hash)
import           Pos.Delegation.Types             (ProxySKLightConfirmation)
import           Pos.Ssc.GodTossing.Types.Message (MCCommitment (..), MCOpening (..),
                                                   MCShares (..), MCVssCertificate (..))
import           Pos.Types                        (ProxySKHeavy, ProxySKLight)
import           Pos.Update.Core                  (UpdateProposal, UpdateVote (..))

instance (Bi key) => Bi (InvMsg key) where
    sizeNPut = putField imKey
    get = label "InvMsg" $ InvMsg <$> get

instance (Bi key) => Bi (ReqMsg key) where
    sizeNPut = putField rmKey
    get = label "ReqMsg" $ ReqMsg <$> get

instance Bi (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    sizeNPut = putConst @Word8 228
    get = label "MempoolMsg" $ do
        x <- getWord8
        when (x /= 228) $ fail "wrong byte"
        pure MempoolMsg

instance Bi (DataMsg MCCommitment) where
    sizeNPut = putField (\(DataMsg (MCCommitment signedComm)) -> signedComm)
    get = fmap DataMsg $ label "DataMsg MCCommitment" $ MCCommitment <$> get

instance Bi (DataMsg MCOpening) where
    sizeNPut = putField (\(DataMsg (MCOpening st _)) -> st) <>
               putField (\(DataMsg (MCOpening _ op)) -> op)
    get = fmap DataMsg $ label "DataMsg MCOpening" $ liftM2 MCOpening get get

instance Bi (DataMsg MCShares) where
    sizeNPut = putField (\(DataMsg (MCShares st _)) -> st) <>
               putField (\(DataMsg (MCShares _ im)) -> im)
    get = fmap DataMsg $ label "DataMsg MCShares" $ liftM2 MCShares get get

instance Bi (DataMsg MCVssCertificate) where
    sizeNPut = putField $ \(DataMsg (MCVssCertificate vssCert)) -> vssCert
    get = fmap DataMsg $ label "DataMsg MCVssCertificate" $ MCVssCertificate <$> get

instance Bi (DataMsg (UpdateProposal, [UpdateVote])) where
    sizeNPut = putField dmContents
    get = label "DataMsg (UpdateProposal, [UpdateVote])" $ do
        c@(up, votes) <- get
        let !id = hash up
        unless (all ((id ==) . uvProposalId) votes) $
            fail "get@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance Bi (DataMsg UpdateVote) where
    sizeNPut = putField dmContents
    get = label "DataMsg UpdateVote" $ DataMsg <$> get

instance Bi (DataMsg ProxySKLight) where
    sizeNPut = putField dmContents
    get = label "DataMsg ProxySKLight" $ DataMsg <$> get

instance Bi (DataMsg ProxySKHeavy) where
    sizeNPut = putField dmContents
    get = label "DataMsg ProxySKHeavy" $ DataMsg <$> get

instance Bi (DataMsg ProxySKLightConfirmation) where
    sizeNPut = putField dmContents
    get = label "DataMsg ProxySKLightConfirmation" $ DataMsg <$> get
