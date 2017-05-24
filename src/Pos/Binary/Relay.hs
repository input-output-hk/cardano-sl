-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), getWord8, label, putWord8)
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
    put InvMsg {..} = put imKey
    get = label "InvMsg" $ InvMsg <$> get

instance (Bi key) => Bi (ReqMsg key) where
    put ReqMsg {..} = put rmKey
    get = label "ReqMsg" $ ReqMsg <$> get

instance Bi (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    put MempoolMsg = putWord8 228
    get = label "MempoolMsg" $ do
        x <- getWord8
        when (x /= 228) $ fail "wrong byte"
        pure MempoolMsg

instance Bi (DataMsg MCCommitment) where
    put (DataMsg (MCCommitment signedComm)) = put signedComm
    get = fmap DataMsg $ label "DataMsg MCCommitment" $ MCCommitment <$> get

instance Bi (DataMsg MCOpening) where
    put (DataMsg (MCOpening st op)) = put st >> put op
    get = fmap DataMsg $ label "DataMsg MCOpening" $ liftM2 MCOpening get get

instance Bi (DataMsg MCShares) where
    put (DataMsg (MCShares st im)) = put st >> put im
    get = fmap DataMsg $ label "DataMsg MCShares" $ liftM2 MCShares get get

instance Bi (DataMsg MCVssCertificate) where
    put (DataMsg (MCVssCertificate vssCert)) = put vssCert
    get = fmap DataMsg $ label "DataMsg MCVssCertificate" $ MCVssCertificate <$> get

instance Bi (DataMsg (UpdateProposal, [UpdateVote])) where
    put (DataMsg dmContents) = put dmContents
    get = label "DataMsg (UpdateProposal, [UpdateVote])" $ do
        c@(up, votes) <- get
        let !id = hash up
        unless (all ((id ==) . uvProposalId) votes) $
            fail "get@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance Bi (DataMsg UpdateVote) where
    put (DataMsg uv) = put uv
    get = label "DataMsg UpdateVote" $ DataMsg <$> get

instance Bi (DataMsg ProxySKLight) where
    put (DataMsg p) = put p
    get = label "DataMsg ProxySKLight" $ DataMsg <$> get

instance Bi (DataMsg ProxySKHeavy) where
    put (DataMsg p) = put p
    get = label "DataMsg ProxySKHeavy" $ DataMsg <$> get

instance Bi (DataMsg ProxySKLightConfirmation) where
    put (DataMsg p) = put p
    get = label "DataMsg ProxySKLightConfirmation" $ DataMsg <$> get
