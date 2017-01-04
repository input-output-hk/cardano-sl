{-# LANGUAGE LambdaCase #-}

-- | GodTossing serialization instances

module Pos.Binary.Ssc () where

import           Data.Binary.Get                  (getWord8)
import           Data.Binary.Put                  (putWord8)
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Ssc.GodTossing.Secret.Types  (GtSecretStorage (..))
import           Pos.Ssc.GodTossing.Types.Base    (Commitment (..), Opening (..),
                                                   VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtMsgTag (..))
import           Pos.Ssc.GodTossing.Types.Types   (GtPayload (..), GtProof (..))

----------------------------------------------------------------------------
-- Types.Base
----------------------------------------------------------------------------

instance Bi Commitment where
    put Commitment{..} = do
        put commExtra
        put commProof
        put commShares
    get = liftM3 Commitment get get get

instance Bi VssCertificate where
    put VssCertificate{..} = do
        put vcVssKey
        put vcExpiryEpoch
        put vcSignature
        put vcSigningKey
    get = liftM4 VssCertificate get get get get

instance Bi Opening where
    put (Opening secret) = put secret
    get = Opening <$> get

----------------------------------------------------------------------------
-- Types.Types
----------------------------------------------------------------------------

instance Bi GtPayload where
    put x = case x of
        CommitmentsPayload comMap vssMap -> putWord8 0 >> put comMap >> put vssMap
        OpeningsPayload opMap vssMap     -> putWord8 1 >> put opMap >> put vssMap
        SharesPayload sharesMap vssMap   -> putWord8 2 >> put sharesMap >> put vssMap
        CertificatesPayload vssMap       -> putWord8 3 >> put vssMap
    get = getWord8 >>= \case
        0 -> liftM2 CommitmentsPayload get get
        1 -> liftM2 OpeningsPayload get get
        2 -> liftM2 SharesPayload get get
        3 -> CertificatesPayload <$> get
        tag -> fail ("get@GtPayload: invalid tag: " ++ show tag)

instance Bi GtProof where
    put x = case x of
        CommitmentsProof a b -> putWord8 0 >> put a >> put b
        OpeningsProof a b    -> putWord8 1 >> put a >> put b
        SharesProof a b      -> putWord8 2 >> put a >> put b
        CertificatesProof a  -> putWord8 3 >> put a
    get = getWord8 >>= \case
        0 -> liftM2 CommitmentsProof get get
        1 -> liftM2 OpeningsProof get get
        2 -> liftM2 SharesProof get get
        3 -> CertificatesProof <$> get
        tag -> fail ("get@GtProof: invalid tag: " ++ show tag)

----------------------------------------------------------------------------
-- Types.Message
----------------------------------------------------------------------------

instance Bi GtMsgTag where
    put msgtag = case msgtag of
        CommitmentMsg     -> putWord8 0
        OpeningMsg        -> putWord8 1
        SharesMsg         -> putWord8 2
        VssCertificateMsg -> putWord8 3
    get = getWord8 >>= \case
        0 -> pure CommitmentMsg
        1 -> pure OpeningMsg
        2 -> pure SharesMsg
        3 -> pure VssCertificateMsg
        tag -> fail ("get@MsgTag: invalid tag: " ++ show tag)

instance Bi GtMsgContents where
    put datamsg = case datamsg of
        MCCommitment signedComm  -> putWord8 0 >>  put signedComm
        MCOpening opening        -> putWord8 1 >>  put opening
        MCShares innerMap        -> putWord8 2 >>  put innerMap
        MCVssCertificate vssCert -> putWord8 3 >>  put vssCert
    get = getWord8 >>= \case
        0 -> liftM MCCommitment get
        1 -> liftM MCOpening get
        2 -> liftM MCShares get
        3 -> liftM MCVssCertificate get
        tag -> fail ("get@DataMsg: invalid tag: " ++ show tag)

----------------------------------------------------------------------------
-- SecretStorage Type
----------------------------------------------------------------------------
instance Bi GtSecretStorage where
    put (GtSecretStorage s last) = put s >> put last
    get = GtSecretStorage <$> get <*> get
