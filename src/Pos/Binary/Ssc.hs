{-# LANGUAGE LambdaCase #-}

-- | GodTossing serialization instances

module Pos.Binary.Ssc () where

import           Data.Binary.Get                  (getWord8)
import           Data.Binary.Put                  (putWord8)
import qualified Data.HashMap.Strict              as HM
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc.GodTossing.Core   ()
import           Pos.Ssc.GodTossing.Core          (VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtMsgTag (..))
import           Pos.Ssc.GodTossing.Types.Types   (GtPayload (..), GtProof (..),
                                                   GtSecretStorage (..))
import           Pos.Types.Address                (addressHash)

----------------------------------------------------------------------------
-- Types.Types
----------------------------------------------------------------------------

instance Bi GtPayload where
    put x =
        case x of
            CommitmentsPayload commMap vssMap ->
                putWord8 0 >> put (toList commMap) >> put (toList vssMap)
            OpeningsPayload opMap vssMap ->
                putWord8 1 >> put opMap >> put (toList vssMap)
            SharesPayload sharesMap vssMap ->
                putWord8 2 >> put sharesMap >> put (toList vssMap)
            CertificatesPayload vssMap -> putWord8 3 >> put (toList vssMap)
    get =
        getWord8 >>= \case
            0 -> liftM2 CommitmentsPayload getCommitments getVssCerts
            1 -> liftM2 OpeningsPayload get getVssCerts
            2 -> liftM2 SharesPayload get getVssCerts
            3 -> CertificatesPayload <$> getVssCerts
            tag -> fail ("get@GtPayload: invalid tag: " ++ show tag)
          where
            getCommitments = HM.fromList . map toCommPair <$> get
            toCommPair signedComm@(pk, _, _) = (addressHash pk, signedComm)
            getVssCerts = HM.fromList . map toCertPair <$> get
            toCertPair vc = (addressHash $ vcSigningKey vc, vc)

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
    put (GtSecretStorage c o e) = put c >> put o >> put e
    get = GtSecretStorage <$> get <*> get <*> get
