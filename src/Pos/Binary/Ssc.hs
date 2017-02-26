
-- | GodTossing serialization instances

module Pos.Binary.Ssc () where

import           Data.Binary.Get                  (getWord8, label)
import           Data.Binary.Put                  (putWord8)
import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc.GodTossing.Core   ()
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtTag (..))
import           Pos.Ssc.GodTossing.Types.Types   (GtSecretStorage (..))

----------------------------------------------------------------------------
-- Types.Message
----------------------------------------------------------------------------

instance Bi GtTag where
    put msgtag = case msgtag of
        CommitmentMsg     -> putWord8 0
        OpeningMsg        -> putWord8 1
        SharesMsg         -> putWord8 2
        VssCertificateMsg -> putWord8 3
    get = label "GtTag" $ do
        getWord8 >>= \case
            0 -> pure CommitmentMsg
            1 -> pure OpeningMsg
            2 -> pure SharesMsg
            3 -> pure VssCertificateMsg
            tag -> fail ("get@MsgTag: invalid tag: " ++ show tag)

instance Bi GtMsgContents where
    put datamsg = case datamsg of
        MCCommitment signedComm   -> putWord8 0 >> put signedComm
        MCOpening stkhdId opening -> putWord8 1 >> put stkhdId >> put opening
        MCShares stkhdId innerMap -> putWord8 2 >> put stkhdId >> put innerMap
        MCVssCertificate vssCert  -> putWord8 3 >> put vssCert
    get = label "GtMsgContents" $ do
        getWord8 >>= \case
            0 -> liftM MCCommitment get
            1 -> liftM2 MCOpening get get
            2 -> liftM2 MCShares get get
            3 -> liftM MCVssCertificate get
            tag -> fail ("get@DataMsg: invalid tag: " ++ show tag)

----------------------------------------------------------------------------
-- SecretStorage Type
----------------------------------------------------------------------------
instance Bi GtSecretStorage where
    put (GtSecretStorage c o e) = put c >> put o >> put e
    get = label "GtSecretStorage" $
        GtSecretStorage <$> get <*> get <*> get
