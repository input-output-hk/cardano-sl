{-# LANGUAGE LambdaCase #-}

-- | GodTossing serialization instances

module Pos.Binary.Ssc () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), getWord8, label, putWord8)
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc.GodTossing.Core   ()
import           Pos.Binary.Ssc.GodTossing.Toss   ()
import           Pos.Binary.Ssc.GodTossing.Types  ()
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..))

----------------------------------------------------------------------------
-- Types.Message
----------------------------------------------------------------------------

instance Bi GtMsgContents where
    put datamsg = case datamsg of
        MCCommitment signedComm   -> putWord8 0 >> put signedComm
        MCOpening stkhdId opening -> putWord8 1 >> put stkhdId >> put opening
        MCShares stkhdId innerMap -> putWord8 2 >> put stkhdId >> put innerMap
        MCVssCertificate vssCert  -> putWord8 3 >> put vssCert
    get = label "GtMsgContents" $ do
        getWord8 >>= \case
            0 -> MCCommitment <$> get
            1 -> liftM2 MCOpening get get
            2 -> liftM2 MCShares get get
            3 -> MCVssCertificate <$> get
            tag -> fail ("get@DataMsg: invalid tag: " ++ show tag)
