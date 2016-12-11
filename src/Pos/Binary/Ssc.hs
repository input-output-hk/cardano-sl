{-# LANGUAGE StandaloneDeriving #-}

-- | SSC-related serialization

module Pos.Binary.Ssc where

import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Ssc.GodTossing.Types.Base    (Commitment (..), Opening (..),
                                                   VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Message (DataMsg, InvMsg, MsgTag, ReqMsg)
import           Pos.Ssc.GodTossing.Types.Types   (GtGlobalState, GtPayload, GtProof)

----------------------------------------------------------------------------
-- Abstract
----------------------------------------------------------------------------

-- hayaku hayaku! impuremento kore no inusutanso kudasai!
instance Bi Commitment where
instance Bi VssCertificate where

----------------------------------------------------------------------------
-- GodTossing
----------------------------------------------------------------------------

instance Bi Opening
instance Bi GtGlobalState
instance Bi GtPayload
instance Bi GtProof
instance Bi MsgTag
instance Bi InvMsg
instance Bi ReqMsg
instance Bi DataMsg
