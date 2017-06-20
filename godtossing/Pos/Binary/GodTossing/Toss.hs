{-# LANGUAGE TemplateHaskell #-}

-- | Binary instances for Toss types.

module Pos.Binary.GodTossing.Toss
       (
       ) where

import           Pos.Binary.Class              (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Ssc.GodTossing.Core       (CommitmentsMap, OpeningsMap, SharesMap,
                                                VssCertificatesMap)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..), TossModifier (..))

deriveSimpleBi ''GtTag [
    Cons 'CommitmentMsg [],
    Cons 'OpeningMsg [],
    Cons 'SharesMsg [],
    Cons 'VssCertificateMsg []]

deriveSimpleBi ''TossModifier [
    Cons 'TossModifier [
        Field [| _tmCommitments  :: CommitmentsMap     |],
        Field [| _tmOpenings     :: OpeningsMap        |],
        Field [| _tmShares       :: SharesMap          |],
        Field [| _tmCertificates :: VssCertificatesMap |]
    ]]
