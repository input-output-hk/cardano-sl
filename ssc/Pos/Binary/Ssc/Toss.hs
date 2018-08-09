{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary instances for Toss types.

module Pos.Binary.Ssc.Toss
       (
       ) where

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi, deriveSimpleBiCxt)
import           Pos.Core.Ssc (CommitmentsMap, OpeningsMap, SharesMap, VssCertificatesMap)
import           Pos.Ssc.Toss.Types (SscTag (..), TossModifier (..))
import           Pos.Util.Util (cborError)

deriveSimpleBi ''SscTag [
    Cons 'CommitmentMsg [],
    Cons 'OpeningMsg [],
    Cons 'SharesMsg [],
    Cons 'VssCertificateMsg []]

deriveSimpleBiCxt [t|()|] ''TossModifier [
    Cons 'TossModifier [
        Field [| _tmCommitments  :: CommitmentsMap     |],
        Field [| _tmOpenings     :: OpeningsMap        |],
        Field [| _tmShares       :: SharesMap          |],
        Field [| _tmCertificates :: VssCertificatesMap |]
    ]]
