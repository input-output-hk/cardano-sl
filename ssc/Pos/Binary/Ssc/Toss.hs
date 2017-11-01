-- | Binary instances for Toss types.

module Pos.Binary.Ssc.Toss
       (
       ) where

import           Pos.Binary.Class              (Cons (..), Field (..), deriveSimpleBi,
                                                deriveSimpleBiCxt)
import           Pos.Ssc.Core                  (CommitmentsMap, OpeningsMap, SharesMap)
import           Pos.Ssc.Toss.Types            (SscTag (..), TossModifier (..))
import           Pos.Core.Configuration        (HasConfiguration)
import           Pos.Core.Vss                  (VssCertificatesMap)

deriveSimpleBi ''SscTag [
    Cons 'CommitmentMsg [],
    Cons 'OpeningMsg [],
    Cons 'SharesMsg [],
    Cons 'VssCertificateMsg []]

deriveSimpleBiCxt [t|HasConfiguration|] ''TossModifier [
    Cons 'TossModifier [
        Field [| _tmCommitments  :: CommitmentsMap     |],
        Field [| _tmOpenings     :: OpeningsMap        |],
        Field [| _tmShares       :: SharesMap          |],
        Field [| _tmCertificates :: VssCertificatesMap |]
    ]]
