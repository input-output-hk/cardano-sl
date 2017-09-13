-- | Binary instances for Toss types.

module Pos.Binary.GodTossing.Toss
       (
       ) where

import           Pos.Binary.Class              (Cons (..), Field (..), deriveSimpleBi,
                                                deriveSimpleBiCxt)
import           Pos.Ssc.GodTossing.Core       (CommitmentsMap, OpeningsMap, SharesMap)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..), TossModifier (..))
import           Pos.Core.Configuration        (HasConfiguration)
import           Pos.Core.Vss                  (VssCertificatesMap)

deriveSimpleBi ''GtTag [
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
