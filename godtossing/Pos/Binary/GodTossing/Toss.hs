-- | Binary instances for Toss types.

module Pos.Binary.GodTossing.Toss
       (
       ) where

import           Pos.Binary.Class              (Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Binary.Cbor               as Cbor
import           Pos.Ssc.GodTossing.Core       (CommitmentsMap, OpeningsMap, SharesMap,
                                                VssCertificatesMap)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..), TossModifier (..))

deriveSimpleBi ''GtTag [
    Cons 'CommitmentMsg [],
    Cons 'OpeningMsg [],
    Cons 'SharesMsg [],
    Cons 'VssCertificateMsg []]

Cbor.deriveSimpleBi ''GtTag [
    Cbor.Cons 'CommitmentMsg [],
    Cbor.Cons 'OpeningMsg [],
    Cbor.Cons 'SharesMsg [],
    Cbor.Cons 'VssCertificateMsg []]

deriveSimpleBi ''TossModifier [
    Cons 'TossModifier [
        Field [| _tmCommitments  :: CommitmentsMap     |],
        Field [| _tmOpenings     :: OpeningsMap        |],
        Field [| _tmShares       :: SharesMap          |],
        Field [| _tmCertificates :: VssCertificatesMap |]
    ]]

Cbor.deriveSimpleBi ''TossModifier [
    Cbor.Cons 'TossModifier [
        Cbor.Field [| _tmCommitments  :: CommitmentsMap     |],
        Cbor.Field [| _tmOpenings     :: OpeningsMap        |],
        Cbor.Field [| _tmShares       :: SharesMap          |],
        Cbor.Field [| _tmCertificates :: VssCertificatesMap |]
    ]]
