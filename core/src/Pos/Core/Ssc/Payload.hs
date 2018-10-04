module Pos.Core.Ssc.Payload
       ( SscPayload (..)
       , checkSscPayload
       , spVss
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (MonadError)
import qualified Data.HashMap.Strict as HM
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, bprint, int, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (listJson)

import           Pos.Crypto (ProtocolMagic, shortHashF)

import           Pos.Binary.Class (Cons (..), Field (..), deriveIndexedBi)
import           Pos.Core.Ssc.CommitmentsMap
import           Pos.Core.Ssc.OpeningsMap
import           Pos.Core.Ssc.SharesMap
import           Pos.Core.Ssc.VssCertificate (VssCertificate (vcExpiryEpoch))
import           Pos.Core.Ssc.VssCertificatesMap
import           Pos.Util.Util (cborError)

-- | Payload included into blocks.
data SscPayload
    = CommitmentsPayload
        !CommitmentsMap
        !VssCertificatesMap
    | OpeningsPayload
        !OpeningsMap
        !VssCertificatesMap
    | SharesPayload
        !SharesMap
        !VssCertificatesMap
    | CertificatesPayload
        !VssCertificatesMap
    deriving (Eq, Show, Generic)

instance NFData SscPayload

instance Buildable SscPayload where
    build gp
        | isEmptySscPayload gp = "  no SSC payload"
        | otherwise =
            case gp of
                CommitmentsPayload comms certs ->
                    formatTwo formatCommitments comms certs
                OpeningsPayload openings certs ->
                    formatTwo formatOpenings openings certs
                SharesPayload shares certs ->
                    formatTwo formatShares shares certs
                CertificatesPayload certs -> formatCertificates certs
      where
        formatIfNotNull
            :: Container c
            => Format Builder (c -> Builder) -> c -> Builder
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = bprint formatter l
        formatCommitments (getCommitmentsMap -> comms) =
            formatIfNotNull
                ("  commitments from: " %listJson % "\n")
                (HM.keys comms)
        formatOpenings openings =
            formatIfNotNull
                ("  openings from: " %listJson % "\n")
                (HM.keys openings)
        formatShares shares =
            formatIfNotNull
                ("  shares from: " %listJson % "\n")
                (HM.keys shares)
        formatCertificates (getVssCertificatesMap -> certs) =
            formatIfNotNull
                ("  certificates from: " %listJson % "\n")
                (map formatVssCert $ HM.toList certs)
        formatVssCert (id, cert) =
            bprint (shortHashF%":"%int) id (vcExpiryEpoch cert)
        formatTwo formatter hm certs =
            mconcat [formatter hm, formatCertificates certs]

isEmptySscPayload :: SscPayload -> Bool
isEmptySscPayload (CommitmentsPayload comms certs) = null comms && null certs
isEmptySscPayload (OpeningsPayload opens certs)    = null opens && null certs
isEmptySscPayload (SharesPayload shares certs)     = null shares && null certs
isEmptySscPayload (CertificatesPayload certs)      = null certs

spVss :: SscPayload -> VssCertificatesMap
spVss (CommitmentsPayload _ vss) = vss
spVss (OpeningsPayload    _ vss) = vss
spVss (SharesPayload      _ vss) = vss
spVss (CertificatesPayload  vss) = vss

checkSscPayload
    :: MonadError Text m
    => ProtocolMagic
    -> SscPayload
    -> m ()
checkSscPayload pm payload = checkVssCertificatesMap pm (spVss payload)


-- TH-generated instances go to the end of the file

deriveIndexedBi ''SscPayload [
    Cons 'CommitmentsPayload [
        Field [| 0 :: CommitmentsMap     |],
        Field [| 1 :: VssCertificatesMap |] ],
    Cons 'OpeningsPayload [
        Field [| 0 :: OpeningsMap        |],
        Field [| 1 :: VssCertificatesMap |] ],
    Cons 'SharesPayload [
        Field [| 0 :: SharesMap          |],
        Field [| 1 :: VssCertificatesMap |] ],
    Cons 'CertificatesPayload [
        Field [| 0 :: VssCertificatesMap |] ]
    ]

deriveSafeCopySimple 0 'base ''SscPayload
