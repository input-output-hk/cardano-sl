module Pos.Core.Update.Proposal
       ( UpdateProposal (..)
       , UpdateProposals
       , UpId
       , UpAttributes
       , UpdateProposalToSign (..)
       , formatMaybeProposal
       , mkUpdateProposalWSign
       , checkUpdateProposal
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (bprint, build, builder, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..),
                     deriveSimpleBi, encodeListLen, enforceSize)
import           Pos.Crypto (Hash, ProtocolMagic, PublicKey, SafeSigner,
                     SignTag (SignUSProposal), Signature, checkSig, hash,
                     safeSign, safeToPublic)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

import           Pos.Core.Update.BlockVersion
import           Pos.Core.Update.BlockVersionModifier
import           Pos.Core.Update.Data
import           Pos.Core.Update.SoftwareVersion
import           Pos.Core.Update.SystemTag

-- | ID of software update proposal
type UpId = Hash UpdateProposal

type UpAttributes = Attributes ()

data UpdateProposalToSign
    = UpdateProposalToSign
    { upsBV   :: !BlockVersion
    , upsBVM  :: !BlockVersionModifier
    , upsSV   :: !SoftwareVersion
    , upsData :: !(HM.HashMap SystemTag UpdateData)
    , upsAttr :: !UpAttributes
    } deriving (Eq, Show, Generic)

-- | Proposal for software update
data UpdateProposal = UnsafeUpdateProposal
    { upBlockVersion    :: !BlockVersion
    , upBlockVersionMod :: !BlockVersionModifier
    , upSoftwareVersion :: !SoftwareVersion
    , upData            :: !(HM.HashMap SystemTag UpdateData)
    -- ^ UpdateData for each system which this update affects.
    -- It must be non-empty.
    , upAttributes      :: !UpAttributes
    -- ^ Attributes which are currently empty, but provide
    -- extensibility.
    , upFrom            :: !PublicKey
    -- ^ Who proposed this UP.
    , upSignature       :: !(Signature UpdateProposalToSign)
    } deriving (Eq, Show, Generic, Typeable)

type UpdateProposals = HashMap UpId UpdateProposal

instance Hashable UpdateProposal

instance Buildable UpdateProposal where
    build up@UnsafeUpdateProposal {..} =
      bprint (build%
              " { block v"%build%
              ", UpId: "%build%
              ", "%build%
              ", tags: "%listJson%
              ", "%builder%
              " }")
        upSoftwareVersion
        upBlockVersion
        (hash up)
        upBlockVersionMod
        (HM.keys upData)
        attrsBuilder
      where
        attrs = upAttributes
        attrsBuilder
            | areAttributesKnown upAttributes = "no attributes"
            | otherwise = bprint ("attributes: " %build) attrs

instance NFData UpdateProposal

instance Bi UpdateProposal where
    encode up = encodeListLen 7
            <> encode (upBlockVersion up)
            <> encode (upBlockVersionMod up)
            <> encode (upSoftwareVersion up)
            <> encode (upData up)
            <> encode (upAttributes up)
            <> encode (upFrom up)
            <> encode (upSignature up)
    decode = do
        enforceSize "UpdateProposal" 7
        UnsafeUpdateProposal <$> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode

formatMaybeProposal :: Maybe UpdateProposal -> Builder
formatMaybeProposal = maybe "no proposal" Buildable.build

mkUpdateProposalWSign
    :: ProtocolMagic
    -> BlockVersion
    -> BlockVersionModifier
    -> SoftwareVersion
    -> HM.HashMap SystemTag UpdateData
    -> UpAttributes
    -> SafeSigner
    -> UpdateProposal
mkUpdateProposalWSign pm upBlockVersion upBlockVersionMod upSoftwareVersion upData upAttributes ss =
    UnsafeUpdateProposal {..}
  where
    toSign =
        UpdateProposalToSign
            upBlockVersion
            upBlockVersionMod
            upSoftwareVersion
            upData
            upAttributes
    upFrom = safeToPublic ss
    upSignature = safeSign pm SignUSProposal ss toSign

checkUpdateProposal
    :: MonadError Text m
    => ProtocolMagic
    -> UpdateProposal
    -> m ()
checkUpdateProposal pm it = do
    checkBlockVersionModifier (upBlockVersionMod it)
    checkSoftwareVersion (upSoftwareVersion it)
    forM_ (HM.keys (upData it)) checkSystemTag
    let toSign = UpdateProposalToSign
            (upBlockVersion it)
            (upBlockVersionMod it)
            (upSoftwareVersion it)
            (upData it)
            (upAttributes it)
    unless (checkSig pm SignUSProposal (upFrom it) toSign (upSignature it))
        (throwError "UpdateProposal: invalid signature")

-- TH generated instances at the end of the file.

deriveSimpleBi ''UpdateProposalToSign [
    Cons 'UpdateProposalToSign [
        Field [| upsBV   :: BlockVersion                     |],
        Field [| upsBVM  :: BlockVersionModifier           |],
        Field [| upsSV   :: SoftwareVersion                  |],
        Field [| upsData :: HashMap SystemTag UpdateData |],
        Field [| upsAttr :: UpAttributes                   |]
    ]]

deriveSafeCopySimple 0 'base ''UpdateProposal
