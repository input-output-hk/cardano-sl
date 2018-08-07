-- | Types related to Toss.

module Pos.Chain.Ssc.Toss.Types
       ( SscTag (..)
       , isGoodSlotForTag
       , isGoodSlotIdForTag

       , TossModifier (..)
       , tmCommitments
       , tmOpenings
       , tmShares
       , tmCertificates

       , TossT
       , runTossT
       , evalTossT
       , execTossT
       ) where

import           Universum hiding (id)

import           Control.Lens (at, makeLenses, (%=), (.=))
import qualified Ether
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi,
                     deriveSimpleBiCxt)
import           Pos.Chain.Ssc.Base (deleteSignedCommitment,
                     insertSignedCommitment, isCommitmentId, isCommitmentIdx,
                     isOpeningId, isOpeningIdx, isSharesId, isSharesIdx)
import           Pos.Chain.Ssc.Toss.Class (MonadToss (..), MonadTossEnv (..),
                     MonadTossRead (..))
import           Pos.Core (HasProtocolConstants, LocalSlotIndex, SlotId)
import           Pos.Core.Ssc (CommitmentsMap, OpeningsMap, SharesMap,
                     VssCertificatesMap, insertVss)
import           Pos.Util.Util (cborError, ether)

-- | Tag corresponding to SSC data.
data SscTag
    = CommitmentMsg
    | OpeningMsg
    | SharesMsg
    | VssCertificateMsg
    deriving (Show, Eq, Generic)

instance Buildable SscTag where
    build CommitmentMsg     = "commitment"
    build OpeningMsg        = "opening"
    build SharesMsg         = "shares"
    build VssCertificateMsg = "VSS certificate"

isGoodSlotForTag :: HasProtocolConstants => SscTag -> LocalSlotIndex -> Bool
isGoodSlotForTag CommitmentMsg     = isCommitmentIdx
isGoodSlotForTag OpeningMsg        = isOpeningIdx
isGoodSlotForTag SharesMsg         = isSharesIdx
isGoodSlotForTag VssCertificateMsg = const True

isGoodSlotIdForTag :: HasProtocolConstants => SscTag -> SlotId -> Bool
isGoodSlotIdForTag CommitmentMsg     = isCommitmentId
isGoodSlotIdForTag OpeningMsg        = isOpeningId
isGoodSlotIdForTag SharesMsg         = isSharesId
isGoodSlotIdForTag VssCertificateMsg = const True

data TossModifier = TossModifier
    { _tmCommitments  :: !CommitmentsMap
    , _tmOpenings     :: !OpeningsMap
    , _tmShares       :: !SharesMap
    , _tmCertificates :: !VssCertificatesMap
    } deriving (Generic, Show, Eq)

instance Semigroup TossModifier where
   (TossModifier leftComms leftOpens leftShares leftCerts)
     <> (TossModifier rightComms rightOpens rightShares rightCerts) =
        TossModifier
        { _tmCommitments = rightComms <> leftComms
        , _tmOpenings = rightOpens <> leftOpens
        , _tmShares = rightShares <> leftShares
        , _tmCertificates = rightCerts <> leftCerts
        }

instance Monoid TossModifier where
    mempty = TossModifier mempty mempty mempty mempty
    mappend = (<>)

----------------------------------------------------------------------------
-- Tranformer
----------------------------------------------------------------------------

-- | Monad transformer which stores TossModifier and implements
-- writable MonadToss.
--
-- [WARNING] This transformer uses StateT and is intended for
-- single-threaded usage only.
type TossT = Ether.StateT' TossModifier

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runTossT :: TossModifier -> TossT m a -> m (a, TossModifier)
runTossT = flip Ether.runStateT

evalTossT :: Monad m => TossModifier -> TossT m a -> m a
evalTossT = flip Ether.evalStateT

execTossT :: Monad m => TossModifier -> TossT m a -> m TossModifier
execTossT = flip Ether.execStateT

----------------------------------------------------------------------------
-- MonadToss
----------------------------------------------------------------------------

makeLenses ''TossModifier

instance MonadTossRead m =>
         MonadTossRead (TossT m) where
    getCommitments = ether $ (<>) <$> use tmCommitments <*> getCommitments
    getOpenings = ether $ (<>) <$> use tmOpenings <*> getOpenings
    getShares = ether $ (<>) <$> use tmShares <*> getShares
    getVssCertificates = ether $ (<>) <$> use tmCertificates <*> getVssCertificates
    getStableCertificates = ether . getStableCertificates

instance MonadTossEnv m =>
         MonadTossEnv (TossT m) where
    getRichmen = ether . getRichmen
    getAdoptedBVData = ether getAdoptedBVData

instance MonadToss m =>
         MonadToss (TossT m) where
    putCommitment signedComm =
        ether $ tmCommitments %= insertSignedCommitment signedComm
    putOpening id op =
        ether $ tmOpenings . at id .= Just op
    putShares id sh =
        ether $ tmShares . at id .= Just sh
    -- NB. 'insertVss' might delete some certs from the map, but it
    -- shouldn't actually happen in practice because
    -- 'checkCertificatesPayload' ensures that there are no clashes between
    -- the certificates in blocks and certificates in the map
    putCertificate cert =
        ether $ tmCertificates %= fst . insertVss cert
    delCommitment id =
        ether $ tmCommitments %= deleteSignedCommitment id
    delOpening id =
        ether $ tmOpenings . at id .= Nothing
    delShares id =
        ether $ tmShares . at id .= Nothing
    resetCO = ether $ do
        tmCommitments .= mempty
        tmOpenings .= mempty
        tmCertificates .= mempty
        resetCO
    resetShares = ether $ do
        tmShares .= mempty
        resetShares
    setEpochOrSlot = ether . setEpochOrSlot

deriveSimpleBiCxt [t|()|] ''TossModifier [
    Cons 'TossModifier [
        Field [| _tmCommitments  :: CommitmentsMap     |],
        Field [| _tmOpenings     :: OpeningsMap        |],
        Field [| _tmShares       :: SharesMap          |],
        Field [| _tmCertificates :: VssCertificatesMap |]
    ]]

deriveSimpleBi ''SscTag [
    Cons 'CommitmentMsg [],
    Cons 'OpeningMsg [],
    Cons 'SharesMsg [],
    Cons 'VssCertificateMsg []]
