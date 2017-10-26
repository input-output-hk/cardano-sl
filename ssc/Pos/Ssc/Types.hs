{-# LANGUAGE TypeFamilies #-}

-- | Some types related to GodTossing. There are also types in
-- "Pos.Core.Ssc".

module Pos.Ssc.Types
       (
         -- * Types
         SscGlobalState (..)
       , SscContext (..)
       , SscParams (..)
       , SscSecretStorage (..)
       , SscLocalData (..)
       , SscState (..)
       , SscBlock (..)

       -- * Lenses
       -- ** SscGlobalState
       , sgsCommitments
       , sgsOpenings
       , sgsShares
       , sgsVssCertificates
       -- ** SscLocalData
       , ldModifier
       , ldEpoch
       , ldSize

       -- * Misc
       , HasSscContext(..)
       , createSscContext
       ) where

import           Control.Lens                   (choosing, makeLenses, makeWrapped,
                                                 _Wrapped)
import           Data.Default                   (Default, def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder         (Builder, fromText)
import           Formatting                     (sformat, (%))
import           Serokell.Data.Memory.Units     (Byte)
import           Serokell.Util                  (listJson)
import           Universum

import           Pos.Core                       (EpochIndex, HasDifficulty (..),
                                                 HasEpochIndex (..), HasEpochOrSlot (..),
                                                 HasHeaderHash (..), IsGenesisHeader,
                                                 IsMainHeader)
import           Pos.Core.Ssc                   (CommitmentsMap (getCommitmentsMap),
                                                 Opening, OpeningsMap, SharesMap,
                                                 SignedCommitment, SscPayload)
import           Pos.Crypto                     (VssKeyPair)
import           Pos.Ssc.GodTossing.Behavior    (GtBehavior)
import           Pos.Ssc.GodTossing.Toss.Types  (TossModifier)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Util.Util                  (Some)

----------------------------------------------------------------------------
-- SscGlobalState
----------------------------------------------------------------------------

-- | Global state of GodTossing, contains relevant SSC data from blocks.
data SscGlobalState = SscGlobalState
    { -- | Commitments are added during the first phase of epoch.
      _sgsCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _sgsOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _sgsShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _sgsVssCertificates :: !VCD.VssCertData
    } deriving (Eq, Show, Generic)

makeLenses ''SscGlobalState

instance Default SscGlobalState where
    def =
        SscGlobalState
        {
          _sgsCommitments = mempty
        , _sgsOpenings = mempty
        , _sgsShares = mempty
        , _sgsVssCertificates = VCD.empty
        }

instance Buildable SscGlobalState where
    build SscGlobalState {..} =
        formatMPC $ mconcat
            [ formatCommitments
            , formatOpenings
            , formatShares
            , formatCertificates
            ]
      where
        formatMPC :: Text -> Builder
        formatMPC msg
            | T.null msg = "  no MPC data"
            | otherwise = fromText msg
        formatIfNotNull formatter l
            | null l = mempty
            | otherwise = sformat formatter l
        formatCommitments =
            formatIfNotNull
                ("  commitments from: "%listJson%"\n")
                (HM.keys $ getCommitmentsMap _sgsCommitments)
        formatOpenings =
            formatIfNotNull
                ("  openings from: "%listJson%"\n")
                (HM.keys _sgsOpenings)
        formatShares =
            formatIfNotNull
                ("  shares from: "%listJson%"\n")
                (HM.keys _sgsShares)
        formatCertificates =
            formatIfNotNull
                ("  certificates from: "%listJson%"\n")
                (VCD.keys _sgsVssCertificates)

-- | Needed options for creating SscContext
data SscParams = SscParams
    { spSscEnabled :: !Bool        -- ^ Whether node should participate in
                                   --    SSC in case SSC requires
                                   --    participation.
    , spVssKeyPair :: !VssKeyPair  -- ^ Key pair used for secret sharing
    , spBehavior   :: !GtBehavior  -- ^ Settings for the algorithm
    }

-- | SSC specific context in NodeContext
data SscContext = SscContext
    {
      -- | Vss key pair used for MPC.
      scVssKeyPair     :: !VssKeyPair
    , -- | Flag which determines whether we want to participate in SSC.
      -- TODO: consider putting it into GtBehavior?
      scParticipateSsc :: !(TVar Bool)
    , -- | Settings for the algorithm
      scBehavior       :: !(TVar GtBehavior)
    }

createSscContext :: MonadIO m => SscParams -> m SscContext
createSscContext SscParams {..} =
    SscContext spVssKeyPair
          <$> newTVarIO spSscEnabled
          <*> newTVarIO spBehavior

class HasSscContext ctx where
    sscContext :: Lens' ctx SscContext

----------------------------------------------------------------------------
-- Secret storage
----------------------------------------------------------------------------

data SscSecretStorage = SscSecretStorage
    { -- | Our commitment.
      sssCommitment :: !SignedCommitment
    , -- | Corresponding opening
      sssOpening    :: !Opening
    , -- | Epoch for which this secret were generated
      sssEpoch      :: !EpochIndex
    } deriving (Generic, Show, Eq)

----------------------------------------------------------------------------
-- SscLocalData
----------------------------------------------------------------------------

-- | Internal SSC state stored in memory
data SscLocalData = SscLocalData
    { -- | 'TossModifier' which also serves as mempool of GT data,
      -- because for GodTossing modifier and mempool are same.
      _ldModifier :: !TossModifier
    , -- | Epoch for which this mempool can be used to form payload.
      _ldEpoch    :: !EpochIndex
    , -- | Approximate size of this mempool (raw bytes).
      _ldSize     :: !Byte
    }

makeLenses ''SscLocalData

----------------------------------------------------------------------------
-- SscState
----------------------------------------------------------------------------

-- | Whole state of SSC. Stored only in-memory by design.
data SscState =
    SscState
    { sscGlobal :: !(TVar SscGlobalState)
    , sscLocal  :: !(TVar SscLocalData)
    }

----------------------------------------------------------------------------
-- SscBlock
----------------------------------------------------------------------------

-- [CSL-1156] Find a better way for this
--
-- NB. there are plans to make it a 'type' (like 'TxpBlock' and
-- 'UpdateBlock'). Previously it wasn't possible, but now it is.
newtype SscBlock = SscBlock
    { getSscBlock :: Either (Some IsGenesisHeader)
                            (Some IsMainHeader, SscPayload)
    }

makeWrapped ''SscBlock

instance HasDifficulty SscBlock where
    difficultyL = _Wrapped . choosing difficultyL (_1 . difficultyL)
instance HasEpochIndex SscBlock where
    epochIndexL = _Wrapped . choosing epochIndexL (_1 . epochIndexL)
instance HasHeaderHash SscBlock where
    headerHash     = either headerHash (headerHash . fst) . getSscBlock
instance HasEpochOrSlot SscBlock where
    getEpochOrSlot = either getEpochOrSlot (getEpochOrSlot . fst) . getSscBlock
