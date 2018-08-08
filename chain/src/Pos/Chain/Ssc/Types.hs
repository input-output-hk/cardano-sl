{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Some types related to SSC. There are also types in
-- "Pos.Core.Ssc".

module Pos.Chain.Ssc.Types
       (
         -- * Types
         SscGlobalState (..)
       , SscContext (..)
       , SscParams (..)
       , SscSecretStorage (..)
       , SscLocalData (..)
       , SscState (..)
       , SscBlock

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

import           Control.DeepSeq (NFData)
import           Control.Lens (makeLenses)
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder, fromText)
import           Formatting (sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util (listJson)
import           Universum

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi,
                     deriveSimpleBiCxt)
import           Pos.Chain.Block.Union (ComponentBlock (..))
import           Pos.Chain.Ssc.Behavior (SscBehavior)
import           Pos.Chain.Ssc.Toss.Types (TossModifier)
import           Pos.Chain.Ssc.VssCertData as VCD
import           Pos.Core (EpochIndex)
import           Pos.Core.Ssc (CommitmentsMap (getCommitmentsMap), Opening,
                     OpeningsMap, SharesMap, SignedCommitment, SscPayload)
import           Pos.Crypto (VssKeyPair)

----------------------------------------------------------------------------
-- SscGlobalState
----------------------------------------------------------------------------

-- | Global state of SSC, contains relevant SSC data from blocks.
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

instance NFData SscGlobalState

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

deriveSimpleBiCxt [t|()|] ''SscGlobalState [
    Cons 'SscGlobalState [
        Field [| _sgsCommitments     :: CommitmentsMap |],
        Field [| _sgsOpenings        :: OpeningsMap    |],
        Field [| _sgsShares          :: SharesMap      |],
        Field [| _sgsVssCertificates :: VCD.VssCertData    |]
    ]]

-- | Needed options for creating SscContext
data SscParams = SscParams
    { spSscEnabled :: !Bool        -- ^ Whether node should participate in
                                   --    SSC in case SSC requires
                                   --    participation.
    , spVssKeyPair :: !VssKeyPair  -- ^ Key pair used for secret sharing
    , spBehavior   :: !SscBehavior -- ^ Settings for the algorithm
    }

-- | SSC specific context in NodeContext
data SscContext = SscContext
    {
      -- | Vss key pair used for MPC.
      scVssKeyPair     :: !VssKeyPair
    , -- | Flag which determines whether we want to participate in SSC.
      -- TODO: consider putting it into SscBehavior?
      scParticipateSsc :: !(TVar Bool)
    , -- | Settings for the algorithm
      scBehavior       :: !(TVar SscBehavior)
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

deriveSimpleBi ''SscSecretStorage [
    Cons 'SscSecretStorage [
        Field [| sssCommitment :: SignedCommitment |],
        Field [| sssOpening    :: Opening          |],
        Field [| sssEpoch      :: EpochIndex       |]
    ]]


----------------------------------------------------------------------------
-- SscLocalData
----------------------------------------------------------------------------

-- | Internal SSC state stored in memory
data SscLocalData = SscLocalData
    { -- | 'TossModifier' which also serves as mempool of SSC data,
      -- because for SSC modifier and mempool are same.
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

type SscBlock = ComponentBlock SscPayload
