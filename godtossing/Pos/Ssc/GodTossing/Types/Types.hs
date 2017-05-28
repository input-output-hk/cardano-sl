{-# LANGUAGE TemplateHaskell #-}

-- | Some types related to GodTossing necessary for Ssc instance.

module Pos.Ssc.GodTossing.Types.Types
       (
         -- * Instance types
         GtGlobalState (..)
       , GtContext (..)
       , GtParams (..)
       , GtSecretStorage (..)

       -- * Lenses
       -- ** GtPayload
       , gsCommitments
       , gsOpenings
       , gsShares
       , gsVssCertificates
       , createGtContext
       ) where

import           Control.Lens                   (makeLenses)
import           Data.Default                   (Default, def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder         (Builder, fromText)
import           Formatting                     (sformat, (%))
import           Serokell.Util                  (listJson)
import           Universum

import           Pos.Core                       (EpochIndex)
import           Pos.Crypto                     (VssKeyPair)
import           Pos.Ssc.GodTossing.Core        (CommitmentsMap (getCommitmentsMap),
                                                 Opening, OpeningsMap, SharesMap,
                                                 SignedCommitment)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD

----------------------------------------------------------------------------
-- SscGlobalState
----------------------------------------------------------------------------

-- | Global state of GodTossing, contains relevant SSC data from blocks.
data GtGlobalState = GtGlobalState
    { -- | Commitments are added during the first phase of epoch.
      _gsCommitments     :: !CommitmentsMap
      -- | Openings are added during the second phase of epoch.
    , _gsOpenings        :: !OpeningsMap
      -- | Decrypted shares to be used in the third phase.
    , _gsShares          :: !SharesMap
      -- | Vss certificates are added at any time if they are valid and
      -- received from stakeholders.
    , _gsVssCertificates :: !VCD.VssCertData
    } deriving (Eq, Show, Generic)

makeLenses ''GtGlobalState

instance Default GtGlobalState where
    def =
        GtGlobalState
        {
          _gsCommitments = mempty
        , _gsOpenings = mempty
        , _gsShares = mempty
        , _gsVssCertificates = VCD.empty
        }

instance Buildable GtGlobalState where
    build GtGlobalState {..} =
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
                (HM.keys $ getCommitmentsMap _gsCommitments)
        formatOpenings =
            formatIfNotNull
                ("  openings from: "%listJson%"\n")
                (HM.keys _gsOpenings)
        formatShares =
            formatIfNotNull
                ("  shares from: "%listJson%"\n")
                (HM.keys _gsShares)
        formatCertificates =
            formatIfNotNull
                ("  certificates from: "%listJson%"\n")
                (VCD.keys _gsVssCertificates)

data GtParams = GtParams
    { gtpSscEnabled :: !Bool              -- ^ Whether node should participate in SSC
                                          -- in case SSC requires participation.
    , gtpVssKeyPair :: !VssKeyPair        -- ^ Key pair used for secret sharing
    }

data GtContext = GtContext
    {
      -- | Vss key pair used for MPC.
      gtcVssKeyPair     :: !VssKeyPair
    , -- | Flag which determines whether we want to participate in SSC.
      gtcParticipateSsc :: !(TVar Bool)
    }

createGtContext :: MonadIO m => GtParams -> m GtContext
createGtContext GtParams {..} =
    GtContext gtpVssKeyPair <$> newTVarIO gtpSscEnabled

----------------------------------------------------------------------------
-- Secret storage
----------------------------------------------------------------------------

data GtSecretStorage = GtSecretStorage
    { -- | Our commitment.
      gssCommitment :: !SignedCommitment
    , -- | Corresponding opening
      gssOpening    :: !Opening
    , -- | Epoch for which this secret were generated
      gssEpoch      :: !EpochIndex
    } deriving (Show, Eq)
