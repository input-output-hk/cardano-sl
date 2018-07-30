{-# LANGUAGE BangPatterns #-}

module Cardano.Wallet.Kernel.ChainState (
    -- * Chain state
    UpdateInfo(..)
  , ChainState(..)
    -- * Updates
  , getChainState
    -- * Getters
  , maxTxSize
  ) where

import           Universum

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Pos.Core.Common (Coin, ScriptVersion)
import           Pos.Core.Update (BlockVersion, BlockVersionData (..),
                     BlockVersionModifier (..), NumSoftwareVersion,
                     SoftwareVersion, UpdateProposal (..), svNumber)
import           Pos.DB.Update (getAdoptedBVData, getConfirmedProposals)
import           Pos.Update.Configuration (curSoftwareVersion)
import           Pos.Update.Poll.Types (ConfirmedProposalState (..),
                     StakeholderVotes, VoteState, isPositiveVote)
import           Serokell.Data.Memory.Units (Byte)

import           Cardano.Wallet.Kernel.MonadDBReadAdaptor

{-------------------------------------------------------------------------------
  Chain state state
-------------------------------------------------------------------------------}

data UpdateInfo = UpdateInfo {
      uiSoftwareVersion :: !SoftwareVersion
    , uiBlockVersion    :: !BlockVersion
    , uiScriptVersion   :: !ScriptVersion
    , uiImplicit        :: !Bool
    , uiVotesFor        :: !Int
    , uiVotesAgainst    :: !Int
    , uiPositiveStake   :: !Coin
    , uiNegativeStake   :: !Coin
    } deriving (Eq, Show)

data ChainState = ChainState {
      -- | Available update (if any)
      csUpdateInfo       :: !(Maybe UpdateInfo)

      -- | Block version data
    , csBlockVersionData :: !BlockVersionData
    }

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Get the latest chain state
getChainState :: MonadDBReadAdaptor IO -> IO ChainState
getChainState rocksDB = withMonadDBRead rocksDB $ do
    bvd       <- getAdoptedBVData
    proposals <- getConfirmedProposals (Just $ svNumber curSoftwareVersion)
    return ChainState{
        csBlockVersionData = bvd
      , csUpdateInfo       = fromProposals bvd proposals
      }

{-------------------------------------------------------------------------------
  Getters
-------------------------------------------------------------------------------}

-- | Maximum transaction size
maxTxSize :: ChainState -> Byte
maxTxSize = bvdMaxTxSize . csBlockVersionData

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

fromProposals :: BlockVersionData
              -> [ConfirmedProposalState]
              -> Maybe UpdateInfo
fromProposals _   []        = Nothing
fromProposals bvd proposals = Just
                            . fromProposal bvd
                            . maximumBy (comparing cpsToNumericVersion)
                            $ proposals
  where
    cpsToNumericVersion :: ConfirmedProposalState -> NumSoftwareVersion
    cpsToNumericVersion = svNumber . upSoftwareVersion . cpsUpdateProposal

-- | Construct 'UpdateInfo' from a 'ConfirmedProposalState'
--
-- NOTE: This is a straight-forward adaptation of 'toCUpdateInfo' in the
-- old wallet.
fromProposal :: BlockVersionData -> ConfirmedProposalState -> UpdateInfo
fromProposal bvd ConfirmedProposalState{..} = UpdateInfo {
      uiSoftwareVersion = upSoftwareVersion
    , uiBlockVersion    = upBlockVersion
    , uiScriptVersion   = fromMaybe (bvdScriptVersion bvd)
                                    (bvmScriptVersion upBlockVersionMod)
    , uiImplicit        = cpsImplicit
    , uiVotesFor        = votesFor
    , uiVotesAgainst    = votesAgainst
    , uiPositiveStake   = cpsPositiveStake
    , uiNegativeStake   = cpsNegativeStake
    }
  where
    UnsafeUpdateProposal {..} = cpsUpdateProposal
    (votesFor, votesAgainst)  = countVotes cpsVotes

-- | Return counts of negative and positive votes
countVotes :: StakeholderVotes -> (Int, Int)
countVotes = foldl' counter (0, 0)
  where
    counter :: (Int, Int) -> VoteState -> (Int, Int)
    counter (!n, !m) vote
      | isPositiveVote vote = (n + 1, m)
      | otherwise           = (n, m + 1)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable ChainState where
  build ChainState{..} = bprint
    ( "ChainState {"
    % "  updateInfo:       " % build
    % ", blockVersionData: " % build
    % "}"
    )
    csUpdateInfo
    csBlockVersionData

instance Buildable UpdateInfo where
  build UpdateInfo{..} = bprint
    ( "UpdateInfo {"
    % ", softwareVersion: " % build
    % ", blockVersion:    " % build
    % ", scriptVersion:   " % build
    % ", implicit:        " % build
    % ", votesFor:        " % build
    % ", votesAgainst:    " % build
    % ", positiveStake:   " % build
    % ", negativeStake:   " % build
    % "}"
    )
    uiSoftwareVersion
    uiBlockVersion
    uiScriptVersion
    uiImplicit
    uiVotesFor
    uiVotesAgainst
    uiPositiveStake
    uiNegativeStake
