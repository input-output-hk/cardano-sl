-- | Error which can arise during seed calculation.

module Pos.Ssc.GodTossing.Error
       ( SeedError (..)
       ) where

import           Universum

import           Data.Text.Buildable (Buildable (..))
import           Serokell.Util       (listBuilderJSON)

import           Pos.Core.Types      (Coin, StakeholderId)

-- | Data type for error during seed calculation.
data SeedError
    -- | Some nodes in the 'OpeningsMap' aren't in the set of participants
    = ExtraOpenings !(HashSet StakeholderId)
    -- | Some nodes in the 'SharesMap' aren't in the set of participants
    | ExtraShares !(HashSet StakeholderId)
    -- | Some participants aren't richmen
    | NonRichmenParticipants !(HashSet StakeholderId)

    -- | There was no majority of stake participating
    -- (first parameter – participating stake, second – total richmen stake)
    | NotEnoughParticipatingStake !Coin !Coin
    -- | There were no good secrets so a seed couldn't be generated
    | NoSecrets

    -- | Commitment can't be deserialized or didn't match secret (either
    -- recovered or in openings)
    | BrokenCommitment !StakeholderId
    -- | Secret couldn't be recovered, or wasn't found in either
    -- 'OpeningsMap' or 'SharesMap'
    | NoSecretFound !StakeholderId
    -- | Secret can't be deserialized
    | BrokenSecret !StakeholderId
    -- | Share can't be deserialized
    | BrokenShare !StakeholderId

    -- | Some errors during computation of commitment distribution
    | CommitmentDistrError !Text
    deriving (Eq, Show)

instance Buildable SeedError where
    build (ExtraOpenings ks) =
        "ExtraOpenings " <> listBuilderJSON ks
    build (ExtraShares ks) =
        "ExtraShares " <> listBuilderJSON ks
    build (NonRichmenParticipants ks) =
        "NonRichmenParticipants " <> listBuilderJSON ks
    build (NotEnoughParticipatingStake c s) =
        "NotEnoughParticipatingStake " <> build c <> " " <> build s
    build NoSecrets =
        "NoSecrets"
    build (BrokenCommitment k) =
        "BrokenCommitment " <> build k
    build (NoSecretFound k) =
        "NoSecretFound " <> build k
    build (BrokenSecret k) =
        "BrokenSecret " <> build k
    build (BrokenShare k) =
        "BrokenShare " <> build k
    build (CommitmentDistrError reason) =
        build ("CommitmentDistrError " <> reason)
