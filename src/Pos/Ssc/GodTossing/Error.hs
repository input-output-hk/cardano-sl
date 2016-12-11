-- | Error which can arise during seed calculation.

module Pos.Ssc.GodTossing.Error
       ( SeedError (..)
       ) where

import           Data.Text.Buildable (Buildable (..))
import           Serokell.Util       (listBuilderJSON)
import           Universum

import           Pos.Types           (Address)

-- | Data type for error during seed calculation.
data SeedError
    -- | Some nodes in the 'OpeningsMap' aren't in the set of participants
    = ExtraneousOpenings (HashSet Address)
    -- | Some nodes in the 'SharesMap' aren't in the set of participants
    | ExtraneousShares (HashSet Address)
    -- | There were no participants so a random string couldn't be generated
    | NoParticipants
    -- | Commitment can't be deserialized or didn't match secret (either recovered or in openings)
    | BrokenCommitment Address
    -- | Secret couldn't be recovered, or wasn't found in either
    -- 'OpeningsMap' or 'SharesMap'
    | NoSecretFound Address
    -- | Secret can't be deserialized
    | BrokenSecret Address
    -- | Share can't be deserialized
    | BrokenShare Address
    deriving (Eq, Show)

instance Buildable SeedError where
    build (ExtraneousOpenings ks) =
        "ExtraneousOpenings " <> listBuilderJSON ks
    build (ExtraneousShares ks) =
        "ExtraneousShares " <> listBuilderJSON ks
    build NoParticipants =
        "NoParticipants"
    build (BrokenCommitment k) =
        "BrokenCommitment " <> build k
    build (NoSecretFound k) =
        "NoSecretFound " <> build k
    build (BrokenSecret k) =
        "BrokenSecret " <> build k
    build (BrokenShare k) =
        "BrokenShare " <> build k
