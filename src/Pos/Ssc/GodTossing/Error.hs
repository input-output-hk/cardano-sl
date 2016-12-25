-- | Error which can arise during seed calculation.

module Pos.Ssc.GodTossing.Error
       ( SeedError (..)
       ) where

import           Data.Text.Buildable (Buildable (..))
import           Serokell.Util       (listBuilderJSON)
import           Universum

import           Pos.Crypto          (PublicKey)
import           Pos.Types.Address   (AddressHash)

-- | Data type for error during seed calculation.
data SeedError
    -- | Some nodes in the 'OpeningsMap' aren't in the set of participants
    = ExtraneousOpenings (HashSet (AddressHash PublicKey))
    -- | Some nodes in the 'SharesMap' aren't in the set of participants
    | ExtraneousShares (HashSet (AddressHash PublicKey))
    -- | There were no participants so a random string couldn't be generated
    | NoParticipants
    -- | Commitment can't be deserialized or didn't match secret (either recovered or in openings)
    | BrokenCommitment (AddressHash PublicKey)
    -- | Secret couldn't be recovered, or wasn't found in either
    -- 'OpeningsMap' or 'SharesMap'
    | NoSecretFound (AddressHash PublicKey)
    -- | Secret can't be deserialized
    | BrokenSecret (AddressHash PublicKey)
    -- | Share can't be deserialized
    | BrokenShare (AddressHash PublicKey)
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
