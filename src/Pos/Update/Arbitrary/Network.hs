{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System networking types.

module Pos.Update.Arbitrary.Network
       (
       ) where

import           Data.DeriveTH             (derive, makeArbitrary)
import qualified Data.HashMap.Strict       as HM
import           Test.QuickCheck           (Arbitrary (..), listOf, suchThat, vector)
import           Universum

import           Pos.Binary.Update         ()
import           Pos.Communication.Limits  (MaxSize (..), appNameLenLimit, upDataNumLimit,
                                            updateVoteNumLimit)
import           Pos.Communication.Relay   (DataMsg (..))
import           Pos.Crypto                (hash, sign, toPublic)
import           Pos.Data.Attributes       (mkAttributes)
import           Pos.Types.Arbitrary       ()
import           Pos.Types.Core            (ApplicationName (..), SoftwareVersion (..))
import           Pos.Update.Arbitrary.Core ()
import           Pos.Update.Core.Types     (UpdateProposal (..), UpdateVote (..))
import           Pos.Update.Network.Types  (ProposalMsgTag (..), VoteMsgTag (..))

derive makeArbitrary ''ProposalMsgTag
derive makeArbitrary ''VoteMsgTag

instance Arbitrary (DataMsg UpdateVote) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary (DataMsg (UpdateProposal, [UpdateVote])) where
    arbitrary = do
        up <- arbitrary
        let id = hash up
            genVote = do
                sk <- arbitrary
                let pk = toPublic sk
                decision <- arbitrary
                pure $ UpdateVote pk id decision $ sign sk (id, decision)
        votes <- listOf genVote
        pure $ DataMsg (up, votes)

instance Arbitrary (MaxSize ApplicationName) where
    arbitrary =
        MaxSize . ApplicationName . fromString <$> vector appNameLenLimit

instance Arbitrary (MaxSize SoftwareVersion) where
    arbitrary = MaxSize <$>
        (SoftwareVersion <$> (getOfMaxSize <$> arbitrary) <*> arbitrary)

instance Arbitrary (MaxSize UpdateProposal) where
    arbitrary = MaxSize <$>
        (UpdateProposal <$> arbitrary <*> arbitrary
                        <*> (getOfMaxSize <$> arbitrary)
                        <*> (HM.fromList <$> vector upDataNumLimit
                                                `suchThat` (not . null))
                        <*> pure (mkAttributes ()))

instance Arbitrary (MaxSize (DataMsg (UpdateProposal, [UpdateVote]))) where
    arbitrary =
        -- we don't care about sensibility
        MaxSize . DataMsg <$> ((,) <$> (getOfMaxSize <$> arbitrary)
                                   <*> vector updateVoteNumLimit)
