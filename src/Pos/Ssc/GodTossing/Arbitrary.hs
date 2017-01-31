{-# LANGUAGE UndecidableInstances #-}

-- | Arbitrary instances for GodTossing types.

module Pos.Ssc.GodTossing.Arbitrary
       ( CommitmentOpening (..)
       ) where

import qualified Data.HashMap.Strict              as HM
import           Test.QuickCheck                  (Arbitrary (..), elements, oneof)
import           Universum

import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Types.Relay    (DataMsg (..))
import           Pos.Crypto                       (deterministicVssKeyGen, toPublic,
                                                   toVssPublicKey)
import           Pos.Ssc.GodTossing.Core          (Commitment, CommitmentsMap,
                                                   GtPayload (..), GtProof (..), Opening,
                                                   VssCertificate (..),
                                                   genCommitmentAndOpening,
                                                   mkCommitmentsMap, mkVssCertificate)
import           Pos.Ssc.GodTossing.Type          ()
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), GtTag (..))
import           Pos.Ssc.GodTossing.Types.Types   (GtGlobalState (..),
                                                   GtSecretStorage (..))
import           Pos.Ssc.GodTossing.VssCertData   (VssCertData (..))
import           Pos.Types.Address                (addressHash)
import           Pos.Types.Arbitrary.Unsafe       ()
import           Pos.Util                         (asBinary)
import           Pos.Util.Arbitrary               (Nonrepeating (..), makeSmall, sublistN,
                                                   unsafeMakePool)

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

-- | Pair of 'Commitment' and 'Opening'.
data CommitmentOpening = CommitmentOpening
    { coCommitment :: !Commitment
    , coOpening    :: !Opening
    } deriving Show

-- | Generate 50 commitment/opening pairs in advance
-- (see `Pos.Crypto.Arbitrary` for explanations)
commitmentsAndOpenings :: [CommitmentOpening]
commitmentsAndOpenings =
    map (uncurry CommitmentOpening) $
    unsafeMakePool "[generating Commitments and Openings for tests...]" 50 $
       genCommitmentAndOpening 1 (one (asBinary vssPk))
  where
    vssPk = toVssPublicKey $ deterministicVssKeyGen "ababahalamaha"
{-# NOINLINE commitmentsAndOpenings #-}

instance Arbitrary CommitmentOpening where
    arbitrary = elements commitmentsAndOpenings

instance Nonrepeating CommitmentOpening where
    nonrepeating n = sublistN n commitmentsAndOpenings

instance Arbitrary Commitment where
    arbitrary = coCommitment <$> arbitrary

instance Arbitrary CommitmentsMap where
    arbitrary = mkCommitmentsMap <$> arbitrary

instance Arbitrary Opening where
    arbitrary = coOpening <$> arbitrary

instance Arbitrary VssCertificate where
    arbitrary = mkVssCertificate <$> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Gt (God Tossing) types
------------------------------------------------------------------------------------------

instance Arbitrary GtProof where
    arbitrary = oneof [
                        CommitmentsProof <$> arbitrary <*> arbitrary
                      , OpeningsProof <$> arbitrary <*> arbitrary
                      , SharesProof <$> arbitrary <*> arbitrary
                      , CertificatesProof <$> arbitrary
                      ]

instance Arbitrary GtPayload where
    arbitrary =
        makeSmall $
        oneof
            [ CommitmentsPayload <$> arbitrary <*> genVssCerts
            , OpeningsPayload <$> arbitrary <*> genVssCerts
            , SharesPayload <$> arbitrary <*> genVssCerts
            , CertificatesPayload <$> genVssCerts
            ]
      where
        genVssCerts = HM.fromList . map toCertPair <$> arbitrary
        toCertPair vc = (addressHash $ vcSigningKey vc, vc)

instance Arbitrary VssCertData where
    arbitrary = makeSmall $ VssCertData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary GtGlobalState where
    arbitrary = makeSmall $ GtGlobalState
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary GtSecretStorage where
    arbitrary = GtSecretStorage <$> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Message types
------------------------------------------------------------------------------------------

instance Arbitrary GtTag where
    arbitrary = oneof [ pure CommitmentMsg
                      , pure OpeningMsg
                      , pure SharesMsg
                      , pure VssCertificateMsg
                      ]

instance Arbitrary GtMsgContents where
    arbitrary = oneof [ MCCommitment <$> arbitrary
                      , MCOpening <$> arbitrary <*> arbitrary
                      , MCShares <$> arbitrary <*> arbitrary
                      , MCVssCertificate <$> arbitrary
                      ]

instance Arbitrary (DataMsg GtMsgContents) where
    arbitrary = do
        sk <- arbitrary
        dmContents <-
            oneof
                [ MCCommitment <$> ((toPublic sk,,) <$> arbitrary <*> arbitrary)
                , MCOpening <$> arbitrary <*> arbitrary
                , MCShares <$> arbitrary <*> arbitrary
                , MCVssCertificate <$>
                  (mkVssCertificate sk <$> arbitrary <*> arbitrary)
                ]
        return $ DataMsg {..}
