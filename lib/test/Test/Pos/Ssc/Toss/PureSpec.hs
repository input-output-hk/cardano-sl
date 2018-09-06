-- | Specification for Pos.Ssc.GodTossing.Toss.Pure

module Test.Pos.Ssc.Toss.PureSpec
       ( spec
       ) where

import           Universum

import qualified Crypto.Random as Rand
import           Data.Default (def)
import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, arbitrary, forAll, generate,
                                  listOf, suchThat, (===))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Ssc ()
import           Pos.Core (EpochOrSlot, HasConfiguration, InnerSharesMap, Opening, SignedCommitment,
                           StakeholderId, VssCertificate (..), addressHash)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import qualified Pos.Ssc.Toss.Class as Toss
import qualified Pos.Ssc.Toss.Pure as Toss
import qualified Pos.Ssc.Types as Toss

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Core.Arbitrary ()

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ do
    let smaller n = modifyMaxSuccess (const n)
    describe "PureToss" $ smaller 30 $ do
        prop "Adding and deleting a signed commitment in the 'PureToss' monad is the\
             \ same as doing nothing"
             putDelCommitment
        prop "Adding and deleting an opening in the 'PureToss' monad is the same as doing\
             \ nothing"
             putDelOpening
        prop "Adding and deleting a share in the 'PureToss' monad is the same as doing\
             \ nothing"
             putDelShare

data TossAction
    = PutCommitment SignedCommitment
    | PutOpening StakeholderId Opening
    | PutShares StakeholderId InnerSharesMap
    | PutCertificate VssCertificate
    | ResetCO
    | ResetShares
    | DelCommitment StakeholderId
    | DelOpening StakeholderId
    | DelShares StakeholderId
    | SetEpochOrSlot EpochOrSlot
    deriving (Show, Eq, Generic)

instance HasConfiguration => Arbitrary TossAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

actionToMonad :: Toss.MonadToss m => TossAction -> m ()
actionToMonad (PutCommitment sc)   = Toss.putCommitment sc
actionToMonad (PutOpening sid o)   = Toss.putOpening sid o
actionToMonad (PutShares sid ism)  = Toss.putShares sid ism
actionToMonad (PutCertificate v)   = Toss.putCertificate v
actionToMonad ResetCO              = Toss.resetCO
actionToMonad ResetShares          = Toss.resetShares
actionToMonad (DelCommitment sid)  = Toss.delCommitment sid
actionToMonad (DelOpening sid)     = Toss.delOpening sid
actionToMonad (DelShares sid)      = Toss.delShares sid
actionToMonad (SetEpochOrSlot eos) = Toss.setEpochOrSlot eos

emptyTossSt :: Toss.SscGlobalState
emptyTossSt = def

perform :: HasConfiguration => [TossAction] -> Toss.PureToss ()
perform = mapM_ actionToMonad

-- | Type synonym used for convenience. This quintuple is used to pass the randomness
-- needed to run 'PureToss' actions to the testing property.
type TossTestInfo = (Word64, Word64, Word64, Word64, Word64)

-- | Operational equivalence operator in the 'PureToss' monad. To be used when
-- equivalence between two sequences of actions in 'PureToss' is to be tested/proved.
(==^)
    :: HasConfiguration
    => [TossAction]
    -> [TossAction]
    -> Gen TossAction
    -> TossTestInfo
    -> Property
t1 ==^ t2 = \prefixGen ttInfo ->
    forAll ((listOf prefixGen) :: Gen [TossAction]) $ \prefix ->
    forAll (arbitrary :: Gen [TossAction]) $ \suffix ->
        let applyAction x =
                view _2 .
                fst . Rand.withDRG (Rand.drgNewTest ttInfo) .
                Toss.runPureToss emptyTossSt $ (perform $ prefix ++ x ++ suffix)
        in applyAction t1 === applyAction t2

{- A note on the following tests
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The reason these tests have to pass a custom generator for the prefix of the action list
to '(==^)' is that in each case, there is a particular sequence of actions for which
the property does not hold. Using one of the following tests as an example:

Let 'o, o´ :: Opening' such that 'o /= o´'. This sequence of actions
in the 'PureToss' monad:

    [PutOpening sid o´, PutOpening sid o, DelOpening sid]

is not, in operational semantics terms, equal to the sequence

    [PutOpening sid o´]

It is instead equivalent to

    []

Because these actions are performed from left to right, performing an insertion with the
same key several times without deleting it in between those insertions means only the
last insertion actually matters for these tests.

As such, prefixes with an insertion with the same key as the action being tested in the
property will cause it to fail.
-}

putDelCommitment :: HasConfiguration => SignedCommitment -> TossTestInfo -> Property
putDelCommitment sc =
    let actionPrefixGen = arbitrary `suchThat` (\case
            PutCommitment sc' -> sc ^. _1 /= sc'^. _1
            _                 -> True)
    in ([PutCommitment sc, DelCommitment $ addressHash $ sc ^. _1] ==^ []) actionPrefixGen

putDelOpening
    :: HasConfiguration
    => StakeholderId
    -> Opening
    -> TossTestInfo
    -> Property
putDelOpening sid o =
    let actionPrefixGen = arbitrary `suchThat` (\case
            PutOpening sid' _ -> sid /= sid'
            _                 -> True)
    in ([PutOpening sid o, DelOpening sid] ==^ []) actionPrefixGen

putDelShare
    :: HasConfiguration
    => StakeholderId
    -> InnerSharesMap
    -> TossTestInfo
    -> Property
putDelShare sid ism =
    let actionPrefixGen = arbitrary `suchThat` (\case
            PutShares sid' _ -> sid' /= sid
            _                -> True)
    in ([PutShares sid ism, DelShares sid] ==^ []) actionPrefixGen
