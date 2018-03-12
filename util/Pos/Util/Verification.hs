{- |

Machinery to support pure datatype verification.

1. Datatypes can have and not have internal pure consistency. If
datatype doesn't have any implicit rules, it's called checked. 'Integer'
is checked, 'Either Text String' is checked too. Otherwise, it's called
unchecked. 'Coin' is unchecked, because it has implicit upper bound on the
value, which is lower than 'Word64' bound. Newtype over string that is
supposed to contain only strings of length less than 30 symbols is
unchecked.

2. Unchecked types' constructors should have prefix \"Unchecked\" (for
example, 'UncheckedProxySecretKey'). They should also have description
of why are they unchecked and provide some easy-to-use builders
(@mkX@) if needed.

3. If unchecked datatype @S@ is a part of @T@ (e.g. record field),
then @T@ is not called unchecked. In other words, "being unchecked"
property doesn't propagate, it is only related to the datatype itself.

4. @T@ is called verifiable if it is unchecked or any of its parts are
verifiable.

5. Every verifiable type must have 'PVerifiable' instance. See
the description of this class.


The advantage of it over an arbitrary set of verification functions:

* Forces programmer to think about verification in two levels (self +
  recursive)
* Unifies the standard about how to name validators; easier to call them.
* Forbids multiple verifiers for the same type
* Forces intermediate datatypes to be verified as well (not only
  sub-sub-children, but also children), so it's harder to miss something

The need to distinguish between pure and impure verification is coming
from the fact that sometimes we don't really need full gstate (when
sending updates, for example), but still want data to be
consistent. It is also much easier to determine the place to put the
check to if it's pure.
-}
module Pos.Util.Verification
    (
      PVer
    , pverFail

    , PVerifiable(..)
    , PVerifiableSub(..)
    , runPVerify
    , runPVerifyText
    , runPVerifyPanic
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Formatting (sformat)
import           Serokell.Util (VerificationRes (..), listJson, verResToMonadError)

-- Prefix P is annoying, yes.

-- | Pure verification monad. It accumulates errors along the
-- verification (AST) path.
newtype PVer a = PVer
    { getPVer :: Either (NonEmpty Text) a
    } deriving (Show, Eq, Functor, Applicative, Monad)

-- | Fail inside the 'PVer' monad.
pverFail :: Text -> PVer ()
pverFail t = PVer (Left $ one t)

-- | Verifies some field, prefixing with the text value in case of
-- error. Prefix is supposed to be the record field name. Use it when
-- you want to specify which field/component are you verifying.
pverField :: Text -> PVer () -> PVer ()
pverField p (PVer v) = PVer $ first (\x -> p `NE.cons` x) v

-- | Existential wrapper for "PVerifiable" subfield of the
-- datatype. It includes the datatype itself and field identifier.
data PVerifiableSub = forall x. PVerifiable x => PVerifiableSub Text x

-- | Things that can be (purely) verified.
class PVerifiable a where

    -- | Verify the 'a' predicates only, assuming that all subfields
    -- are correct. Is needed to be implemented for unsafe types only.
    pverifySelf :: a -> PVer ()

    -- | Retrieves all subfields that need to be verified.
    pverifyFields :: a -> [PVerifiableSub]

    -- pverifySelf can be omitted for safe types
    default pverifySelf :: a -> PVer ()
    pverifySelf = const pass

    -- For primitive types you just want to implement 'pverifySelf'.
    default pverifyFields :: a -> [PVerifiableSub]
    pverifyFields = const []

-- | Verify all subcomponents and the component itself. Basically,
-- call 'pverify' for subcomponents and 'pverifySelf' in the end.
pverify :: PVerifiable a => a -> PVer ()
pverify x = do
    forM_ (pverifyFields x) $ \(PVerifiableSub t f) -> pverField t $ pverify f
    pverifySelf x

-- | Run verification for the datatype.
runPVerify :: PVerifiable a => a -> VerificationRes
runPVerify =
    let convert = ("Pure verification failed: "<>) . T.intercalate "." . NE.toList
    in either (VerFailure . one . convert) (const VerSuccess) . getPVer . pverify

-- | Like 'runPVerify', but converts error to text.
runPVerifyText :: PVerifiable a => a -> Maybe Text
runPVerifyText = leftToMaybe . verResToMonadError (sformat listJson) . runPVerify

-- | 'runPVerify' that panics on error.
runPVerifyPanic :: PVerifiable a => Text -> a -> a
runPVerifyPanic expl a =
    case runPVerify a of
        VerSuccess   -> a
        VerFailure e -> error $ "runPVerifyError: " <> expl <> " " <> show e
