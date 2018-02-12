{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Test.Pos.Helpers
       (
       -- * From/to
         binaryEncodeDecode
       , binaryTest
       , safeCopyEncodeDecode
       , safeCopyTest
       , serDeserId
       , showReadTest
       , canonicalJsonTest

       -- * Message length
       , msgLenLimitedTest
       ) where

import           Universum

import           Codec.CBOR.FlatTerm (toFlatTerm, validFlatTerm)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Identity (Identity (..))
import           Data.SafeCopy (SafeCopy, safeGet, safePut)
import           Data.Serialize (runGet, runPut)
import           Data.Typeable (typeRep)
import           Formatting (formatToString, int, (%))
import           Prelude (read)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (arbitrary), Property, conjoin, counterexample, forAll,
                                  property, resize, suchThat, vectorOf, (.&&.), (===))
import qualified Text.JSON.Canonical as CanonicalJSON

import           Pos.Core.Genesis (SchemaError)
import           Pos.Binary (AsBinaryClass (..), Bi (..), decodeFull, serialize, serialize',
                             unsafeDeserialize)
import           Pos.Communication (Limit (..), MessageLimited (..))
import           Pos.Util.QuickCheck.Arbitrary (SmallGenerator (..))
import           Test.Pos.Cbor.Canonicity (perturbCanonicity)
import qualified Test.Pos.Cbor.RefImpl as R

----------------------------------------------------------------------------
-- From/to tests
----------------------------------------------------------------------------

-- | Basic binary serialization/deserialization identity.
binaryEncodeDecode :: (Show a, Eq a, Bi a) => a -> Property
binaryEncodeDecode a = (unsafeDeserialize . serialize $ a) === a

-- | Machinery to test we perform "flat" encoding.
cborFlatTermValid :: (Show a, Bi a) => a -> Property
cborFlatTermValid = property . validFlatTerm . toFlatTerm . encode

-- Test that serialized 'a' has canonical representation, i.e. if we're able to
-- change its serialized form, it won't be successfully deserialized.
cborCanonicalRep :: forall a. (Bi a, Show a) => a -> Property
cborCanonicalRep a = counterexample (show a) . property $ do
    let sa = serialize a
    sa' <- R.serialise <$> perturbCanonicity (R.deserialise sa)
    pure $ sa == sa' || isLeft (decodeFull @a $ BSL.toStrict sa')

safeCopyEncodeDecode :: (Show a, Eq a, SafeCopy a) => a -> Property
safeCopyEncodeDecode a =
    either (error . toText) identity
        (runGet safeGet $ runPut $ safePut a) === a

serDeserId :: forall t . (Show t, Eq t, AsBinaryClass t) => t -> Property
serDeserId a =
    either (error . toText) identity
        (fromBinary $ asBinary @t a) === a

showReadId :: (Show a, Eq a, Read a) => a -> Property
showReadId a = read (show a) === a

type IdTestingRequiredClassesAlmost a = (Eq a, Show a, Arbitrary a, Typeable a)

type IdTestingRequiredClasses f a = (Eq a, Show a, Arbitrary a, Typeable a, f a)

identityTest :: forall a. (IdTestingRequiredClassesAlmost a) => (a -> Property) -> Spec
identityTest fun = prop (typeName @a) fun
  where
    typeName :: forall x. Typeable x => String
    typeName = show $ typeRep (Proxy @a)

binaryTest :: forall a. IdTestingRequiredClasses Bi a => Spec
binaryTest =
    identityTest @a $ \x -> binaryEncodeDecode x
                       .&&. cborFlatTermValid x
                       .&&. cborCanonicalRep x

safeCopyTest :: forall a. IdTestingRequiredClasses SafeCopy a => Spec
safeCopyTest = identityTest @a safeCopyEncodeDecode

showReadTest :: forall a. IdTestingRequiredClasses Read a => Spec
showReadTest = identityTest @a showReadId


type ToAndFromCanonicalJson a
     = ( CanonicalJSON.ToJSON Identity a
       , CanonicalJSON.FromJSON (Either SchemaError) a
       )

canonicalJsonTest ::
       forall a. (IdTestingRequiredClassesAlmost a, ToAndFromCanonicalJson a)
    => Spec
canonicalJsonTest =
    identityTest @a $ \x ->
        canonicalJsonRenderAndDecode x .&&. canonicalJsonPrettyAndDecode x
  where
    canonicalJsonRenderAndDecode x =
        let encodedX =
                CanonicalJSON.renderCanonicalJSON $
                runIdentity $ CanonicalJSON.toJSON x
        in canonicalJsonDecodeAndCompare x encodedX
    canonicalJsonPrettyAndDecode x =
        let encodedX =
                encodeUtf8 $
                CanonicalJSON.prettyCanonicalJSON $
                runIdentity $ CanonicalJSON.toJSON x
        in canonicalJsonDecodeAndCompare x encodedX
    canonicalJsonDecodeAndCompare ::
           CanonicalJSON.FromJSON (Either SchemaError) a
        => a
        -> LByteString
        -> Property
    canonicalJsonDecodeAndCompare x encodedX =
        let decodedValue =
                either (error . toText) identity $
                CanonicalJSON.parseCanonicalJSON encodedX
            decodedX =
                either (error . pretty @SchemaError) identity $
                CanonicalJSON.fromJSON decodedValue
        in decodedX === x

----------------------------------------------------------------------------
-- Message length
----------------------------------------------------------------------------

msgLenLimitedCheck
    :: (Show a, Bi a) => Limit a -> a -> Property
msgLenLimitedCheck limit msg =
    let sz = BS.length . serialize' $ msg
    in if sz <= fromIntegral limit
        then property True
        else flip counterexample False $
            formatToString ("Message size (max found "%int%") exceedes \
            \limit ("%int%")") sz limit

msgLenLimitedTest'
    :: forall a. IdTestingRequiredClasses Bi a
    => Limit a -> String -> (a -> Bool) -> Spec
msgLenLimitedTest' limit desc whetherTest =
    -- instead of checking for `arbitrary` values, we'd better generate
    -- many values and find maximal message size - it allows user to get
    -- correct limit on the spot, if needed.
    addDesc $
        modifyMaxSuccess (const 1) $
            identityTest @a $ \_ -> findLargestCheck .&&. listsCheck
  where
    addDesc act = if null desc then act else describe desc act

    genNice = arbitrary `suchThat` whetherTest

    findLargestCheck =
        forAll (resize 1 $ vectorOf 50 genNice) $
            \samples -> counterexample desc $ msgLenLimitedCheck limit $
                maximumBy (comparing $ BS.length . serialize') samples

    -- In this test we increase length of lists, maps, etc. generated
    -- by `arbitrary` (by default lists sizes are bounded by 100).
    --
    -- Motivation: if your structure contains lists, you should ensure
    -- their lengths are limited in practise. If you did, use `MaxSize`
    -- wrapper to generate `arbitrary` objects of that type with lists of
    -- exactly maximal possible size.
    listsCheck =
        let doCheck power = forAll (resize (2 ^ power) genNice) $
                \a -> counterexample desc $
                    counterexample "Potentially unlimited size!" $
                        msgLenLimitedCheck limit a
        -- Increase lists length gradually to avoid hanging.
        in  conjoin $ doCheck <$> [1..13 :: Int]

msgLenLimitedTest
    :: forall a. (IdTestingRequiredClasses Bi a, MessageLimited a Identity)
    => Spec
msgLenLimitedTest = msgLenLimitedTest' @a (runIdentity (getMsgLenLimit Proxy)) "" (const True)

----------------------------------------------------------------------------
-- Orphans
----------------------------------------------------------------------------

deriving instance Bi bi => Bi (SmallGenerator bi)
