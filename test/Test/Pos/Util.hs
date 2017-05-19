{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Util
       ( binaryEncodeDecode
       , networkBinaryEncodeDecode
       , binaryTest
       , formsCommutativeMonoid
       , formsMonoid
       , formsSemigroup
       , networkBinaryTest
       , msgLenLimitedTest
       , msgLenLimitedTest'
       , safeCopyEncodeDecode
       , safeCopyTest
       , serDeserId
       , serDeserTest
       , shouldThrowException
       , showRead
       , showReadTest
       , (.=.)
       , (>=.)
       ) where

import           Data.Binary.Get       (Decoder (..), isEmpty, runGetIncremental)
import qualified Data.Binary.Get       as Bin
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.SafeCopy         (SafeCopy, safeGet, safePut)
import qualified Data.Semigroup        as Semigroup
import           Data.Serialize        (runGet, runPut)
import           Data.Tagged           (Tagged (..))
import           Data.Typeable         (typeRep)
import           Formatting            (formatToString, int, (%))
import           Prelude               (read)

import           Pos.Binary            (AsBinaryClass (..), Bi (..), encode, encodeStrict)
import           Pos.Communication     (Limit (..), MessageLimitedPure (..))

import           Test.Hspec            (Expectation, Selector, Spec, describe,
                                        shouldThrow)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), Property, conjoin,
                                        counterexample, forAll, property, resize,
                                        suchThat, vectorOf, (.&&.), (===))

import           Universum

instance Arbitrary a => Arbitrary (Tagged s a) where
    arbitrary = Tagged <$> arbitrary

binaryEncodeDecode :: (Show a, Eq a, Bi a) => a -> Property
binaryEncodeDecode a = Bin.runGet parser (encode a) === a
  where
    parser = get <* unlessM isEmpty (fail "Unconsumed input")

-- | This check is indended to be used for all messages sent via
-- networking.
-- Except correctness, it also checks that parser requires exactly
-- needed amount of data to be parsed, without knowing anything about
-- what's going next, or whether is it going at all.
-- So, using e.g. `lookAhead` at the end of given bytestring would lead
-- to error.
networkBinaryEncodeDecode :: (Show a, Eq a, Bi a) => a -> Property
networkBinaryEncodeDecode a = stage1 $ runGetIncremental get
  where
    failText why = counterexample why False

    -- nothing has been put yet
    stage1 (Done remaining _ _) =
        if BS.null remaining
        then failText
             "Serializes to \"\", networking may not work with such data"
        else failText "Unconsumed input"
    stage1 (Fail _ _ why)    =
        failText $ "parse error: " ++ why
    stage1 (Partial continue)   =
        stage2 $ continue $ Just (encodeStrict a)

    -- all data has been put
    stage2 (Done remaining _ b) =
        if BS.null remaining
        then a === b          -- the only nice outcome
        else failText "Unconsumed input"
    stage2 (Fail _ _ why) =
        failText $ "parse error: " ++ why
    stage2 (Partial continue) =
        stage3 $ continue Nothing

    -- all data consumed, but parser wants more input
    stage3 (Done {}) =
        failText "Parser tried to check, at end of the input, \
            \whether input ends - this is not allowed"
    stage3 (Fail _ _ why) =
        failText $ "parse error: " ++ why
    stage3 (Partial _) =
        failText "Parser required extra input"

msgLenLimitedCheck
    :: (Show a, Bi a) => Limit a -> a -> Property
msgLenLimitedCheck limit msg =
    let size = LBS.length . encode $ msg
    in if size <= fromIntegral limit
        then property True
        else flip counterexample False $
            formatToString ("Message size (max found "%int%") exceedes \
            \limit ("%int%")") size limit

safeCopyEncodeDecode :: (Show a, Eq a, SafeCopy a) => a -> Property
safeCopyEncodeDecode a =
    either (error . toText) identity
        (runGet safeGet $ runPut $ safePut a) === a

showRead :: (Show a, Eq a, Read a) => a -> Property
showRead a = read (show a) === a

serDeserId :: forall t . (Show t, Eq t, AsBinaryClass t) => t -> Property
serDeserId a =
    either (error . toText) identity
        (fromBinary $ asBinary @t a) ===  a

typeName :: forall a. Typeable a => String
typeName = show $ typeRep (Proxy @a)

type IdTestingRequiredClasses f a = (Eq a, Show a, Arbitrary a, Typeable a, f a)

identityTest :: forall f a. (IdTestingRequiredClasses f a) => (a -> Property) -> Spec
identityTest fun = prop (typeName @a) fun

binaryTest :: forall a. IdTestingRequiredClasses Bi a => Spec
binaryTest = identityTest @Bi @a binaryEncodeDecode

networkBinaryTest :: forall a. IdTestingRequiredClasses Bi a => Spec
networkBinaryTest = identityTest @Bi @a networkBinaryEncodeDecode

msgLenLimitedTest'
    :: forall a. IdTestingRequiredClasses Bi a
    => Limit a -> String -> (a -> Bool) -> Spec
msgLenLimitedTest' limit desc whetherTest =
    -- instead of checking for `arbitrary` values, we'd better generate
    -- many values and find maximal message size - it allows user to get
    -- correct limit on the spot, if needed.
    addDesc $
        modifyMaxSuccess (const 1) $
            identityTest @Bi @a $ \_ -> findLargestCheck .&&. listsCheck
  where
    addDesc act = if null desc then act else describe desc act

    genNice = arbitrary `suchThat` whetherTest

    findLargestCheck =
        forAll (resize 1 $ vectorOf 50 genNice) $
            \samples -> counterexample desc $ msgLenLimitedCheck limit $
                maximumBy (comparing $ LBS.length . encode) samples

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
    :: forall a. (IdTestingRequiredClasses Bi a, MessageLimitedPure a)
    => Spec
msgLenLimitedTest = msgLenLimitedTest' @a msgLenLimit "" (const True)

safeCopyTest :: forall a. IdTestingRequiredClasses SafeCopy a => Spec
safeCopyTest = identityTest @SafeCopy @a safeCopyEncodeDecode

serDeserTest :: forall a. IdTestingRequiredClasses AsBinaryClass a => Spec
serDeserTest = identityTest @AsBinaryClass @a serDeserId

showReadTest :: forall a. IdTestingRequiredClasses Read a => Spec
showReadTest = identityTest @Read @a showRead

-- | Extensional equality combinator. Useful to express function properties as functional
-- equations.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
(.=.) f g a = f a === g a

infixr 5 .=.

-- | Monadic extensional equality combinator.
(>=.)
    :: (Show (m b), Arbitrary a, Monad m, Eq (m b))
    => (a -> m b)
    -> (a -> m b)
    -> a
    -> Property
(>=.) f g a = f a === g a

infixr 5 >=.

shouldThrowException
    :: (Show a, Eq a, Exception e)
    => (a -> b)
    -> Selector e
    -> a
    -> Expectation
shouldThrowException action exception arg =
    (return $! action arg) `shouldThrow` exception

isAssociative :: (Show m, Eq m, Semigroup m) => m -> m -> m -> Property
isAssociative m1 m2 m3 =
    let assoc1 = (m1 Semigroup.<> m2) Semigroup.<> m3
        assoc2 = m1 Semigroup.<> (m2 Semigroup.<> m3)
    in assoc1 === assoc2

formsSemigroup :: (Show m, Eq m, Semigroup m) => m -> m -> m -> Property
formsSemigroup = isAssociative

hasIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Property
hasIdentity m =
    let id1 = mempty Semigroup.<> m
        id2 = m Semigroup.<> mempty
    in (m == id1) .&&. (m == id2)

formsMonoid :: (Show m, Eq m, Semigroup m, Monoid m) => m -> m -> m -> Property
formsMonoid m1 m2 m3 =
    (formsSemigroup m1 m2 m3) .&&. (hasIdentity m1)

isCommutative :: (Show m, Eq m, Semigroup m, Monoid m) => m -> m -> Property
isCommutative m1 m2 =
    let comm1 = m1 <> m2
        comm2 = m2 <> m1
    in comm1 === comm2

formsCommutativeMonoid :: (Show m, Eq m, Semigroup m, Monoid m) => m -> m -> m -> Property
formsCommutativeMonoid m1 m2 m3 =
    (formsMonoid m1 m2 m3) .&&. (isCommutative m1 m2)
