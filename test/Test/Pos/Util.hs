{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Pos.Util
       ( binaryEncodeDecode
       , networkBinaryEncodeDecode
       , binaryTest
       , networkBinaryTest
       , safeCopyEncodeDecode
       , safeCopyTest
       , serDeserId
       , serDeserTest
       , showRead
       , showReadTest
       ) where

import           Data.Binary.Get       (isEmpty, runGetIncremental, Decoder (..))
import qualified Data.Binary.Get       as Bin
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.SafeCopy         (SafeCopy, safeGet, safePut)
import           Data.Serialize        (runGet, runPut)
import           Data.Typeable         (typeRep)
import           Test.QuickCheck       (counterexample)
import           Prelude               (read)

import           Pos.Binary            (Bi (..), encode)
import           Pos.Util              (AsBinaryClass (..))

import           Test.Hspec            (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, Property, (===))
import           Universum

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
        stage2 $ continue $ Just (LBS.toStrict $ encode a)

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
    stage3 (Done _ _ _) =
        failText "Parser tried to check content out of given bytestring\
            \ - this is not allowed"
    stage3 (Fail _ _ why) =
        failText $ "parse error: " ++ why
    stage3 (Partial _) =
        failText "Parser required extra input"

safeCopyEncodeDecode :: (Show a, Eq a, SafeCopy a) => a -> Property
safeCopyEncodeDecode a =
    either (panic . toText) identity
        (runGet safeGet $ runPut $ safePut a) === a

showRead :: (Show a, Eq a, Read a) => a -> Property
showRead a = read (show a) === a

serDeserId :: forall t . (Show t, Eq t, AsBinaryClass t) => t -> Property
serDeserId a =
    either (panic . toText) identity
        (fromBinary $ asBinary @t a) ===  a

typeName :: forall a. Typeable a => [Char]
typeName = show $ typeRep (Proxy @a)

type IdTestingRequiredClasses f a = (Eq a, Show a, Arbitrary a, Typeable a, f a)

identityTest :: forall f a. (IdTestingRequiredClasses f a) => (a -> Property) -> Spec
identityTest fun = prop (typeName @a) fun

binaryTest :: forall a. IdTestingRequiredClasses Bi a => Spec
binaryTest = identityTest @Bi @a binaryEncodeDecode

networkBinaryTest :: forall a. IdTestingRequiredClasses Bi a => Spec
networkBinaryTest = identityTest @Bi @a networkBinaryEncodeDecode

safeCopyTest :: forall a. IdTestingRequiredClasses SafeCopy a => Spec
safeCopyTest = identityTest @SafeCopy @a safeCopyEncodeDecode

serDeserTest :: forall a. IdTestingRequiredClasses AsBinaryClass a => Spec
serDeserTest = identityTest @AsBinaryClass @a serDeserId

showReadTest :: forall a. IdTestingRequiredClasses Read a => Spec
showReadTest = identityTest @Read @a showRead
