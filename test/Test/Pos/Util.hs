{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Pos.Util
       ( binaryEncodeDecode
       , binaryTest
       , safeCopyEncodeDecode
       , safeCopyTest
       , serDeserId
       , serDeserTest
       , showRead
       , showReadTest
       ) where

import           Data.SafeCopy         (SafeCopy, safeGet, safePut)
import           Data.Serialize        (runGet, runPut)
import           Data.Typeable         (typeRep)
import           Prelude               (read)

import           Pos.Binary            (Bi, decode, encode)
import           Pos.Util              (AsBinaryClass (..))

import           Test.Hspec            (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, Property, (===))
import           Universum

binaryEncodeDecode :: (Show a, Eq a, Bi a) => a -> Property
binaryEncodeDecode a = decode (encode a) === a

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

safeCopyTest :: forall a. IdTestingRequiredClasses SafeCopy a => Spec
safeCopyTest = identityTest @SafeCopy @a safeCopyEncodeDecode

serDeserTest :: forall a. IdTestingRequiredClasses AsBinaryClass a => Spec
serDeserTest = identityTest @AsBinaryClass @a serDeserId

showReadTest :: forall a. IdTestingRequiredClasses Read a => Spec
showReadTest = identityTest @Read @a showRead
