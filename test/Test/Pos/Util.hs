{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
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
import           Data.Typeable         (typeOf)
import           Pos.Binary            (Bi, decode, encode)
import           Pos.Util              (AsBinaryClass (..))
import           Prelude               (read)
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

class BinaryTestable a where
    binaryTest :: Spec

class SafeCopyTestable a where
    safeCopyTest :: Spec

class SerDeserTestable a where
    serDeserTest :: Spec

class ReadShowTestable a where
    showReadTest :: Spec

typeName :: forall a. Typeable a => [Char]
typeName = show $ typeOf @a undefined

instance (Bi a, Arbitrary a, Show a, Eq a, Typeable a) => BinaryTestable a where
    binaryTest = prop (typeName @a) (binaryEncodeDecode @a)

instance (SafeCopy a, Arbitrary a, Show a, Eq a, Typeable a) => SafeCopyTestable a where
    safeCopyTest = prop (typeName @a) (safeCopyEncodeDecode @a)

instance (AsBinaryClass a, Arbitrary a, Show a, Eq a, Typeable a) =>
    SerDeserTestable a where
    serDeserTest = prop (typeName @a) (serDeserId @a)

instance (Read a, Arbitrary a, Show a, Eq a, Typeable a) => ReadShowTestable a where
    showReadTest = prop (typeName @a) (showRead @a)
