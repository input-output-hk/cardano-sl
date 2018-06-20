{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Pos.Binary.BiSerialize
    ( tests
    ) where

import           Universum

import           Hedgehog (Gen, Property, discover, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Binary.Class (Cons (..), Field (..), cborError, deriveIndexedBi,
                                   deriveSimpleBi, serialize')

import qualified Serokell.Util.Base16 as B16


--------------------------------------------------------------------------------

data TestSimple
    = TsInt
        { unTsInt :: Int }
    | TsIntList
        { unTsIntList :: [Int] }
    | TsChar2
        { unTsChar2L :: Char
        , unTsChar2R :: Char }
    | TsInteger
        { unTsInteger :: Integer }
    | TsMaybeInt
        { unTsMaybeInt :: Maybe Int }
    | TsChar2Permuted
        { unTsChar2PermutedL :: Char
        , unTsChar2PermutedR :: Char }
    | TsPair
        { unTsPairL :: TestSimple
        , unTsPairR :: TestSimple }
    deriving (Eq, Show, Typeable)

deriveSimpleBi ''TestSimple [
    Cons 'TsInt [
        Field [| unTsInt            :: Int        |]
        ],
    Cons 'TsIntList [
        Field [| unTsIntList        :: [Int]      |]
        ],
    Cons 'TsChar2 [
        Field [| unTsChar2L         :: Char       |],
        Field [| unTsChar2R         :: Char       |]
        ],
    Cons 'TsInteger [
        Field [| unTsInteger        :: Integer    |]
        ],
    Cons 'TsMaybeInt [
        Field [| unTsMaybeInt       :: Maybe Int  |]
        ],
    Cons 'TsChar2Permuted [
        Field [| unTsChar2PermutedR :: Char       |],
        Field [| unTsChar2PermutedL :: Char       |]
        ],
    Cons 'TsPair [
        Field [| unTsPairL          :: TestSimple |],
        Field [| unTsPairR          :: TestSimple |]
        ]
    ]

--------------------------------------------------------------------------------

data TestIndexed
    = TiInt Int
    | TiIntList [Int]
    | TiChar2 Char Char
    | TiInteger Integer
    | TiMaybeInt (Maybe Int)
    | TiChar2Permuted Char Char
    | TiPair TestIndexed TestIndexed
    deriving (Eq, Show, Typeable)

deriveIndexedBi ''TestIndexed [
    Cons 'TiInt [
        Field [| 0 :: Int         |]
        ],
    Cons 'TiIntList [
        Field [| 0 :: [Int]       |]
        ],
    Cons 'TiChar2 [
        Field [| 0 :: Char        |],
        Field [| 1 :: Char        |]
        ],
    Cons 'TiInteger [
        Field [| 0 :: Integer     |]
        ],
    Cons 'TiMaybeInt [
        Field [| 0 :: Maybe Int   |]
        ],
    Cons 'TiChar2Permuted [
        Field [| 1 :: Char        |],
        Field [| 0 :: Char        |]
        ],
    Cons 'TiPair [
        Field [| 0 :: TestIndexed |],
        Field [| 1 :: TestIndexed |]
        ]
    ]

--------------------------------------------------------------------------------

-- The validity of our comparison tests relies on this function. Fortunately,
-- it's a pretty straightforward translation.
simpleToIndexed :: TestSimple -> TestIndexed
simpleToIndexed (TsInt i)             = TiInt i
simpleToIndexed (TsIntList is)        = TiIntList is
simpleToIndexed (TsChar2 l r)         = TiChar2 l r
simpleToIndexed (TsInteger i)         = TiInteger i
simpleToIndexed (TsMaybeInt mi)       = TiMaybeInt mi
simpleToIndexed (TsChar2Permuted l r) = TiChar2Permuted l r
simpleToIndexed (TsPair l r)          = TiPair (simpleToIndexed l)
                                               (simpleToIndexed r)

--------------------------------------------------------------------------------

genTestSimple :: Range.Size -> Gen TestSimple
genTestSimple sz
  | sz > 0    = Gen.choice (pairType : flatTypes)
  | otherwise = Gen.choice flatTypes
  where
    pairType  = TsPair <$> genTestSimple (sz `div` 2)
                       <*> genTestSimple (sz `div` 2)
    flatTypes =
        [ TsInt <$> Gen.int Range.constantBounded
        , TsIntList <$> Gen.list (Range.linear 0 20) (Gen.int Range.constantBounded)
        , TsChar2 <$> Gen.unicode <*> Gen.unicode
        , TsInteger <$> Gen.integral (Range.linear (- bignum) bignum)
        , TsMaybeInt <$> Gen.maybe (Gen.int Range.constantBounded)
        , TsChar2Permuted <$> Gen.unicode <*> Gen.unicode
        ]
    bignum = 2 ^ (80 :: Integer)


prop_TestSimple_TestIndexed_equivalent :: Property
prop_TestSimple_TestIndexed_equivalent = H.withTests 2000 $ H.property $
    H.forAll (Gen.sized genTestSimple) >>= \ts ->
        B16.encode (serialize' ts)
        ===
        B16.encode (serialize' (simpleToIndexed ts))


tests :: IO Bool
tests =
  H.checkParallel $$discover
