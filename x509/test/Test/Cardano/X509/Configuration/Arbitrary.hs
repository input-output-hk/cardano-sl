{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Cardano.X509.Configuration.Arbitrary
    ( Invalid(..)
    , Unknown(..)
    , AltNames(..)
    ) where

import           Universum

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck (Arbitrary (..), Gen, choose, elements, listOf,
                     listOf1, oneof, scale, suchThat)
import           Test.QuickCheck.Modifiers (Positive (..))

import           Cardano.X509.Configuration (CertConfiguration (..),
                     DirConfiguration (..), ServerConfiguration (..),
                     TLSConfiguration (..))


--
-- Types
--

-- | Helper to declare Arbitrary instances with generators that
-- generate invalid values
newtype (Show a) => Invalid a = Invalid { getInvalid :: a } deriving (Show)


-- | Helper to declare Arbitrary instances with generators that
-- generate unkown valus
newtype (Show a) => Unknown a = Unknown { getUnknown :: a } deriving (Show)


-- | Easily generate alternative names
newtype AltNames = AltNames { getAltNames :: NonEmpty String } deriving (Show)


--
-- Instances
--

instance Arbitrary TLSConfiguration where
    arbitrary = TLSConfiguration
        <$> arbitrary
        <*> arbitrary
        <*> scale (`mod` 5) (listOf arbitrary)

    shrink (TLSConfiguration ca server clients) =
        TLSConfiguration ca server <$> shrink clients


instance Arbitrary (Invalid TLSConfiguration) where
    arbitrary = fmap Invalid $ TLSConfiguration
        <$> arbitraryInvalid
        <*> arbitraryInvalid
        <*> scale (`mod` 5) (listOf arbitraryInvalid)

    shrink (Invalid tlsConf) =
        Invalid <$> shrink tlsConf


instance Arbitrary CertConfiguration where
    arbitrary = CertConfiguration
        <$> elements ["IOHK", "Emurgo", "Cardano Foundation"]
        <*> elements ["Daedalus Wallet", "Icarus Wallet", "Prometheus", "Root CA"]
        <*> arbitraryPositive

    shrink (CertConfiguration org name days) =
        CertConfiguration org name . getPositive <$> shrink (Positive days)


instance Arbitrary (Invalid CertConfiguration) where
    arbitrary = fmap Invalid $ CertConfiguration
        <$> elements ["IOHK", "Emurgo", "Cardano Foundation"]
        <*> elements ["Daedalus Wallet", "Icarus Wallet", "Prometheus", "Root CA"]
        <*> choose (-10, 10)

    shrink (Invalid (CertConfiguration org name days)) =
        (Invalid . CertConfiguration org name) <$> shrink days


instance Arbitrary ServerConfiguration where
    arbitrary = ServerConfiguration
        <$> arbitrary
        <*> fmap getAltNames arbitrary

    shrink (ServerConfiguration cert altNames) =
        mkServerConfiguration <$> shrink (cert, AltNames altNames)


instance Arbitrary (Invalid ServerConfiguration) where
    arbitrary = fmap Invalid $ ServerConfiguration
        <$> arbitraryInvalid
        <*> fmap getAltNames arbitrary

    shrink (Invalid serverConf) =
        Invalid <$> shrink serverConf


instance Arbitrary DirConfiguration where
    arbitrary = DirConfiguration
        <$> arbitraryBasicString
        <*> arbitraryBasicString
        <*> oneof [pure Nothing, Just <$> arbitraryBasicString]

    shrink _ = []


instance Arbitrary AltNames where
    arbitrary =
        fmap mkAltNames $ listOf1 $ elements
            [ "localhost"
            , "localhost.localdomain"
            , "127.0.0.1"
            , "::1"
            ]

    shrink (AltNames xs) =
        case xs of
            (_ :| [])   -> []
            (x :| rest) ->
                mkAltNames <$> filter (not . null) (shrink (x:rest))


instance Arbitrary (Unknown AltNames) where
    arbitrary =
        fmap (Unknown . mkAltNames) $ listOf1 $ elements
            [ "www.iohk.io"
            , "14.14.14.14"
            , "2607:f0d0:1002:0051:0000:0000:0000:0004"
            ]

    shrink (Unknown altNames) =
        Unknown <$> shrink altNames


--
-- Internals
--

mkAltNames :: [String] -> AltNames
mkAltNames =
    AltNames . NonEmpty.fromList . List.nub

mkServerConfiguration :: (CertConfiguration, AltNames) -> ServerConfiguration
mkServerConfiguration =
    uncurry ServerConfiguration . second getAltNames

arbitraryPositive :: Gen Int
arbitraryPositive = fmap getPositive arbitrary

arbitraryBasicString :: Gen String
arbitraryBasicString =
    listOf (suchThat arbitrary Char.isLetter)

arbitraryInvalid :: (Show a, Arbitrary (Invalid a)) => Gen a
arbitraryInvalid =
    fmap getInvalid arbitrary
