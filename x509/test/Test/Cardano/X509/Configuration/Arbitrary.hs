{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.X509.Configuration.Arbitrary () where

import           Universum

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck (Arbitrary (..), Gen, elements, listOf,
                     listOf1, oneof, scale, suchThat)
import           Test.QuickCheck.Modifiers (Positive (..))

import           Cardano.X509.Configuration (CertConfiguration (..),
                     DirConfiguration (..), ServerConfiguration (..),
                     TLSConfiguration (..))


instance Arbitrary TLSConfiguration where
    arbitrary = TLSConfiguration
        <$> arbitrary
        <*> arbitrary
        <*> scale (`mod` 5) (listOf arbitrary)

    shrink (TLSConfiguration ca server clients) =
        TLSConfiguration ca server <$> shrink clients


instance Arbitrary CertConfiguration where
    arbitrary = CertConfiguration
        <$> elements ["IOHK", "Emurgo", "Cardano Foundation"]
        <*> elements ["Daedalus Wallet", "Icarus Wallet", "Prometheus", "Root CA"]
        <*> arbitraryPositive

    shrink (CertConfiguration org name days) =
        CertConfiguration org name . getPositive <$> shrink (Positive days)


instance Arbitrary ServerConfiguration where
    arbitrary = ServerConfiguration
        <$> arbitrary
        <*> fmap getAltNames arbitrary

    shrink (ServerConfiguration cert altNames) =
        mkServerConfiguration <$> shrink (cert, AltNames altNames)


instance Arbitrary DirConfiguration where
    arbitrary = DirConfiguration
        <$> arbitraryBasicString
        <*> arbitraryBasicString
        <*> oneof [pure Nothing, Just <$> arbitraryBasicString]

    shrink _ = []


instance Arbitrary AltNames where
    arbitrary =
        fmap mkAltNames $ listOf1 $ elements
            ["127.0.0.1", "localhost", "::1", "localhost.localdomain"]

    shrink (AltNames xs) =
        case xs of
            (_ :| [])   -> []
            (x :| rest) ->
                mkAltNames <$> filter (not . null) (shrink (x:rest))


newtype AltNames = AltNames { getAltNames :: NonEmpty String }

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
