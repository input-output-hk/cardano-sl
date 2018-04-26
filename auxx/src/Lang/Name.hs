module Lang.Name
       ( Letter(getLetter)
       , unsafeMkLetter
       , Name(..)
       , unsafeMkName
       ) where

import           Prelude (Show (..))
import           Universum

import           Data.Char (isAlpha)
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Split (splitWhen)
import qualified Data.Text.Buildable as Buildable
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary (..))
import           Test.QuickCheck.Gen (Gen, suchThat, listOf)

-- | Invariant: @isAlpha . getLetter = const True@
newtype Letter = Letter { getLetter :: Char }
    deriving (Eq, Ord, Show)

unsafeMkLetter :: Char -> Letter
unsafeMkLetter = Letter

instance Arbitrary Letter where
    arbitrary = Letter <$> arbitrary `suchThat` isAlpha

newtype Name = Name (NonEmpty (NonEmpty Letter))
    deriving (Eq, Ord, Generic)

unsafeMkName :: [String] -> Name
unsafeMkName = coerce . fmap NonEmpty.fromList . NonEmpty.fromList

instance Arbitrary Name where
    arbitrary = Name <$> neList (neList arbitrary)
      where
        neList :: Gen a -> Gen (NonEmpty a)
        neList gen = (:|) <$> gen <*> listOf gen

instance Buildable Name where
    build
        = foldMap (fromString . toList)
        . NonEmpty.intersperse ('-' :| [])
        . fromLetterNENE
      where
        fromLetterNENE :: Name -> NonEmpty (NonEmpty Char)
        fromLetterNENE = coerce

instance Show Name where
    showsPrec n = showsPrec n . pretty

-- | Unsafe, requires manual validation.
instance IsString Name where
    fromString = unsafeMkName . splitWhen (=='-')
