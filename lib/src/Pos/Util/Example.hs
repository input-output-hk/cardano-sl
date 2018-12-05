{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pos.Util.Example where

import           Universum

import           Data.Aeson (ToJSON)
import qualified Data.Map.Strict as Map
import           Data.Swagger (Definitions, NamedSchema (..), Schema,
                     sketchSchema)
import           Data.Swagger.Declare (Declare)
import           Data.Typeable (typeOf)

import           Test.QuickCheck (Arbitrary (..), listOf1)
import           Test.QuickCheck.Gen (Gen (..), resize)
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Random (mkQCGen)

class Arbitrary a => Example a where
    example :: Gen a
    example = arbitrary

instance Example ()
instance Example a => Example (NonEmpty a)

-- NOTE: we don't want to see empty list examples in our swagger doc :)
instance Example a => Example [a] where
    example = listOf1 example

-- NOTE: we don't want to see "null" examples in our swagger doc :)
instance Example a => Example (Maybe a) where
    example = Just <$> example

-- NOTE: we don't want to see empty maps in our swagger doc :)
instance (Ord k, Example k, Example v) => Example (Map k v) where
    example = Map.fromList <$> listOf1 ((,) <$> example <*> example)

--
-- HELPERS
--

-- | Generates an example for type `a` with a static seed.
genExample :: Example a => a
genExample =
    unGen (resize 3 example) (mkQCGen 42) 42

-- | Generates a `NamedSchema` exploiting the `ToJSON` instance in scope,
-- by calling `sketchSchema` under the hood.
fromExampleJSON
    :: (ToJSON a, Typeable a, Example a)
    => proxy a
    -> Declare (Definitions Schema) NamedSchema
fromExampleJSON (_ :: proxy a) = do
    let (randomSample :: a) = genExample
    return $ NamedSchema (Just $ fromString $ show $ typeOf randomSample) (sketchSchema randomSample)
