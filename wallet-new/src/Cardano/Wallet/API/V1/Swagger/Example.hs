module Cardano.Wallet.API.V1.Swagger.Example where

import           Universum

import           Data.Aeson (ToJSON)
import           Data.Swagger (Definitions, NamedSchema (..), Schema,
                     sketchSchema)
import           Data.Swagger.Declare (Declare)
import           Data.Typeable (typeOf)

import           Test.QuickCheck (Arbitrary (..), listOf1)
import           Test.QuickCheck.Gen (Gen (..), resize)
import           Test.QuickCheck.Random (mkQCGen)

import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Data.Map.Strict as Map

class Arbitrary a => Example a where
    example :: Gen a
    example = arbitrary

instance Example ()
instance (Example a, Arbitrary (NonEmpty a) ) => Example (NonEmpty a)

-- NOTE: we don't want to see empty list examples in our swagger doc :)
instance Example a => Example [a] where
    example = listOf1 example

instance (IxSet.Indexable a, Example a) => Example (IxSet.IxSet a) where
    example = IxSet.fromList <$> listOf1 example

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


-- IMPORTANT: if executing `grep "[]\|null" wallet-new/spec/swagger.json` returns any element - then we have to add Example instances for those objects because we don't want to see [] or null examples in our docs.
--
-- TODO: We should probably add this as a part of our swagger CI script and fail swagger if we find some of them - with instruction to the developer above what is said above.
--
