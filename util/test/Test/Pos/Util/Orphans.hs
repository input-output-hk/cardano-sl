{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for external types/classes.

module Test.Pos.Util.Orphans where

import           Control.Monad.Reader.Class (MonadReader (..))
import qualified Crypto.Random as Rand

import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Monadic (PropertyM (..))

import           Universum

instance MonadReader r m => MonadReader r (PropertyM m) where
    ask = lift ask
    local f (MkPropertyM propertyM) =
        MkPropertyM $ \hole -> local f <$> propertyM hole

-- TODO: use the 'vec' package for traversable N-products
data Five a = Five a a a a a
    deriving (Functor, Foldable, Traversable)

five :: a -> Five a
five a = Five a a a a a

instance Rand.MonadRandom QC.Gen where
    getRandomBytes n = do
        Five a b c d e <- sequenceA . five $ QC.choose (minBound, maxBound)
        pure $ fst $ Rand.randomBytesGenerate n (Rand.drgNewTest (a,b,c,d,e))
