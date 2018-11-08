{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module Cardano.Wallet.Kernel.DB.Util.Zoomable (
    Zoomable(..)
    -- * Generic zoom operators
  , zoom
  , zoomAllM
  , zoomAll
  , zoomAll_
  , zoomMaybe
  , zoomDef
    -- * Support for defining instances
  , UpdResult(..)
  , QryResult(..)
  ) where

import           Universum

import           Control.Monad.Except (Except)
import           Data.Biapplicative (Biapplicative (..), Bifunctor (..))

{-------------------------------------------------------------------------------
  Zoomable
-------------------------------------------------------------------------------}

class Biapplicative (Result f) => Zoomable f where
  type Result f :: * -> * -> *

  -- | Roll
  --
  -- Should be equal to 'coerce'
  wrap :: (st -> Result f a st) -> f st a

  -- | Unroll
  --
  -- Should be equal to 'coerce'
  unwrap :: f st a -> (st -> Result f a st)

{-------------------------------------------------------------------------------
  Generic zoom operators
-------------------------------------------------------------------------------}

-- | Lift an action to a larger context
--
-- Lemma: @zoom = id@
-- Proof:
-- >    zoom l k
-- >  { definition }
-- > == wrap $ \st -> unwrapBi $ l (WrapBi . unwrap k) st
-- >  { remove wrap, unwrap, WrapBi/unwrapBi }
-- > == \st -> l k st
-- >  { eta-reduce }
-- > == l k
-- QED.
zoom :: Zoomable f => Lens' st st' -> f st' a -> f st  a
zoom l k = wrap $ \st -> unwrapBi $ l (WrapBi . unwrap k) st

-- | Lift an action to a larger context
--
-- If there are multiple matching smaller contexts, apply the action to all
-- of them, and accumulate the results.
--
-- Lemma: @zoomAll = id@
-- Proof: see proof of @zoom@ (which has an identical implementation)
zoomAllM :: (Zoomable f, Monoid a) => Traversal' st st' -> f st' a -> f st a
zoomAllM l k = wrap $ \st -> unwrapBi $ l (WrapBi . unwrap k) st

-- | Variation on 'zoomAll' that returns lists
zoomAll :: Zoomable f => Traversal' st st' -> f st' a -> f st [a]
zoomAll l k = wrap $ \st -> unwrapBi $ l (WrapBi . first (:[]) . unwrap k) st

-- | Variation on 'zoomAllM' for updates with no result
zoomAll_ :: Zoomable f => Traversal' st st' -> f st' () -> f st ()
zoomAll_ = zoomAllM

-- | Lift an action to a larger context
--
-- If the smaller context is missing, return 'Nothing'.
--
-- Lemma: @zoomMaybe l == l . liftMaybe@
-- Proof:
-- >    zoomMaybe l k
-- >  { definition }
-- > == wrap $ \st -> unwrapBi $ l (WrapBi . liftMaybe (unwrap k)) st
-- >  { remove wrap/unwrap, WrapBi/unwrapBi }
-- > == \st -> l (liftMaybe k) st
-- >  { eta-reduce }
-- > == l (liftMaybe k)
-- QED.
zoomMaybe :: Zoomable f
          => Lens' st (Maybe st')
          -> f st' a
          -> f st (Maybe a)
zoomMaybe l k = wrap $ \st -> unwrapBi $ l (WrapBi . liftMaybe (unwrap k)) st

-- | Like 'zoomMaybe', but run a fallback if the smaller context is missing
zoomDef :: (Zoomable f, Monad (f st))
        => f st  a -- ^ When not found
        -> Lens' st (Maybe st')
        -> f st' a -- ^ When found
        -> f st  a
zoomDef def l k = zoomMaybe l k `catchNothing` def

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

catchNothing :: Monad m => m (Maybe a) -> m a -> m a
catchNothing act fallback = act >>= maybe fallback return

liftMaybe :: Biapplicative f
          => (st -> f a st) -> Maybe st -> f (Maybe a) (Maybe st)
liftMaybe _ Nothing   = bipure Nothing Nothing
liftMaybe f (Just st) = bimap Just Just $ f st

{-------------------------------------------------------------------------------
  Applicative from Biapplicative
-------------------------------------------------------------------------------}

newtype FromBi f a st = WrapBi { unwrapBi :: f a st }

instance Bifunctor f => Functor (FromBi f a) where
  fmap f (WrapBi x) = WrapBi (second f x)

instance (Biapplicative f, Monoid a) => Applicative (FromBi f a) where
  pure st     = WrapBi $ bipure mempty st
  fun <*> arg = WrapBi $ bimap mappend ($) (unwrapBi fun) <<*>> unwrapBi arg

{-------------------------------------------------------------------------------
  Support for defining instances
-------------------------------------------------------------------------------}

newtype UpdResult e a st = UpdResult { getUpdResult :: Except e (a, st) }
newtype QryResult e a st = QryResult { getQryResult :: Except e  a      }

instance Bifunctor (UpdResult e) where
  bimap f g (UpdResult r) = UpdResult (fmap (bimap f g) r)

instance Bifunctor (QryResult e) where
  bimap f _ (QryResult r) = QryResult (fmap f r)

instance Biapplicative (UpdResult e) where
  bipure a st     = UpdResult $ return (a, st)
  funs <<*>> args = UpdResult $ do
      (f, g) <- getUpdResult funs
      (x, y) <- getUpdResult args
      return $ (f x, g y)

instance Biapplicative (QryResult e) where
  bipure x _    = QryResult $ return x
  fun <<*>> arg = QryResult $ getQryResult fun `ap` getQryResult arg
