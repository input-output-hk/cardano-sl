{-# LANGUAGE RankNTypes #-}

-- | Some utilities for working with acid-state
module Cardano.Wallet.Kernel.DB.Util.AcidState (
    -- * Acid-state updates with support for errors
    Update'
  , runUpdate'
  , runUpdateNoErrors
  , mapUpdateErrors
    -- * Zooming
  , zoom
  , zoomTry
  , zoomAll
    -- ** Convenience re-exports
  , throwError
  ) where

import           Universum

import           Control.Monad.Except
import           Data.Acid (Update)

import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet, Indexable)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Acid-state updates with support for errors (and zooming, see below)
-------------------------------------------------------------------------------}

type Update' st e = StateT st (Except e)

runUpdate' :: forall e st a. Update' st e a -> Update st (Either e a)
runUpdate' upd = do
    st <- get
    case upd' st of
      Left  e        -> return (Left e)
      Right (a, st') -> put st' >> return (Right a)
  where
    upd' :: st -> Either e (a, st)
    upd' = runExcept . runStateT upd

runUpdateNoErrors :: Update' st Void a -> Update st a
runUpdateNoErrors = fmap mustBeRight . runUpdate'

mapUpdateErrors :: (e -> e') -> Update' st e a -> Update' st e' a
mapUpdateErrors f upd = StateT $ withExcept f . runStateT upd

{-------------------------------------------------------------------------------
  Zooming
-------------------------------------------------------------------------------}

-- | Run an update on part of the state.
zoom :: Lens' st st' -> Update' st' e a -> Update' st e a
zoom l upd = StateT $ \large -> do
    let update small' = large & l .~ small'
        small         = large ^. l
    fmap update <$> runStateT upd small

-- | Run an update on part of the state.
--
-- If the specified part does not exist, throw the given error.
zoomTry :: e -> Lens' st (Maybe st') -> Update' st' e a -> Update' st e a
zoomTry e l upd = StateT $ \large -> do
    let update small' = large & l .~ Just small'
        mSmall        = large ^. l
    case mSmall of
      Nothing    -> throwError e
      Just small -> fmap update <$> runStateT upd small

-- | Run an update on /all/ parts of the state.
zoomAll :: Indexable st'
        => Lens' st (IxSet st') -> Update' st' e () -> Update' st e ()
zoomAll l upd = StateT $ \large -> do
    let update ixset' = large & l .~ ixset'
        ixset         = large ^. l
    (((), ) . update) <$> IxSet.traverse (execStateT upd) ixset

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mustBeRight :: Either Void b -> b
mustBeRight (Left  a) = absurd a
mustBeRight (Right b) = b
