{-# LANGUAGE RankNTypes #-}

-- | Some utilities for working with acid-state
module Cardano.Wallet.Kernel.DB.Util.AcidState (
    -- * Acid-state updates with support for errors
    Update'
  , runUpdate'
  , runUpdateNoErrors
  , runUpdateDiscardSnapshot
  , mapUpdateErrors
    -- * Zooming
  , zoom
  , zoomDef
  , zoomCreate
  , zoomAll
    -- ** Convenience re-exports
  , throwError
  ) where

import           Universum

import           Control.Monad.Except
import           Data.Acid (Update)

import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexable, IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Acid-state updates with support for errors (and zooming, see below).
  NOTA BENE: Your acid-state 'Update' queries should be composed by one
  @and only one@ \"runUpdate'\", as each call to \"runUpdate'\" will return
  the modified copy of the state 'st' alongside with the final result, so
  in presence of multiple calls it would be very error prone: one wrong return
  and an 'Update' query could return a stale version of the DB.
-------------------------------------------------------------------------------}

type Update' st e = StateT st (Except e)

runUpdate' :: forall e st a. Update' st e a -> Update st (Either e (st, a))
runUpdate' upd = do
    st <- get
    case upd' st of
      Left  e        -> return (Left e)
      Right (a, st') -> put st' >> return (Right (st, a))
  where
    upd' :: st -> Either e (a, st)
    upd' = runExcept . runStateT upd

runUpdateNoErrors :: Update' st Void a -> Update st a
runUpdateNoErrors = fmap (snd . mustBeRight) . runUpdate'

mapUpdateErrors :: (e -> e') -> Update' st e a -> Update' st e' a
mapUpdateErrors f upd = StateT $ withExcept f . runStateT upd

-- | Like \"runUpdate'\", but it discards the DB after running the query.
-- Use this function sparingly only when you are sure you won't need the
-- DB snapshot afterwards. If you find yourself willing to re-access the DB
-- after you ran the 'Update', it means you didn't need this function in the
-- first place.
runUpdateDiscardSnapshot :: forall e st a. Update' st e a
                         -> Update st (Either e a)
runUpdateDiscardSnapshot upd = fmap snd <$> runUpdate' upd

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
-- If the specified part does not exist, run the default action.
zoomDef :: Update' st  e a      -- ^ Run when lens returns 'Nothing'
        -> Lens' st (Maybe st') -- ^ Index the state
        -> Update' st' e a      -- ^ Action to run on the smaller state
        -> Update' st  e a
zoomDef def l upd = StateT $ \large -> do
    let update small' = large & l .~ Just small'
        mSmall        = large ^. l
    case mSmall of
      Nothing    -> runStateT def large
      Just small -> fmap update <$> runStateT upd small

-- | Run an update on part of the state.
--
-- If the specified part does not exist, use the default provided,
-- then only apply the update.
zoomCreate :: st'                   -- ^ Default state
           -> Lens' st (Maybe st')  -- ^ Index the state
           -> Update' st' e a       -- ^ Action to run on the smaller state
           -> Update' st  e a
zoomCreate def l upd = StateT $ \large -> do
    let update small' = large & l .~ Just small'
        small         = fromMaybe def (large ^. l)
    fmap update <$> runStateT upd small

-- | Run an update on /all/ parts of the state.
--
-- NOTE: Uses 'otraverse' under the hood, and therefore needs to reconstruct
-- the entire 'IxSet' and associated indices. Only use for small sets.
zoomAll :: Indexable st'
        => Lens' st (IxSet st')
        -> Update' st' e ()
        -> Update' st  e ()
zoomAll l upd = StateT $ \large -> do
    let update ixset' = large & l .~ ixset'
        ixset         = large ^. l
    (((),) . update) <$> IxSet.otraverse (execStateT upd) ixset

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mustBeRight :: Either Void b -> b
mustBeRight (Left  a) = absurd a
mustBeRight (Right b) = b
