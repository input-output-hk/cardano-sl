{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}

-- | Some utilities for working with acid-state
module Cardano.Wallet.Kernel.DB.Util.AcidState (
    -- * Acid-state updates with support for errors
    Update' -- opaque
  , runUpdate'
  , runUpdate_
  , runUpdateNoErrors
  , runUpdateDiscardSnapshot
  , mapUpdateErrors
    -- * Queries (to be run on a snapshot)
  , Query' -- opaque
  , runQuery'
  , runQueryNoErrors
  , mapQueryErrors
  , localQuery
    -- * Generalize over updates and queries
  , CanZoom(..)
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
import qualified Data.Map.Strict as Map

import           Cardano.Wallet.Kernel.DB.Util.IxSet (HasPrimKey, Indexable,
                     IxSet, PrimKey)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Util (mustBeRight)
import           Cardano.Wallet.Kernel.Util.StrictStateT

{-------------------------------------------------------------------------------
  Acid-state updates with support for errors (and zooming, see below).
-------------------------------------------------------------------------------}

newtype Update' st e a = Update' {
       unUpdate' :: StrictStateT st (Except e) a
     }
   deriving
     ( Functor
     , Applicative
     , Monad
     , MonadError e
     , MonadState st
     )

-- | NOTA BENE: Your acid-state 'Update' queries should be composed by one
-- @and only one@ \"runUpdate'\", as each call to \"runUpdate'\" will return
-- the modified copy of the state 'st' alongside with the final result, so
-- in presence of multiple calls it would be very error prone: one wrong return
-- and an 'Update' query could return a stale version of the DB.
runUpdate' :: forall e st a. Update' st e a -> Update st (Either e (st, a))
runUpdate' (Update' upd) = do
    st <- get
    case upd' st of
      Left  e        -> return (Left e)
      Right (a, st') -> put st' >> return (Right (st, a))
  where
    upd' :: st -> Either e (a, st)
    upd' = runExcept . runStrictStateT upd

-- | Variation on 'runUpdate' for functions with no result
--
-- (Naming to match @forM_@ and co)
runUpdate_ :: Update' st e () -> Update st (Either e st)
runUpdate_ = fmap (fmap fst) . runUpdate'

runUpdateNoErrors :: Update' st Void a -> Update st a
runUpdateNoErrors = fmap (snd . mustBeRight) . runUpdate'

mapUpdateErrors :: (e -> e') -> Update' st e a -> Update' st e' a
mapUpdateErrors f (Update' upd) = Update' $
    strictStateT $ withExcept f . runStrictStateT upd

-- | Like \"runUpdate'\", but it discards the DB after running the query.
-- Use this function sparingly only when you are sure you won't need the
-- DB snapshot afterwards. If you really want to re-access the DB
-- after you ran an 'Update', it means the function you need is \"runUpdate'\".
runUpdateDiscardSnapshot :: forall e st a. Update' st e a
                         -> Update st (Either e a)
runUpdateDiscardSnapshot upd = fmap snd <$> runUpdate' upd

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

newtype Query' st e a = Query' {
       unQuery' :: ReaderT st (Except e) a
     }
   deriving
     ( Functor
     , Applicative
     , Monad
     , MonadError e
     , MonadReader st
     )

runQuery' :: Query' st e a -> st -> Either e a
runQuery' (Query' qry) st = runExcept $ runReaderT qry st

runQueryNoErrors :: Query' st Void a -> st -> a
runQueryNoErrors qry = mustBeRight . runQuery' qry

mapQueryErrors :: (e -> e') -> Query' st e a -> Query' st e' a
mapQueryErrors f (Query' qry) = Query' $
    ReaderT $ withExcept f . runReaderT qry

-- | Generalization of 'local'
localQuery :: Query' st e st' -> Query' st' e a -> Query' st e a
localQuery (Query' getSmall) (Query' qry) = Query' $ ReaderT $ \large -> do
    small <- runReaderT getSmall large
    runReaderT qry small

{-------------------------------------------------------------------------------
  Zooming
-------------------------------------------------------------------------------}

-- | Contexts in which we can zoom (updates or queries)
class CanZoom f where
    -- | Zoom to a smaller part of the current context
    --
    -- The signature looks a bit frightening, but it is not hard to use.
    -- Given some lens @l@ from the larger context to the smaller context,
    -- typical usage looks something like this:
    --
    -- > withZoom $ \large zoomTo ->
    -- >   -- .. at this point @large@ is the full context ..
    -- >   zoomTo (large ^. l) (\small' -> large & l .~ small') $
    -- >     -- an action that runs in the smaller context
    --
    -- See 'zoom' and 'zoomDef' for two examples.
    withZoom :: (st -> (forall st'. st' -> (st' -> st) -> f st' e a -> f st e a) -> f st e a) -> f st e a

    -- | Throw an error about a missing part
    --
    -- Many zoom operators check for existance of some smaller context. When
    -- this smaller context does not exist, they can use 'missing' to throw
    -- an error.
    missing  :: e -> f st e a

instance CanZoom Update' where
    withZoom :: forall st e a. (st -> (forall st'. st' -> (st' -> st) -> Update' st' e a -> Update' st e a) -> Update' st e a) -> Update' st e a
    withZoom f = Update' $ strictStateT $ \large ->
        (($ large) . runStrictStateT . unUpdate') $ f large zoomTo
      where
        zoomTo :: st' -> (st' -> st) -> Update' st' e a -> Update' st e a
        zoomTo small updLarge (Update' upd) = Update' $ strictStateT $ \_large ->
            fmap updLarge <$> runStrictStateT upd small

    missing = throwError

instance CanZoom Query' where
    withZoom :: (st -> (forall st'. st' -> (st' -> st) -> Query' st' e a -> Query' st e a) -> Query' st e a) -> Query' st e a
    withZoom f = Query' $ ReaderT $ \large ->
        (($ large) . runReaderT . unQuery') $ f large zoomTo
      where
        zoomTo :: st' -> (st' -> st) -> Query' st' e a -> Query' st e a
        zoomTo small _updLarge (Query' qry) = Query' $ ReaderT $ \_large ->
            runReaderT qry small

    missing = throwError

-- | Run an update on part of the state.
zoom :: CanZoom f => Lens' st st' -> f st' e a -> f st e a
zoom l upd = withZoom $ \large zoomTo ->
    zoomTo (large ^. l) (\small' -> large & l .~ small') upd

-- | Run an update on part of the state.
--
-- If the specified part does not exist, run the default action.
zoomDef :: CanZoom f
        => f st  e a            -- ^ Run when lens returns 'Nothing'
        -> Lens' st (Maybe st') -- ^ Index the state
        -> f st' e a            -- ^ Action to run on the smaller state
        -> f st  e a
zoomDef def l upd = withZoom $ \large zoomTo ->
    case large ^. l of
      Nothing    -> def
      Just small -> zoomTo small (\small' -> large & l .~ Just small') upd

-- | Run an update on part of the state.
--
-- If the specified part of the state does not yet exist, run an action to
-- create it. This action is run in the context of the overall state and may
-- modify it.
--
-- NOTE: Since creation only makes sense for updates, this is not defined for
-- queries.
zoomCreate :: Update' st  e st'     -- ^ Action to create small state if needed
           -> Lens' st (Maybe st')  -- ^ Index the state
           -> Update' st' e a       -- ^ Action to run on the smaller state
           -> Update' st  e a
zoomCreate (Update' mkSmall) l (Update' upd) = Update' $ strictStateT $ \large -> do
    case large ^. l of
      Just small -> do
        let update small' = large & l .~ Just small'
        fmap update <$> runStrictStateT upd small
      Nothing -> do
        (small, large') <- runStrictStateT mkSmall large
        let update small' = large' & l .~ Just small'
        fmap update <$> runStrictStateT upd small

-- | Run an update on /all/ parts of the state.
--
-- NOTE: Uses 'otraverseCollect' under the hood, and therefore needs to
-- reconstruct the entire 'IxSet' and associated indices. Only use for small
-- sets.
--
-- NOTE: This could conceivably be generalized to also work on queries, but it's
-- not entirely obvious how to do so. For now we haven't needed that
-- generalization.
zoomAll :: (Indexable a, HasPrimKey a)
        => Lens' st (IxSet a)
        -> Update' a  e b
        -> Update' st e (Map (PrimKey a) b)
zoomAll l (Update' upd) = Update' $ strictStateT $ \large -> do
    let update (ixset', bs) = (Map.fromList bs, large & l .~ ixset')
        ixset               = large ^. l
    update <$> IxSet.otraverseCollect (fmap swap . runStrictStateT upd) ixset
