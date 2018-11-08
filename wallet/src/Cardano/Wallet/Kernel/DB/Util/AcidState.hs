{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Some utilities for working with acid-state
module Cardano.Wallet.Kernel.DB.Util.AcidState (
    -- * Updates
    Update' -- opaque
    -- ** Running updates
  , runUpdate'
  , runUpdateDiscardSnapshot
  , runUpdate_
  , runUpdateNoErrors
  , tryUpdate
  , tryUpdate'
  , mapUpdateErrors
  , discardUpdateErrors
    -- * Queries (to be run on a snapshot)
  , Query' -- opaque
  , mapQueryErrors
  , localQuery
    -- ** Running queries
  , runQuery'
  , runQueryNoErrors
    -- * Zoom operators
  , CanZoom(..)
  , zoom
  , zoomDef
  , zoomIxSet
  , zoomIxSet_
  , zoomCreate
    -- * Convenience re-exports
  , throwError
  ) where

import           Universum

import           Control.Monad.Except
import           Data.Acid (Update)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map

import           Cardano.Wallet.Kernel.DB.Util.IxSet (HasPrimKey, Indexable,
                     IxSet, PrimKey)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.DB.Util.Zoomable as Z
import           Cardano.Wallet.Kernel.Util.StrictStateT
import           UTxO.Util (mustBeRight)

{-------------------------------------------------------------------------------
  Acid-state updates
-------------------------------------------------------------------------------}

newtype Update' e st a = Update' (StrictStateT st (Except e) a)
   deriving
     ( Functor
     , Applicative
     , Monad
     , MonadError e
     , MonadState st
     )

instance Z.Zoomable (Update' e) where
  type Result (Update' e) = Z.UpdResult e

  wrap   = coerce
  unwrap = coerce

mapUpdateErrors :: (e -> e') -> Update' e st a -> Update' e' st a
mapUpdateErrors f (Update' upd) = Update' $
    strictStateT $ withExcept f . runStrictStateT upd

discardUpdateErrors :: Update' e st a -> Update' e' st ()
discardUpdateErrors (Update' upd) = Update' $
    strictStateT $ \s -> ExceptT $ do
        runExceptT (runStrictStateT (void upd) s) <&> \case
            Left  _       -> return ((), s)
            Right (_, s') -> return ((), s')

tryUpdate :: Update' e st a -> Update' e' st (Either e a)
tryUpdate = mapUpdateErrors absurd . tryUpdate'

tryUpdate' :: forall st e a. Update' e st a -> Update' Void st (Either e a)
tryUpdate' (Update' action) = do
    state0 <- get
    let convert :: Either e (a, st) -> Either Void (Either e a, st)
        convert = Right . \case
            Left  e           -> (Left  e, state0)
            Right (x, state1) -> (Right x, state1)
    Update' $ mapStrictStateT (mapExcept convert) action

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

newtype Query' e st a = Query' (ReaderT st (Except e) a)
   deriving
     ( Functor
     , Applicative
     , Monad
     , MonadError e
     , MonadReader st
     )

instance Z.Zoomable (Query' e) where
  type Result (Query' e) = Z.QryResult e

  wrap   = coerce
  unwrap = coerce

mapQueryErrors :: (e -> e') -> Query' e st a -> Query' e' st a
mapQueryErrors f (Query' qry) = Query' $
    ReaderT $ withExcept f . runReaderT qry

-- | Generalization of 'local'
localQuery :: Query' e st st' -> Query' e st' a -> Query' e st a
localQuery (Query' getSmall) (Query' qry) = Query' $ ReaderT $ \large -> do
    small <- runReaderT getSmall large
    runReaderT qry small

{-------------------------------------------------------------------------------
  Running updates
-------------------------------------------------------------------------------}

-- | Run an update
--
-- NOTE: Your acid-state 'Update' queries should be composed by one /and only
-- one/ 'runUpdate'', as each call to 'unUpdate'' will return the modified copy
-- of the state alongside with the final result, so in presence of multiple
-- calls it would be very error prone: one wrong return and an 'Update' query
-- could return a stale version of the DB.
runUpdate' :: forall e st a. Update' e st a -> Update st (Either e (st, a))
runUpdate' (Update' upd) = do
    st <- get
    case upd' st of
      Left  e        -> return (Left e)
      Right (a, st') -> put st' >> return (Right (st, a))
  where
    upd' :: st -> Either e (a, st)
    upd' = runExcept . runStrictStateT upd

-- | Run update and discard the DB snapshot
--
-- Use this function sparingly only when you are sure you won't need the DB
-- snapshot afterwards. If you do want to re-access the DB after you ran an
-- 'Update', it means the function you probably need is 'runUpdate''.
runUpdateDiscardSnapshot :: forall e st a. Update' e  st a
                         -> Update st (Either e a)
runUpdateDiscardSnapshot upd = fmap snd <$> runUpdate' upd

-- | Variation on 'runUpdate' for functions with no result
--
-- (Naming to match @forM_@ and co)
runUpdate_ :: Update' e st () -> Update st (Either e st)
runUpdate_ = fmap (fmap fst) . runUpdate'

runUpdateNoErrors :: Update' Void st a -> Update st a
runUpdateNoErrors = fmap (snd . mustBeRight) . runUpdate'

{-------------------------------------------------------------------------------
  Running queries
-------------------------------------------------------------------------------}

runQuery' :: Query' e st a -> st -> Either e a
runQuery' (Query' qry) st = runExcept $ runReaderT qry st

runQueryNoErrors :: Query' Void st a -> st -> a
runQueryNoErrors qry = mustBeRight . runQuery' qry

{-------------------------------------------------------------------------------
  Wrap existing zoom operators
-------------------------------------------------------------------------------}

class CanZoom f where
  withZoomableConstraints ::
         ( ( Z.Zoomable (f e)
            , Monad (f e st)
           )
           => f e st a
         )
      -> f e st a
  missing :: e -> f e st a

instance CanZoom Update' where
  withZoomableConstraints = id
  missing = throwError

instance CanZoom Query' where
  withZoomableConstraints = id
  missing = throwError

zoom :: CanZoom f => Lens' st st' -> f e st' a -> f e st a
zoom l k = withZoomableConstraints $ Z.zoom l k

zoomDef :: CanZoom f
        => f e st  a -- ^ When not found
        -> Lens' st (Maybe st')
        -> f e st' a -- ^ When found
        -> f e st  a
zoomDef def l k = withZoomableConstraints $ Z.zoomDef def l k

{-------------------------------------------------------------------------------
  Additional zoom operators

  Most of these are just thin wrappers around their counterparts in "Zoomable"
-------------------------------------------------------------------------------}

zoomIxSet_ :: Indexable a
           => Lens' st (IxSet a)
           -> Update' e a  ()
           -> Update' e st ()
zoomIxSet_ l k = Z.zoomAll_ (l . IxSet.otraversal) k

zoomIxSet :: forall e st a b. (Indexable a, HasPrimKey a)
          => Lens' st (IxSet a)
          -> Update' e a  b
          -> Update' e st (Map (PrimKey a) b)
zoomIxSet = \l k -> aux <$> Z.zoomAll (l . IxSet.otraversal) (withSnapshot k)
  where
    aux :: [(b, a)] -> Map (PrimKey a) b
    aux = Map.fromList . map (\(b, a) -> (IxSet.primKey a, b))

    withSnapshot :: Update' e a b -> Update' e a (b, a)
    withSnapshot k = (,) <$> k <*> get

-- | Run an update on part of the state.
--
-- If the specified part of the state does not yet exist, run an action to
-- create it. This action is run in the context of the overall state and may
-- modify it.
--
-- NOTE: Since creation only makes sense for updates, this is not defined for
-- queries.
zoomCreate :: Update' e st  st'     -- ^ Action to create small state if needed
           -> Lens' st (Maybe st')  -- ^ Index the state
           -> Update' e st' a       -- ^ Action to run on the smaller state
           -> Update' e st  a
zoomCreate (Update' mkSmall) l (Update' upd) =
    Update' $ strictStateT $ \large -> do
      case large ^. l of
        Just small -> do
          let update small' = large & l .~ Just small'
          fmap update <$> runStrictStateT upd small
        Nothing -> do
          (small, large') <- runStrictStateT mkSmall large
          let update small' = large' & l .~ Just small'
          fmap update <$> runStrictStateT upd small
