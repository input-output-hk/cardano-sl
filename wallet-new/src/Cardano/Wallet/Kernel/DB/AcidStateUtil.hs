{-# LANGUAGE RankNTypes #-}

-- | Some utilities for working with acid-state
module Cardano.Wallet.Kernel.DB.AcidStateUtil (
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

zoom :: Lens' st st' -> Update' st' e a -> Update' st e a
zoom l upd = StateT $ \large -> do
    let update small' = large & l .~ small'
        small         = large ^. l
    fmap update <$> runStateT upd small

zoomTry :: e -> Lens' st (Maybe st') -> Update' st' e a -> Update' st e a
zoomTry e l upd = StateT $ \large -> do
    let update small' = large & l .~ Just small'
        mSmall        = large ^. l
    case mSmall of
      Nothing    -> throwError e
      Just small -> fmap update <$> runStateT upd small

zoomAll :: Traversal' st st' -> Update' st' e () -> Update' st e ()
zoomAll t upd = StateT $ \large -> do
    let updateSmall = execStateT upd
    ((),) <$> t updateSmall large

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mustBeRight :: Either Void b -> b
mustBeRight (Left  a) = absurd a
mustBeRight (Right b) = b
