{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides implementations of "MonadCede" based on
-- either pure DB or DB+hashmap access.

module Pos.Delegation.Cede.Holders
       ( DBCede
       , runDBCede

       , MapCede
       , runMapCede
       , evalMapCede
       ) where

import           Universum

import           Control.Lens (at, (%=), (.=))
import qualified Control.Monad.State.Strict as Mtl
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet as HS
import           UnliftIO (MonadUnliftIO)

import           Pos.DB.Class (MonadDBRead)
import           Pos.Delegation.Cede.Class (MonadCede (..), MonadCedeRead (..))
import           Pos.Delegation.Cede.Types (CedeModifier, DlgEdgeAction (..), cmHasPostedThisEpoch,
                                            cmLookupCache, cmPskMods, dlgEdgeActionIssuer)
import qualified Pos.Delegation.DB as DB

----------------------------------------------------------------------------
-- Pure database-only holder
----------------------------------------------------------------------------

type DBCede = IdentityT

runDBCede :: DBCede m a -> m a
runDBCede = coerce

instance (MonadDBRead m, MonadUnliftIO m) => MonadCedeRead (DBCede m) where
    getPsk = DB.getPskByIssuer . Right
    hasPostedThisEpoch = DB.isIssuerPostedThisEpoch
    getAllPostedThisEpoch = DB.getThisEpochPostedKeys

-- We don't provide 'MonadCede' instance as writing into database is
-- performed in batches on block application only.

----------------------------------------------------------------------------
-- DB + CedeModifier resolving
----------------------------------------------------------------------------

-- | Monad transformer that holds extra layer of modifications to the
-- underlying set of PSKs (which can be empty if you want).
newtype MapCede m a = MapCede
    { unMapCede :: StateT CedeModifier m a
    } deriving (Functor,Applicative,Monad,MonadIO,MonadState CedeModifier,MonadThrow,MonadTrans)

runMapCede :: CedeModifier -> MapCede m a -> m (a, CedeModifier)
runMapCede c m = Mtl.runStateT (unMapCede m) c

evalMapCede :: Monad m => CedeModifier -> MapCede m a -> m a
evalMapCede c m = Mtl.evalStateT (unMapCede m) c

instance (MonadDBRead m, MonadUnliftIO m) => MonadCedeRead (MapCede m) where
    getPsk i =
        use (cmLookupCache . at i) >>= \case
            Just res -> pure res
            Nothing ->
                use (cmPskMods . at i) >>= \case
                    Nothing                -> do
                        res <- lift $ DB.getPskByIssuer $ Right i
                        cmLookupCache . at i .= Just res
                        pure res
                    Just (DlgEdgeDel _)    -> pure Nothing
                    Just (DlgEdgeAdd psk ) -> pure (Just psk)
    hasPostedThisEpoch sId =
        use (cmHasPostedThisEpoch . at sId) >>= \case
            Nothing                -> lift $ DB.isIssuerPostedThisEpoch sId
            Just v                 -> pure v
    getAllPostedThisEpoch = do
        allPostedDb <- lift DB.getThisEpochPostedKeys
        mods <- use cmHasPostedThisEpoch
        pure $ HM.foldlWithKey'
                   (\hs k v -> (if v then HS.insert else HS.delete) k hs)
                   allPostedDb
                   mods

instance (MonadDBRead m, MonadUnliftIO m) => MonadCede (MapCede m) where
    modPsk eAction = do
        let issuer = dlgEdgeActionIssuer eAction
        -- drop cache
        cmLookupCache . at issuer .= Nothing
        cmPskMods %= HM.insert issuer eAction
    addThisEpochPosted sId = cmHasPostedThisEpoch %= HM.insert sId True
    delThisEpochPosted sId = cmHasPostedThisEpoch %= HM.insert sId False
