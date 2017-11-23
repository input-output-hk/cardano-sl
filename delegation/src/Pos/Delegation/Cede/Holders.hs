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

import           Control.Lens (at, (%=))
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet as HS
import qualified Ether

import           Pos.DB.Class (MonadDBRead)
import           Pos.Delegation.Cede.Class (MonadCede (..), MonadCedeRead (..))
import           Pos.Delegation.Cede.Types (CedeModifier, DlgEdgeAction (..), cmHasPostedThisEpoch,
                                            cmPskMods, dlgEdgeActionIssuer)
import qualified Pos.Delegation.DB as DB
import           Pos.Util.Util (ether)

----------------------------------------------------------------------------
-- Pure database-only holder
----------------------------------------------------------------------------

data DBCedeTag

type DBCede = Ether.TaggedTrans DBCedeTag IdentityT

runDBCede :: DBCede m a -> m a
runDBCede = coerce

instance MonadDBRead m => MonadCedeRead (DBCede m) where
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
type MapCede = Ether.LazyStateT' CedeModifier

runMapCede :: CedeModifier -> MapCede m a -> m (a, CedeModifier)
runMapCede = flip Ether.runLazyStateT

evalMapCede :: Monad m => CedeModifier -> MapCede m a -> m a
evalMapCede = flip Ether.evalLazyStateT

instance MonadDBRead m => MonadCedeRead (MapCede m) where
    getPsk iPk =
        ether $ use (cmPskMods . at iPk) >>= \case
            Nothing                -> lift $ DB.getPskByIssuer $ Right iPk
            Just (DlgEdgeDel _)    -> pure Nothing
            Just (DlgEdgeAdd psk ) -> pure (Just psk)
    hasPostedThisEpoch sId =
        ether $ use (cmHasPostedThisEpoch . at sId) >>= \case
            Nothing                -> lift $ DB.isIssuerPostedThisEpoch sId
            Just v                 -> pure v
    getAllPostedThisEpoch = ether $ do
        allPostedDb <- lift DB.getThisEpochPostedKeys
        mods <- use cmHasPostedThisEpoch
        pure $ HM.foldlWithKey'
                   (\hs k v -> (if v then HS.insert else HS.delete) k hs)
                   allPostedDb
                   mods

instance MonadDBRead m => MonadCede (MapCede m) where
    modPsk eAction = do
        let issuer = dlgEdgeActionIssuer eAction
        ether $ cmPskMods %= HM.insert issuer eAction
    addThisEpochPosted sId = ether $ cmHasPostedThisEpoch %= HM.insert sId True
    delThisEpochPosted sId = ether $ cmHasPostedThisEpoch %= HM.insert sId False
