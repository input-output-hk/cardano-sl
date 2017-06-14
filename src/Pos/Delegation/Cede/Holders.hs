-- | This module provides implementations of "MonadCede" based on
-- either pure DB or DB+hashmap access.

module Pos.Delegation.Cede.Holders
       ( DBCede
       , runDBCede

       , MapCede
       , runMapCede
       ) where


import           Universum

import           Control.Lens                 (uses, (%=))
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Data.HashMap.Strict          as HM
import qualified Ether

import           Pos.DB.Class                 (MonadDBRead)
import           Pos.Delegation.Cede.Class    (MonadCede (..), MonadCedeRead (..))
import           Pos.Delegation.Cede.Types    (CedeModifier, DlgEdgeAction (..),
                                               dlgEdgeActionIssuer)
import qualified Pos.Delegation.DB            as DB
import           Pos.Util.Util                (ether)

----------------------------------------------------------------------------
-- Pure database-only holder
----------------------------------------------------------------------------

data DBCedeTag

type DBCede = Ether.TaggedTrans DBCedeTag IdentityT

runDBCede :: DBCede m a -> m a
runDBCede = coerce

instance MonadDBRead m => MonadCedeRead (DBCede m) where
    getPsk = DB.getPskByIssuer . Left

----------------------------------------------------------------------------
-- DB + CedeModifier resolving
----------------------------------------------------------------------------

-- | Monad transformer that holds extra layer of modifications to the
-- underlying set of PSKs (which can be empty if you want).
type MapCede = Ether.LazyStateT' CedeModifier

runMapCede :: CedeModifier -> MapCede m a -> m (a, CedeModifier)
runMapCede = flip Ether.runLazyStateT

instance MonadDBRead m => MonadCedeRead (MapCede m) where
    getPsk iPk =
        ether $ uses identity (HM.lookup iPk) >>= \case
            Nothing                -> lift $ DB.getPskByIssuer $ Left iPk
            Just (DlgEdgeDel _)    -> pure Nothing
            Just (DlgEdgeAdd psk ) -> pure (Just psk)

instance MonadDBRead m => MonadCede (MapCede m) where
    modPsk eAction =
        ether $ identity %= HM.insert (dlgEdgeActionIssuer eAction) eAction
