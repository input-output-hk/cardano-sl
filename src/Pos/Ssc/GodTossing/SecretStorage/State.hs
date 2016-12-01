{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.Ssc.GodTossing.SecretStorage.State
       (
         getSecret
       , setSecret
       , prepareSecretToNewSlot
       , checkpointSecret
       , SecretStorage
       ) where

import           Serokell.AcidState                      (queryExtended,
                                                          tidyExtendedState,
                                                          updateExtended)
import           Universum

import           Pos.Ssc.Class.Types                     (Ssc (SscNodeContext))
import           Pos.Ssc.GodTossing.SecretStorage.Acidic (GetS (..), Prepare (..),
                                                          SecretStorage, SetS (..))
import           Pos.Ssc.GodTossing.SecretStorage.Types  (GtSecret)
import           Pos.Ssc.GodTossing.Types.Type           (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types          (GtContext (gtcSecretStorage))
import           Pos.Types                               (SlotId (..))
import           Pos.WorkMode                            (WithNodeContext (getNodeContext),
                                                          ncSscContext)


type SConstraint a = forall m . (MonadIO m
                                , WithNodeContext SscGodTossing m
                                , SscNodeContext SscGodTossing ~ GtContext)
                                => m a
sscStorage :: SConstraint SecretStorage
sscStorage = gtcSecretStorage . ncSscContext <$> getNodeContext

getSecret :: SConstraint (Maybe GtSecret)
getSecret = sscStorage >>= flip queryExtended GetS

setSecret :: GtSecret -> SConstraint ()
setSecret secret = sscStorage >>= flip updateExtended (SetS secret)

prepareSecretToNewSlot :: SlotId -> SConstraint ()
prepareSecretToNewSlot  slotId = sscStorage >>= flip updateExtended (Prepare slotId)

checkpointSecret :: SConstraint ()
checkpointSecret = sscStorage >>= tidyExtendedState
