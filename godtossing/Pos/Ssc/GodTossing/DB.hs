-- | DB operations for storing and dumping GtGlobalState.

module Pos.Ssc.GodTossing.DB
       ( getGtGlobalState
       , getGtGlobalStateMaybe
       , gtGlobalStateToBatch
       , initGtDB
       ) where

import           Universum

import qualified Data.Text.Buildable
import qualified Database.RocksDB         as Rocks
import           Formatting               (bprint, build, (%))

import           Pos.Binary.Class         (serialize')
import           Pos.Binary.GodTossing    ()
import           Pos.DB                   (MonadDB, MonadDBRead, RocksBatchOp (..))
import           Pos.DB.Error             (DBError (DBMalformed))
import           Pos.DB.GState.Common     (gsGetBi)
import           Pos.Ssc.GodTossing.Core  (VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types (GtGlobalState (..))
import           Pos.Util.Util            (maybeThrow)

getGtGlobalState :: MonadDBRead m => m GtGlobalState
getGtGlobalState =
    maybeThrow (DBMalformed "GodTossing global state DB is not initialized") =<<
    gsGetBi gtKey

-- For CSL-1113
getGtGlobalStateMaybe :: MonadDBRead m => m (Maybe GtGlobalState)
getGtGlobalStateMaybe = gsGetBi gtKey

gtGlobalStateToBatch :: GtGlobalState -> GtOp
gtGlobalStateToBatch = PutGlobalState

initGtDB :: MonadDB m => VssCertificatesMap -> m ()
initGtDB _ = pass
-- Commented due to CSL-1113, maybe uncomment when we will use store serialization.
  --   whenNothingM_ (gsGetBi @_ @GtGlobalState gtKey) $
  --       gsPutBi gtKey (def {_gsVssCertificates = vcd})
  -- where
  --   vcd = VCD.fromList . toList $ certs

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data GtOp
    = PutGlobalState !GtGlobalState

instance Buildable GtOp where
    build (PutGlobalState gs) = bprint ("GtOp ("%build%")") gs

instance RocksBatchOp GtOp where
    toBatchOp (PutGlobalState gs) = [Rocks.Put gtKey (serialize' gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

gtKey :: ByteString
gtKey = "ssc/"
