-- | DB operations for storing and dumping GtGlobalState.

module Pos.Ssc.GodTossing.DB
       ( getGtGlobalState
       , gtGlobalStateToBatch
       , prepareGtDB
       ) where

import           Data.Default                   (def)
import qualified Data.Text.Buildable
import qualified Database.RocksDB               as Rocks
import qualified Data.Store as Store
import           Universum

import           Formatting                     (bprint, build, (%))
import           Pos.DB                         (MonadDB, RocksBatchOp (..))
import           Pos.DB.Error                   (DBError (DBMalformed))
import           Pos.DB.GState.Common           (gsGetStore, gsPutStore)
import           Pos.Ssc.GodTossing.Core        (VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..))
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Util.Util                  (maybeThrow)

getGtGlobalState :: MonadDB m => m GtGlobalState
getGtGlobalState =
    maybeThrow (DBMalformed "GodTossing global state DB is not initialized") =<<
    gsGetStore gtKey

gtGlobalStateToBatch :: GtGlobalState -> GtOp
gtGlobalStateToBatch = PutGlobalState

prepareGtDB :: MonadDB m => VssCertificatesMap -> m ()
prepareGtDB certs =
    whenNothingM_ (gsGetStore @_ @GtGlobalState gtKey) $
        gsPutStore gtKey (def {_gsVssCertificates = vcd})
  where
    vcd = VCD.fromList . toList $ certs

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data GtOp
    = PutGlobalState !GtGlobalState

instance Buildable GtOp where
    build (PutGlobalState gs) = bprint ("GtOp ("%build%")") gs

instance RocksBatchOp GtOp where
    toBatchOp (PutGlobalState gs) = [Rocks.Put gtKey (Store.encode gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

gtKey :: ByteString
gtKey = "ssc/"
