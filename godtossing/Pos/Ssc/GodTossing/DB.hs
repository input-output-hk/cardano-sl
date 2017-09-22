-- | DB operations for storing and dumping GtGlobalState.

module Pos.Ssc.GodTossing.DB
       ( getGtGlobalState
       , gtGlobalStateToBatch
       , initGtDB
       ) where

import           Universum

import           Data.Default                   (def)
import qualified Data.Text.Buildable
import qualified Database.RocksDB               as Rocks
import           Formatting                     (bprint, build, (%))

import           Pos.Binary.GodTossing          ()
import           Pos.Core                       (HasConfiguration, genesisVssCerts)
import           Pos.DB                         (MonadDB, MonadDBRead, RocksBatchOp (..))
import           Pos.DB.Error                   (DBError (DBMalformed))
import           Pos.DB.Functions               (dbSerializeValue)
import           Pos.DB.GState.Common           (gsGetBi, gsPutBi)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..))
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Util.Util                  (maybeThrow)

getGtGlobalState :: (MonadDBRead m) => m GtGlobalState
getGtGlobalState =
    maybeThrow (DBMalformed "GodTossing global state DB is not initialized") =<<
    gsGetBi gtKey

gtGlobalStateToBatch :: GtGlobalState -> GtOp
gtGlobalStateToBatch = PutGlobalState

initGtDB :: (HasConfiguration, MonadDB m) => m ()
initGtDB = gsPutBi gtKey (def {_gsVssCertificates = vcd})
  where
    vcd = VCD.fromList . toList $ genesisVssCerts

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data GtOp
    = PutGlobalState !GtGlobalState

instance Buildable GtOp where
    build (PutGlobalState gs) = bprint ("GtOp ("%build%")") gs

instance HasConfiguration => RocksBatchOp GtOp where
    toBatchOp (PutGlobalState gs) = [Rocks.Put gtKey (dbSerializeValue gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

gtKey :: ByteString
gtKey = "ssc/"
