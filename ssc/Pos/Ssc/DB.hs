-- | DB operations for storing and dumping SscGlobalState.

module Pos.Ssc.DB
       ( getSscGlobalState
       , gtGlobalStateToBatch
       , initGtDB
       ) where

import           Universum

import           Data.Default                   (def)
import qualified Data.Text.Buildable
import qualified Database.RocksDB               as Rocks
import           Formatting                     (bprint, build, (%))

import           Pos.Binary.Ssc                 ()
import           Pos.Core                       (HasConfiguration, genesisVssCerts)
import           Pos.DB                         (MonadDB, MonadDBRead, RocksBatchOp (..))
import           Pos.DB.Error                   (DBError (DBMalformed))
import           Pos.DB.Functions               (dbSerializeValue)
import           Pos.DB.GState.Common           (gsGetBi, gsPutBi)
import           Pos.Ssc.Types                  (SscGlobalState (..))
import qualified Pos.Ssc.VssCertData            as VCD
import           Pos.Util.Util                  (maybeThrow)

getSscGlobalState :: (MonadDBRead m) => m SscGlobalState
getSscGlobalState =
    maybeThrow (DBMalformed "GodTossing global state DB is not initialized") =<<
    gsGetBi gtKey

gtGlobalStateToBatch :: SscGlobalState -> GtOp
gtGlobalStateToBatch = PutGlobalState

initGtDB :: (HasConfiguration, MonadDB m) => m ()
initGtDB = gsPutBi gtKey (def {_sgsVssCertificates = vcd})
  where
    vcd = VCD.fromList . toList $ genesisVssCerts

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data GtOp
    = PutGlobalState !SscGlobalState

instance Buildable GtOp where
    build (PutGlobalState gs) = bprint ("GtOp ("%build%")") gs

instance HasConfiguration => RocksBatchOp GtOp where
    toBatchOp (PutGlobalState gs) = [Rocks.Put gtKey (dbSerializeValue gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

gtKey :: ByteString
gtKey = "ssc/"
