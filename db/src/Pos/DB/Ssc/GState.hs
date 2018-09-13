-- | DB operations for storing and dumping SscGlobalState.

module Pos.DB.Ssc.GState
       ( getSscGlobalState
       , sscGlobalStateToBatch
       , initSscDB
       ) where

import           Universum

import           Data.Default (def)
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Ssc (SscGlobalState (..))
import qualified Pos.Chain.Ssc as VCD
import           Pos.Core.Ssc (VssCertificatesMap)
import           Pos.DB (MonadDB, MonadDBRead, RocksBatchOp (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.Util.Util (maybeThrow)

getSscGlobalState :: (MonadDBRead m) => m SscGlobalState
getSscGlobalState =
    maybeThrow (DBMalformed "SSC global state DB is not initialized") =<<
    gsGetBi sscKey

sscGlobalStateToBatch :: SscGlobalState -> SscOp
sscGlobalStateToBatch = PutGlobalState

initSscDB :: (MonadDB m) => VssCertificatesMap -> m ()
initSscDB genesisVssCerts = gsPutBi sscKey (def {_sgsVssCertificates = vcd})
  where
    vcd = VCD.fromList . toList $ genesisVssCerts

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data SscOp
    = PutGlobalState !SscGlobalState

instance Buildable SscOp where
    build (PutGlobalState gs) = bprint ("SscOp ("%build%")") gs

instance RocksBatchOp SscOp where
    toBatchOp (PutGlobalState gs) = [Rocks.Put sscKey (serialize' gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

sscKey :: ByteString
sscKey = "ssc/"
