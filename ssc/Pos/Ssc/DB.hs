-- | DB operations for storing and dumping SscGlobalState.

module Pos.Ssc.DB
       ( getSscGlobalState
       , sscGlobalStateToBatch
       , initSscDB
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
    maybeThrow (DBMalformed "SSC global state DB is not initialized") =<<
    gsGetBi sscKey

sscGlobalStateToBatch :: SscGlobalState -> SscOp
sscGlobalStateToBatch = PutGlobalState

initSscDB :: (HasConfiguration, MonadDB m) => m ()
initSscDB = gsPutBi sscKey (def {_sgsVssCertificates = vcd})
  where
    vcd = VCD.fromList . toList $ genesisVssCerts

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data SscOp
    = PutGlobalState !SscGlobalState

instance Buildable SscOp where
    build (PutGlobalState gs) = bprint ("SscOp ("%build%")") gs

instance HasConfiguration => RocksBatchOp SscOp where
    toBatchOp (PutGlobalState gs) = [Rocks.Put sscKey (dbSerializeValue gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

sscKey :: ByteString
sscKey = "ssc/"
