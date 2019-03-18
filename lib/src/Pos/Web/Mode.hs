{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Web.Mode
    ( WebMode
    , WebModeContext(..)
    ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           System.IO.Unsafe (unsafePerformIO)

import           Pos.Chain.Update (UpdateConfiguration)
import           Pos.Context (HasPrimaryKey (..), HasSscContext (..),
                     NodeContext)
import           Pos.DB (NodeDBs)
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks (dbDeleteDefault, dbGetDefault,
                     dbIterSourceDefault, dbPutDefault, dbWriteBatchDefault)
import           Pos.DB.Txp (GenericTxpLocalData, MempoolExt, TxpHolderTag)
import           Pos.Util.Lens (postfixLFields)
import           Pos.Util.Util (HasLens (..))
import           Pos.DB.Epoch.Index (IndexCache, mkIndexCache)

data WebModeContext ext = WebModeContext
    { wmcNodeDBs             :: !NodeDBs
    , wmcTxpLocalData        :: !(GenericTxpLocalData ext)
    , wmcNodeContext         :: !NodeContext
    , wmcUpdateConfiguration :: !UpdateConfiguration
    }

makeLensesWith postfixLFields ''WebModeContext

instance HasLens NodeDBs (WebModeContext ext) NodeDBs where
    lensOf = wmcNodeDBs_L

instance HasLens TxpHolderTag (WebModeContext ext) (GenericTxpLocalData ext) where
    lensOf = wmcTxpLocalData_L

instance HasLens UpdateConfiguration (WebModeContext ext) UpdateConfiguration where
    lensOf = wmcUpdateConfiguration_L

instance {-# OVERLAPPABLE #-}
    HasLens tag NodeContext r =>
    HasLens tag (WebModeContext ext) r
  where
    lensOf = wmcNodeContext_L . lensOf @tag

instance HasSscContext (WebModeContext ext) where
    sscContext = wmcNodeContext_L . sscContext

instance HasPrimaryKey (WebModeContext ext) where
    primaryKey = wmcNodeContext_L . primaryKey

type WebMode ext = Mtl.ReaderT (WebModeContext ext) IO

-- how would i embed this value inside the WebMode structure? (which i would have to turn into a data record?)
{-# NOINLINE unsafeCache #-}
unsafeCache :: IndexCache
unsafeCache = unsafePerformIO $ mkIndexCache 10

instance MonadDBRead (WebMode ext) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault unsafeCache
    dbGetSerUndo = dbGetSerUndoRealDefault unsafeCache
    dbGetSerBlund = dbGetSerBlundRealDefault unsafeCache

instance MonadDB (WebMode ext) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

type instance MempoolExt (WebMode ext) = ext
