{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Web.Mode
    ( WebMode
    , WebModeContext(..)
    ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Mockable (Production)

import           Pos.Context (HasPrimaryKey (..), HasSscContext (..), NodeContext)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB (NodeDBs)
import           Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault,
                               dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks (dbDeleteDefault, dbGetDefault, dbIterSourceDefault, dbPutDefault,
                               dbWriteBatchDefault)
import           Pos.Txp (GenericTxpLocalData, MempoolExt, TxpHolderTag)
import           Pos.Util.Lens (postfixLFields)
import           Pos.Util.Util (HasLens (..))

data WebModeContext ext = WebModeContext
    { wmcNodeDBs      :: !NodeDBs
    , wmcTxpLocalData :: !(GenericTxpLocalData ext)
    , wmcNodeContext  :: !NodeContext
    }

makeLensesWith postfixLFields ''WebModeContext

instance HasLens NodeDBs (WebModeContext ext) NodeDBs where
    lensOf = wmcNodeDBs_L

instance HasLens TxpHolderTag (WebModeContext ext) (GenericTxpLocalData ext) where
    lensOf = wmcTxpLocalData_L

instance {-# OVERLAPPABLE #-}
    HasLens tag NodeContext r =>
    HasLens tag (WebModeContext ext) r
  where
    lensOf = wmcNodeContext_L . lensOf @tag

instance HasSscContext (WebModeContext ext) where
    sscContext = wmcNodeContext_L . sscContext

instance HasPrimaryKey (WebModeContext ext) where
    primaryKey = wmcNodeContext_L . primaryKey

type WebMode ext = Mtl.ReaderT (WebModeContext ext) Production

instance HasConfiguration => MonadDBRead (WebMode ext) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault

instance HasConfiguration => MonadDB (WebMode ext) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

type instance MempoolExt (WebMode ext) = ext
