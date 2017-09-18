{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Web.Mode
    ( WebMode
    , WebModeContext(..)
    ) where

import           Universum

import           Control.Lens         (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal       (HasLens (..))
import           Mockable             (Production)

import           Pos.Context          (HasPrimaryKey (..), HasSscContext (..),
                                       NodeContext)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB               (NodeDBs)
import           Pos.DB.Class         (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks         (dbDeleteDefault, dbGetDefault, dbIterSourceDefault,
                                       dbPutDefault, dbWriteBatchDefault)
import           Pos.Txp.MemState     (GenericTxpLocalData, TxpHolderTag)
import           Pos.Util.Util        (postfixLFields)
import           Pos.WorkMode         (TxpExtra_TMP)

data WebModeContext ssc = WebModeContext
    { wmcNodeDBs      :: !NodeDBs
    , wmcTxpLocalData :: !(GenericTxpLocalData TxpExtra_TMP)
    , wmcNodeContext  :: !(NodeContext ssc)
    }

makeLensesWith postfixLFields ''WebModeContext

instance HasLens NodeDBs (WebModeContext ssc) NodeDBs where
    lensOf = wmcNodeDBs_L

instance HasLens TxpHolderTag (WebModeContext ssc) (GenericTxpLocalData TxpExtra_TMP) where
    lensOf = wmcTxpLocalData_L

instance {-# OVERLAPPABLE #-}
    HasLens tag (NodeContext ssc) r =>
    HasLens tag (WebModeContext ssc) r
  where
    lensOf = wmcNodeContext_L . lensOf @tag

instance HasSscContext ssc (WebModeContext ssc) where
    sscContext = wmcNodeContext_L . sscContext

instance HasPrimaryKey (WebModeContext ssc) where
    primaryKey = wmcNodeContext_L . primaryKey

type WebMode ssc = Mtl.ReaderT (WebModeContext ssc) Production

instance HasConfiguration => MonadDBRead (WebMode ssc) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance HasConfiguration => MonadDB (WebMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
