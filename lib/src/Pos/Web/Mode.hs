{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Web.Mode
    ( WebMode
    , WebModeContext(..)
    ) where

import           Universum

import           Control.Lens           (makeLensesWith)
import qualified Control.Monad.Reader   as Mtl
import           Ether.Internal         (HasLens (..))
import           Mockable               (Production)

import           Pos.Context            (HasPrimaryKey (..), HasSscContext (..),
                                         NodeContext)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB                 (NodeDBs)
import           Pos.DB.Class           (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks           (dbDeleteDefault, dbGetDefault,
                                         dbIterSourceDefault, dbPutDefault,
                                         dbWriteBatchDefault)
import           Pos.Txp                (GenericTxpLocalData, MempoolExt, TxpHolderTag)
import           Pos.Util.Util          (postfixLFields)

data WebModeContext ssc ext = WebModeContext
    { wmcNodeDBs      :: !NodeDBs
    , wmcTxpLocalData :: !(GenericTxpLocalData ext)
    , wmcNodeContext  :: !(NodeContext ssc)
    }

makeLensesWith postfixLFields ''WebModeContext

instance HasLens NodeDBs (WebModeContext ssc ext) NodeDBs where
    lensOf = wmcNodeDBs_L

instance HasLens TxpHolderTag (WebModeContext ssc ext) (GenericTxpLocalData ext) where
    lensOf = wmcTxpLocalData_L

instance {-# OVERLAPPABLE #-}
    HasLens tag (NodeContext ssc) r =>
    HasLens tag (WebModeContext ssc ext) r
  where
    lensOf = wmcNodeContext_L . lensOf @tag

instance HasSscContext ssc (WebModeContext ssc ext) where
    sscContext = wmcNodeContext_L . sscContext

instance HasPrimaryKey (WebModeContext ssc ext) where
    primaryKey = wmcNodeContext_L . primaryKey

type WebMode ssc ext = Mtl.ReaderT (WebModeContext ssc ext) Production

instance HasConfiguration => MonadDBRead (WebMode ssc ext) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance HasConfiguration => MonadDB (WebMode ssc ext) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

type instance MempoolExt (WebMode ssc ext) = ext
