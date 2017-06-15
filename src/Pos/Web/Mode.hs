{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Web.Mode
    ( WebMode
    , unWebMode
    , WebModeContext(..)
    ) where

import           Universum

import           Control.Lens
import qualified Control.Monad.Reader as Mtl
import           Ether.Internal       (HasLens (..))
import           Mockable             (Production)

import           Pos.Context          (NodeContext)
import qualified Pos.DB               as DB
import           Pos.DB.Class         (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Redirect      (dbDeleteReal, dbGetReal, dbPutReal,
                                       dbWriteBatchReal)
import           Pos.ExecMode         (ExecMode (..), ExecModeM)
import           Pos.Txp.MemState     (GenericTxpLocalData, TxpHolderTag)
import           Pos.WorkMode         (TxpExtra_TMP)

data WebModeContext ssc = WebModeContext
    { wmcNodeDBs     :: !DB.NodeDBs
    , wmcTxpData     :: !(GenericTxpLocalData TxpExtra_TMP)
    , wmcNodeContext :: !(NodeContext ssc)
    }

makeLensesFor
    [ ("wmcNodeContext", "wmcNodeContextL")
    , ("wmcTxpData",     "wmcTxpDataL")
    , ("wmcNodeDBs",     "wmcNodeDBsL") ]
    ''WebModeContext

instance {-# OVERLAPPABLE #-} HasLens tag (NodeContext ssc) r => HasLens tag (WebModeContext ssc) r where
    lensOf = wmcNodeContextL . lensOf @tag

instance HasLens TxpHolderTag (WebModeContext ssc) (GenericTxpLocalData TxpExtra_TMP) where
    lensOf = wmcTxpDataL

instance HasLens DB.NodeDBs (WebModeContext ssc) DB.NodeDBs where
    lensOf = wmcNodeDBsL

data WEB ssc

type WebMode ssc = ExecMode (WEB ssc)

type instance ExecModeM (WEB ssc) =
    Mtl.ReaderT (WebModeContext ssc) Production

unWebMode :: ExecMode (WEB ssc) a -> ExecModeM (WEB ssc) a
unWebMode = unExecMode

instance MonadDBRead (WebMode ssc) where
    dbGet = dbGetReal

instance MonadDB (WebMode ssc) where
    dbPut = dbPutReal
    dbWriteBatch = dbWriteBatchReal
    dbDelete = dbDeleteReal
