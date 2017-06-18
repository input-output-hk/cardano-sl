{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Web.Mode
    ( WebMode
    , unWebMode
    , WebModeContext(..)
    ) where

import qualified Control.Monad.Reader as Mtl
import           Mockable             (Production)

import           Pos.Context          (NodeContext)
import           Pos.DB               (NodeDBs)
import           Pos.DB.Class         (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Redirect      (dbDeleteDefault, dbGetDefault, dbPutDefault,
                                       dbWriteBatchDefault)
import           Pos.ExecMode         ((:::), ExecMode (..), ExecModeM, modeContext)
import           Pos.Txp.MemState     (GenericTxpLocalData, TxpHolderTag)
import           Pos.WorkMode         (TxpExtra_TMP)

modeContext [d|
    data WebModeContext ssc = WebModeContext
        !(NodeDBs      ::: NodeDBs)
        !(TxpHolderTag ::: GenericTxpLocalData TxpExtra_TMP)
        !(NodeContext ssc)
    |]

data WEB ssc

type WebMode ssc = ExecMode (WEB ssc)

type instance ExecModeM (WEB ssc) =
    Mtl.ReaderT (WebModeContext ssc) Production

unWebMode :: ExecMode (WEB ssc) a -> ExecModeM (WEB ssc) a
unWebMode = unExecMode

instance MonadDBRead (WebMode ssc) where
    dbGet = dbGetDefault

instance MonadDB (WebMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
