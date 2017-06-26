{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Web.Mode
    ( WebMode
    , WebModeContext(..)
    ) where

import           Universum

import qualified Control.Monad.Reader as Mtl
import           Mockable             (Production)

import           Pos.Context          (HasSscContext (..), NodeContext)
import           Pos.DB               (NodeDBs)
import           Pos.DB.Class         (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Redirect      (dbDeleteDefault, dbGetDefault, dbIterSourceDefault,
                                       dbPutDefault, dbWriteBatchDefault)
import           Pos.ExecMode.Context ((:::), modeContext)
import           Pos.Txp.MemState     (GenericTxpLocalData, TxpHolderTag)
import           Pos.WorkMode         (TxpExtra_TMP)

modeContext [d|
    data WebModeContext ssc = WebModeContext
        !(NodeDBs      ::: NodeDBs)
        !(TxpHolderTag ::: GenericTxpLocalData TxpExtra_TMP)
        !(NodeContext ssc)
    |]

wmcNodeContext :: Lens' (WebModeContext ssc) (NodeContext ssc)
wmcNodeContext f (WebModeContext x1 x2 nc) = WebModeContext x1 x2 <$> f nc

instance HasSscContext ssc (WebModeContext ssc) where
    sscContext = wmcNodeContext . sscContext

type WebMode ssc = Mtl.ReaderT (WebModeContext ssc) Production

instance MonadDBRead (WebMode ssc) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance MonadDB (WebMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
