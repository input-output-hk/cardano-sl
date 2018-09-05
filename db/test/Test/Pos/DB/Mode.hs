{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Pos.DB.Mode
       ( TestMode
       , runTestMode
       ) where


import           Universum

import           Control.Lens (makeLenses)

import           Pos.DB (MonadDB (..), MonadDBRead (..), NodeDBs, closeNodeDBs,
                     dbDeleteDefault, dbGetDefault, dbIterSourceDefault,
                     dbPutDefault, dbWriteBatchDefault, deleteNodeDBs,
                     openNodeDBs)
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault)
import           Pos.Util.Util (HasLens (..))


--------------------------------------------------------------------------------
-- | We are forced to introduce a @TestMode@ and @TestContext@ because of the
--   @MonadRealDB ctx m@ based design. A neater alternative might be to have a
--   @DB.Handle@ containing the @NodeDBs@, which is passed explicitly.
--
newtype TestMode a = TestMode
    { unTestMode :: ReaderT TestContext IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadCatch
               , MonadIO
               , MonadThrow
               , MonadReader TestContext
               )

runTestMode :: TestMode a -> IO a
runTestMode testMode =
    bracket acquire release $ runReaderT (unTestMode testMode) . TestContext
  where
    acquire = openNodeDBs True "test-db"
    release nodeDBs = do
        closeNodeDBs nodeDBs
        deleteNodeDBs nodeDBs

instance MonadDBRead TestMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

instance MonadDB TestMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

data TestContext = TestContext
    { _tcNodeDBs :: NodeDBs
    }

makeLenses ''TestContext

instance HasLens NodeDBs TestContext NodeDBs where
    lensOf = tcNodeDBs
