{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Monads used for explorer's toil.

module Pos.Explorer.Txp.Toil.Monad
       (
         ExplorerExtraM

       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getUtxoSum

       , putTxExtra
       , delTxExtra
       , updateAddrHistory
       , putAddrBalance
       , putUtxoSum

       , ELocalToilM
       , explorerExtraMToELocalToilM

       , EGlobalToilM
       , explorerExtraMToEGlobalToilM
       ) where

import           Universum

import           Control.Lens (at, magnify, zoom, (%=), (.=))
import           Control.Monad.Free.Church (F (..))
import           Control.Monad.Morph (generalize, hoist)
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Formatting (build, sformat, (%), stext, shown)

import           Pos.Chain.Txp (ExtendedGlobalToilM, ExtendedLocalToilM,
                     StakesLookupF, TxId)
import           Pos.Core (Address, Coin, mkCoin)
import           Pos.Core.Chrono (getNewestFirst)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import           Pos.Explorer.DB (targetAddress)
import           Pos.Explorer.Txp.Toil.Types (ExplorerExtraLookup (..),
                     ExplorerExtraModifier, eemAddrBalances, eemAddrHistories,
                     eemLocalTxsExtra, eemNewUtxoSum)
import           Pos.Util (type (~>))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Wlog (NamedPureLogger, logError, logWarning)

----------------------------------------------------------------------------
-- Monadic actions with extra txp data.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup extra values related to txp and modify them.
type ExplorerExtraM
     = ReaderT ExplorerExtraLookup (StateT ExplorerExtraModifier (NamedPureLogger Identity))

getTxExtra :: TxId -> ExplorerExtraM (Maybe TxExtra)
getTxExtra txId = do
    baseLookup <- eelGetTxExtra <$> ask
    MM.lookup baseLookup txId <$> use eemLocalTxsExtra

getAddrHistory :: Address -> ExplorerExtraM AddrHistory
getAddrHistory addr = do
    hist <- use (eemAddrHistories . at addr) >>= \case
                Nothing -> eelGetAddrHistory <$> ask <*> pure addr
                Just hist -> pure hist
    when (addr == targetAddress) $ do
        logWarning $ sformat ("XXX ToilM.getAddrHistory: "%shown) (getNewestFirst hist)
    pure hist

getAddrBalance :: Address -> ExplorerExtraM (Maybe Coin)
getAddrBalance addr = do
    baseLookup <- eelGetAddrBalance <$> ask
    mcoin <- MM.lookup baseLookup addr <$> use eemAddrBalances
    when (addr == targetAddress) $ do
        logWarning $ sformat ("XXX ToilM.getAddrBalance: "%stext) (maybe "empty" (sformat build) mcoin)

    pure mcoin

getUtxoSum :: ExplorerExtraM Integer
getUtxoSum = fromMaybe <$> (eelGetUtxoSum <$> ask) <*> use eemNewUtxoSum

putTxExtra :: TxId -> TxExtra -> ExplorerExtraM ()
putTxExtra txId extra = eemLocalTxsExtra %= MM.insert txId extra

delTxExtra :: TxId -> ExplorerExtraM ()
delTxExtra txId = eemLocalTxsExtra %= MM.delete txId

updateAddrHistory :: Address -> AddrHistory -> ExplorerExtraM ()
updateAddrHistory addr hist
    | null (getNewestFirst hist) =
        when (addr == targetAddress) $ do
            mb <- getAddrBalance addr
            h <- getAddrHistory addr
            when (isJust mb && null (getNewestFirst h)) $
                logError $ "XXX ToilM.updateAddrHistory: WAT??"
    | otherwise = do
        when (addr == targetAddress) $ do
            logWarning $ sformat ("XXX ToilM.updateAddrHistory: "%shown) (getNewestFirst hist)
        eemAddrHistories . at addr .= Just hist

putAddrBalance :: Address -> Coin -> ExplorerExtraM ()
putAddrBalance addr coin = do
    when (addr == targetAddress) $
        logWarning $ sformat ("XXX ToilM.putAddrBalance: "%build) coin
    -- If the new balance is 0 delete the entry instead of inserting it.
    if coin /= mkCoin 0
        then eemAddrBalances %= MM.insert addr coin
        else eemAddrBalances %= MM.delete addr

putUtxoSum :: Integer -> ExplorerExtraM ()
putUtxoSum utxoSum = eemNewUtxoSum .= Just utxoSum

----------------------------------------------------------------------------
-- Monad used for local Toil in Explorer.
----------------------------------------------------------------------------

type ELocalToilM = ExtendedLocalToilM ExplorerExtraLookup ExplorerExtraModifier

explorerExtraMToELocalToilM :: ExplorerExtraM ~> ELocalToilM
explorerExtraMToELocalToilM = zoom _2 . magnify _2

----------------------------------------------------------------------------
-- Monad used for global Toil in Explorer.
----------------------------------------------------------------------------

type EGlobalToilM
     = ExtendedGlobalToilM ExplorerExtraLookup ExplorerExtraModifier

explorerExtraMToEGlobalToilM :: ExplorerExtraM ~> EGlobalToilM
explorerExtraMToEGlobalToilM = mapReaderT (mapStateT f . zoom _2) . magnify _2
  where
    f :: NamedPureLogger Identity ~> NamedPureLogger (F StakesLookupF)
    f = hoist generalize
