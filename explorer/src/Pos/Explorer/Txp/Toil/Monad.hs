{-# LANGUAGE TypeFamilies #-}
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
       , delAddrBalance
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
import           System.Wlog (NamedPureLogger)

import           Pos.Core (Address, Coin, TxId)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import           Pos.Explorer.Txp.Toil.Types (ExplorerExtraLookup (..), ExplorerExtraModifier,
                                              eemAddrBalances, eemAddrHistories, eemLocalTxsExtra,
                                              eemNewUtxoSum)
import           Pos.Txp.Toil (ExtendedGlobalToilM, ExtendedLocalToilM, StakesLookupF)
import qualified Pos.Util.Modifier as MM
import           Pos.Util (type (~>))

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
    use (eemAddrHistories . at addr) >>= \case
        Nothing -> eelGetAddrHistory <$> ask <*> pure addr
        Just hist -> pure hist

getAddrBalance :: Address -> ExplorerExtraM (Maybe Coin)
getAddrBalance addr = do
    baseLookup <- eelGetAddrBalance <$> ask
    MM.lookup baseLookup addr <$> use eemAddrBalances

getUtxoSum :: ExplorerExtraM Integer
getUtxoSum = fromMaybe <$> (eelGetUtxoSum <$> ask) <*> use eemNewUtxoSum

putTxExtra :: TxId -> TxExtra -> ExplorerExtraM ()
putTxExtra txId extra = eemLocalTxsExtra %= MM.insert txId extra

delTxExtra :: TxId -> ExplorerExtraM ()
delTxExtra txId = eemLocalTxsExtra %= MM.delete txId

updateAddrHistory :: Address -> AddrHistory -> ExplorerExtraM ()
updateAddrHistory addr hist = eemAddrHistories . at addr .= Just hist

putAddrBalance :: Address -> Coin -> ExplorerExtraM ()
putAddrBalance addr coin = eemAddrBalances %= MM.insert addr coin

delAddrBalance :: Address -> ExplorerExtraM ()
delAddrBalance addr = eemAddrBalances %= MM.delete addr

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
