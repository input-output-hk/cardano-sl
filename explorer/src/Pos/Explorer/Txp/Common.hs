-- | Impure functions which are used by both local and global txp in
-- explorer.

module Pos.Explorer.Txp.Common
       ( buildExplorerExtraLookup
       ) where

import           Universum hiding (keys)

import           Control.Lens (at, non)
import qualified Data.HashMap.Strict as HM

import           Pos.Core (Address, Coin)
import           Pos.Core.Txp (Tx (..), TxAux (..), toaOut, txOutAddress)
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.Explorer.DB as ExDB
import           Pos.Txp.Toil (Utxo)
import           Pos.Util.Chrono (NewestFirst (..))

import           Pos.Explorer.Core (AddrHistory)
import           Pos.Explorer.Txp.Toil (ExplorerExtraLookup (..))

-- | Build 'ExplorerExtraLookup' for given transactions using access
-- to DB and 'Utxo' corresponding to inputs of these transactions.
buildExplorerExtraLookup ::
       forall m. MonadDBRead m
    => Utxo
    -> [TxAux]
    -> m ExplorerExtraLookup
buildExplorerExtraLookup utxo txAuxes = do
    utxoSum <- ExDB.getUtxoSum
    (histories, balances) <- concatMapM buildHistoriesAndBalances txAuxes
    -- `ExplorerExtraLookup` is passed to `eProcessTx` where it is
    -- used in a ReaderT environment to provide underlying functions
    -- (`modifyAddrHistory` and `modifyAddrBalance`) with data to
    -- update. In case of `TxExtra` data is only added, but never
    -- updated, hence 'const Nothing' is used as 'eelGetTxExtra'.
    return
        ExplorerExtraLookup
            { eelGetTxExtra = const Nothing
            , eelGetAddrHistory =
                \addr -> histories ^. at addr . non (NewestFirst [])
            , eelGetAddrBalance = \addr -> balances ^. at addr
            , eelGetUtxoSum = utxoSum
            }
  where
    buildHistoriesAndBalances :: TxAux -> m (HashMap Address AddrHistory, HashMap Address Coin)
    buildHistoriesAndBalances txAux = do
        let UnsafeTx {..} = taTx txAux
        let txInAddrs = map (txOutAddress . toaOut) $ toList utxo
            txOutAddrs = toList $ map txOutAddress _txOutputs
            allAddrs = ordNub $ txInAddrs <> txOutAddrs
        histories <-
            buildMap allAddrs <$> mapM (fmap Just . ExDB.getAddrHistory) allAddrs
        balances <- buildMap allAddrs <$> mapM ExDB.getAddrBalance allAddrs
        return (histories, balances)

    buildMap :: (Eq a, Hashable a) => [a] -> [Maybe b] -> HM.HashMap a b
    buildMap keys maybeValues =
        HM.fromList $
        catMaybes $ toList $ zipWith (liftM2 (,) . Just) keys maybeValues
