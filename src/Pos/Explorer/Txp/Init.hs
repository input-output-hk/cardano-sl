-- | Routines for initialization of Explorer's DB.

module Pos.Explorer.Txp.Init
       ( initializeGenesisBalances
       ) where

import           Universum

import qualified Data.Map.Strict             as M

import           Pos.Binary.Class            (encodeStrict)
import           Pos.Context.Class           (WithNodeContext)
import           Pos.Context.Functions       (genesisUtxoM)
import           Pos.DB.Class                (MonadDB)
import           Pos.DB.GState.Common        (gsGetBi, gsPutBi)
import           Pos.Explorer.Txp.Toil.Class (MonadTxExtra (..))
import           Pos.Txp.Core                (TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil                (Utxo)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initializeGenesisBalances :: (WithNodeContext ssc m, MonadDB m) => m ()
initializeGenesisBalances =
    unlessM hasInitMarker $ do
        genesisUtxo <- genesisUtxoM
        putGenesisBalances genesisUtxo
        putInitMarker

initMarker :: ByteString
initMarker = "e/init"

hasInitMarker :: MonadDB m => m Bool
hasInitMarker = do
    maybeUnit <- gsGetBi initMarker
    pure $ isJust (maybeUnit :: Maybe ())

putInitMarker :: MonadDB m => m ()
putInitMarker = gsPutBi initMarker ()

putGenesisBalances :: MonadTxExtra m => Utxo -> m ()
putGenesisBalances genesisUtxo = do
    let txOuts = map (toaOut . snd) . M.toList $ genesisUtxo
    forM_ txOuts $ \txOut ->
        putAddrBalance (txOutAddress txOut) (txOutValue txOut)
