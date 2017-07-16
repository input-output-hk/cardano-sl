{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO Maybe move it somewhere else.
-- | Block payload generation.

module Pos.Generator.Block.Payload
       ( genPayload
       ) where

import           Universum

import           Control.Lens               (at, uses, (.=), (?=))
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Random.Class (MonadRandom (..))
import           Data.Default               (def)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  ((!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Vector                as V
import           System.Random              (RandomGen (..))

import           Pos.Core                   (Coin, SlotId (..), addressHash,
                                             coinToInteger, makePubKeyAddress, sumCoins,
                                             unsafeIntegerToCoin)
import           Pos.Crypto                 (WithHash (..), hash, toPublic)
import           Pos.Generator.Block.Mode   (BlockGenMode, BlockGenRandMode,
                                             MonadBlockGen, MonadBlockGenBase,
                                             mkBlockGenContext, usingPrimaryKey,
                                             withCurrentSlot)
import           Pos.Generator.Block.Param  (AllSecrets, HasBlockGenParams (..),
                                             asSecretKeys)
import qualified Pos.GState                 as DB
import           Pos.Txp.Core               (Tx, TxAux (..), TxIn (..), TxOut (..),
                                             TxOutAux (..), mkTx)
import           Pos.Txp.Logic              (txProcessTransaction)
import           Pos.Txp.Toil.Class         (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Types         (Utxo)
import qualified Pos.Txp.Toil.Utxo          as Utxo

----------------------------------------------------------------------------
-- Tx payload generation
----------------------------------------------------------------------------

-- Generates list of distinct ints in the given range.
selectDistinct :: (MonadRandom m) => Int -> (Int,Int) -> m [Int]
selectDistinct n p@(a, b)
    | b - a < 0 = error $ "selectDistinct: b < a " <> show p
    | otherwise = selectDistinct' [] (min (b - a) n)
  where
    selectDistinct' cur leftN = do
        leftItems <- take leftN <$> getRandomRs (a, b)
        let nubbed = ordNub $ cur ++ leftItems
        if length nubbed < n
            then selectDistinct' nubbed (leftN - length nubbed)
            else pure nubbed

-- Separate coin into provided number of coins. All resulting coins
-- are nonzero.
splitCoins :: (MonadRandom m) => Int -> Coin -> m [Coin]
splitCoins n (fromIntegral . coinToInteger -> c)
    | c < n = error $ "splitCoins: can't split " <> pretty c <>
                      " on " <> show n <> " parts"
    | otherwise = do
          splitPoints <- sort <$> selectDistinct n (0, c - 1)
          let amounts = map (uncurry (+)) $ (0 : splitPoints) `zip` splitPoints
          pure $ map (unsafeIntegerToCoin . fromIntegral) amounts

-- | State datatype for transaction payload generation
data GenTxData = GenTxData
    { _gtdUtxo     :: Utxo
      -- ^ Utxo as it is.
    , _gtdUtxoKeys :: V.Vector TxIn
      -- ^ Keys of 'gtdUtxo' to support random selection by index.
    }

makeLenses ''GenTxData

instance (Monad m) => MonadUtxoRead (StateT GenTxData m) where
    utxoGet txIn = uses gtdUtxo $ M.lookup txIn

instance (Monad m) => MonadUtxo (StateT GenTxData m) where
    utxoPut txIn txOutAux = gtdUtxo . at txIn ?= txOutAux
    utxoDel txIn = gtdUtxo . at txIn .= Nothing

-- TODO: add randomness, implement, move to txp, think how to unite it
-- with 'Pos.Txp.Arbitrary'.
-- Generate valid 'TxPayload' using current global state.
genTxPayload ::
       forall g m. (RandomGen g, MonadBlockGenBase m)
    => BlockGenRandMode g m ()
genTxPayload = do
    utxo <- lift DB.getAllPotentiallyHugeUtxo
    let gtd = GenTxData utxo (V.fromList $ M.keys utxo)
    flip evalStateT gtd $ do
        txsN <- getRandomR (1, 100)
        void $ replicateM txsN genTransaction
  where
    genTransaction :: StateT GenTxData (BlockGenRandMode g m) ()
    genTransaction = do
        utxoSize <- uses gtdUtxoKeys V.length
        -- at most 5 inputs
        inputsN <- getRandomR (0, min 5 utxoSize)
        inputsIxs <- selectDistinct inputsN (0, utxoSize - 1)
        txIns <- forM inputsIxs $ \i -> uses gtdUtxoKeys (`V.unsafeIndex` i)
        (inputsSum :: Integer) <- fmap sumCoins $ forM txIns $ \txIn -> do
            out <- fromMaybe (error "genTxPayload: inputsSum") <$> utxoGet txIn
            pure $ txOutValue $ toaOut out
        secrets <- view asSecretKeys <$> view blockGenParams
        let utxoAddresses = map (makePubKeyAddress . toPublic) $ HM.elems secrets
        -- at most 5 outputs too, but not bigger than amount of coins
        -- (to send 1 coin to everybody at least)
        (outputsN :: Int) <- fromIntegral <$> getRandomR (1, min 5 inputsSum)
        outputsIxs <- selectDistinct outputsN (0, length utxoAddresses - 1)
        let outputAddrs = map (utxoAddresses !!) outputsIxs
        coins <- splitCoins outputsN (unsafeIntegerToCoin inputsSum)
        let txOuts = map (uncurry TxOut) $ outputAddrs `zip` coins
        let txE :: Either Text Tx
            txE = mkTx (NE.fromList txIns) (NE.fromList txOuts) def
        let tx = either (\e -> error $ "genTransaction: couldn't create tx: " <> e)
                        identity
                        txE
        let txAux = undefined tx
        let txId = hash tx
        res <- lift . lift $ runExceptT $ txProcessTransaction (txId, txAux)
        case res of
            Left e  -> error $ "genTransaction@txProcessTransaction: got left: " <> pretty e
            Right () -> do
                let (TxAux tx _ distr) = txAux
                Utxo.applyTxToUtxo (WithHash tx txId) distr
                -- todo also modify gtdUtxoKeys


----------------------------------------------------------------------------
-- Payload generation
----------------------------------------------------------------------------

-- Generate random payload which is valid with respect to the current
-- global state and mempool and add it to mempool.  Currently we are
-- concerned only about tx payload, later we can add more stuff.
genPayload ::
       forall g m.
       (RandomGen g, MonadBlockGenBase m)
    => SlotId
    -> BlockGenRandMode g m ()
genPayload _ = genTxPayload
