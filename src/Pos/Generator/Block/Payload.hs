{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO Maybe move it somewhere else.
-- | Block payload generation.

module Pos.Generator.Block.Payload
       ( genPayload
       ) where

import           Universum

import           Control.Lens               (at, uses, (%=), (.=), (?=))
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Random.Class (MonadRandom (..))
import           Data.Default               (def)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (notElem, (!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Vector                as V
import           Formatting                 (int, sformat, (%))
import           System.Random              (RandomGen (..))

import           Pos.Core                   (Address (..), Coin, SlotId (..),
                                             addressDetailedF, coinToInteger,
                                             makePubKeyAddress, sumCoins,
                                             unsafeIntegerToCoin)
import           Pos.Crypto                 (SignTag (SignTxIn), WithHash (..), hash,
                                             sign, toPublic)
import           Pos.Generator.Block.Mode   (BlockGenRandMode, MonadBlockGenBase)
import           Pos.Generator.Block.Param  (HasBlockGenParams (..), HasTxGenParams (..),
                                             asSecretKeys)
import qualified Pos.GState                 as DB
import           Pos.Txp.Core               (Tx, TxAux (..), TxDistribution (..),
                                             TxIn (..), TxInWitness (..), TxOut (..),
                                             TxOutAux (..), TxSigData (..), mkTx)
import           Pos.Txp.Logic              (txProcessTransaction)
import           Pos.Txp.Toil.Class         (MonadUtxo (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Types         (Utxo)
import qualified Pos.Txp.Toil.Utxo          as Utxo

----------------------------------------------------------------------------
-- Tx payload generation
----------------------------------------------------------------------------

-- | Generates list of distinct ints in the given range [a,b].
selectDistinct :: (MonadRandom m, MonadIO m) => Int -> (Int,Int) -> m [Int]
selectDistinct n0 p@(a, b)
    | b - a < 0 = error $ "selectDistinct: b < a " <> show p
    | otherwise = selectDistinct' [] n
  where
    n = min (b + 1 - a) n0
    selectDistinct' cur leftN = do
        leftItems <- take leftN <$> getRandomRs (a, b)
        let nubbed = ordNub $ cur ++ leftItems
        if length nubbed < n
            then selectDistinct' nubbed (n - length nubbed)
            else pure nubbed

-- | Separates coin into provided number of coins. All resulting coins
-- are nonzero.
splitCoins :: (MonadRandom m, MonadIO m) => Int -> Coin -> m [Coin]
splitCoins n (fromIntegral . coinToInteger -> c)
    | c < n = error $ "splitCoins: can't split " <> pretty c <>
                      " on " <> show n <> " parts"
    | otherwise = do
          splitPoints <- sort <$> selectDistinct n (0, c - 1)
          -- calculate length of intervals
          let amounts = map (\(a,b) -> b - a + 1) $ (0 : splitPoints) `zip` splitPoints
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

-- TODO: move to txp, think how to unite it with 'Pos.Arbitrary.Txp'.
-- | Generate valid 'TxPayload' using current global state.
genTxPayload ::
       forall g m. (RandomGen g, MonadBlockGenBase m)
    => BlockGenRandMode g m ()
genTxPayload = do
    utxo <- lift DB.getAllPotentiallyHugeUtxo
    let gtd = GenTxData utxo (V.fromList $ M.keys utxo)
    flip evalStateT gtd $ do
        (a,d) <- lift $ view tgpTxCountRange
        txsN <- fromIntegral <$> getRandomR (a, a + d)
        void $ replicateM txsN $ do
            genTransaction
  where
    genTransaction :: StateT GenTxData (BlockGenRandMode g m) ()
    genTransaction = do
        utxoSize <- uses gtdUtxoKeys V.length
        canExhaustUtxo <- lift $ view tgpAllowExhaustUtxo
        when (not canExhaustUtxo && utxoSize == 0) $
            error "Utxo exhaustion is not allowed but utxo is empty already"

        secrets <- view asSecretKeys <$> view blockGenParams
        let resolveSecret stId =
                fromMaybe (error $ "can't find stakeholderId " <>
                           pretty stId <> " in secrets map")
                          (HM.lookup stId secrets)
        let utxoAddresses = map (makePubKeyAddress . toPublic) $ HM.elems secrets

        -- INPUTS

        inputsN <- getRandomR (1, min 5 utxoSize)
        inputsIxs <- selectDistinct inputsN (0, utxoSize - 1)
        txIns <- forM inputsIxs $ \i -> uses gtdUtxoKeys (`V.unsafeIndex` i)
        unless (inputsN == length txIns) $
            error $ sformat ("txIns length "%int%" doesn't match inputsN "%int)
                            (length txIns)
                            inputsN
        inputsResolved <- forM txIns $ \txIn ->
            toaOut . fromMaybe (error "genTxPayload: inputsSum") <$> utxoGet txIn
        let (inputsSum :: Integer) = sumCoins $ map txOutValue inputsResolved
        -- at most 5 outputs too, but not bigger than amount of coins
        -- (to send 1 coin to everybody at least)

        -- OUTPUTS

        outputsMaxN <- fromIntegral . max 1 <$> lift (view tgpMaxOutputs)
        (outputsN :: Int) <- fromIntegral <$> getRandomR (1, min outputsMaxN inputsSum)
        outputsIxs <- selectDistinct outputsN (0, max outputsN (length utxoAddresses - 1))
        let outputAddrs = map ((cycle utxoAddresses) !!) outputsIxs
        unless (length outputAddrs == outputsN) $
            error $ sformat ("outputAddrs length "%int%" doesn't match outputsN "%int)
                (length outputAddrs)
                outputsN
        coins <- splitCoins outputsN (unsafeIntegerToCoin inputsSum)
        let txOuts = NE.fromList $ map (uncurry TxOut) $ outputAddrs `zip` coins
        let txE :: Either Text Tx
            txE = mkTx (NE.fromList txIns) txOuts def
        let tx = either (\e -> error $ "genTransaction: couldn't create tx: " <> e)
                        identity
                        txE
        let txId = hash tx
        let taDistribution = TxDistribution $ NE.fromList $ replicate outputsN []
        let toWitness :: TxIn -> Address -> TxInWitness
            toWitness txIn = \case
                PubKeyAddress stId _ ->
                    let sk = resolveSecret stId
                        txSig =
                            sign SignTxIn sk TxSigData { txSigInput = txIn
                                                       , txSigOutsHash = hash txOuts
                                                       , txSigDistrHash = hash taDistribution }
                    in PkWitness (toPublic sk) txSig
                other -> error $
                    sformat ("Found non-pubkey address: "%addressDetailedF) other
        let taWitness =
                V.fromList $ map (uncurry toWitness . second txOutAddress)
                                 (txIns `zip` inputsResolved)
        let txAux = TxAux { taTx = tx, .. }
        res <- lift . lift $ runExceptT $ txProcessTransaction (txId, txAux)
        case res of
            Left e  -> error $ "genTransaction@txProcessTransaction: got left: " <> pretty e
            Right () -> do
                Utxo.applyTxToUtxo (WithHash tx txId) taDistribution
                gtdUtxoKeys %= V.filter (`notElem` txIns)
                let outsAsIns = map (TxIn txId) [0..(fromIntegral outputsN)-1]
                gtdUtxoKeys %= (V.++) (V.fromList outsAsIns)


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
