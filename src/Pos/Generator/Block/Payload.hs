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
import           Formatting                 (sformat, (%))
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
selectDistinct :: forall m. (MonadRandom m, MonadIO m) => Int -> (Int,Int) -> m [Int]
selectDistinct n0 p@(a, b)
    | b - a < 0 = error $ "selectDistinct: b < a " <> show p
    | otherwise = do
          res <- selectDistinct' [] n
          if fromIntegral (length res) /= n
              then error "selectDistinct is broken"
              else pure res
  where
    n :: Int
    n = min (b + 1 - a) n0
    selectDistinct' :: [Int] -> Int -> m [Int]
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
          -- here we can use unsafeIntegerToCoin, because amount of
          -- subcoin is less than 'c' by design.
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
        void $ replicateM txsN genTransaction
  where
    genTransaction :: StateT GenTxData (BlockGenRandMode g m) ()
    genTransaction = do
        utxoSize <- uses gtdUtxoKeys V.length
        when (utxoSize == 0) $
            -- Empty utxo means misconfigured testing settings most
            -- probably, so that's a critical error
            error "Utxo is empty already which is weird"

        secrets <- view asSecretKeys <$> view blockGenParams
        -- Unsafe hashmap resolving is used here because we suppose
        -- utxo contains only related to these secret keys only.
        let resolveSecret stId =
                fromMaybe (error $ "can't find stakeholderId " <>
                           pretty stId <> " in secrets map")
                          (HM.lookup stId secrets)
        let utxoAddresses = map (makePubKeyAddress . toPublic) $ HM.elems secrets

        ----- INPUTS

        inputsN <- getRandomR (1, min 5 utxoSize)
        inputsIxs <- selectDistinct inputsN (0, utxoSize - 1)
        -- It's alright to use unsafeIndex because length of
        -- gtdUtxoKeys must match utxo size and inputsIxs is selected
        -- prior to length limitation.
        txIns <- forM inputsIxs $ \i -> uses gtdUtxoKeys (`V.unsafeIndex` i)
        inputsResolved <- forM txIns $ \txIn ->
            -- we're selecting from utxo by 'gtdUtxoKeys'. Inability to resolve
            -- txin means that 'GenTxData' is malformed.
            toaOut . fromMaybe (error "genTxPayload: inputsSum can't happen") <$> utxoGet txIn
        let (inputsSum :: Integer) = sumCoins $ map txOutValue inputsResolved

        ----- OUTPUTS

        outputsMaxN <- fromIntegral . max 1 <$> lift (view tgpMaxOutputs)
        (outputsN :: Int) <- fromIntegral <$> getRandomR (1, min outputsMaxN inputsSum)
        outputsIxs <- selectDistinct outputsN (0, max outputsN (length utxoAddresses - 1))
        let outputAddrs = map ((cycle utxoAddresses) !!) outputsIxs
        -- We operate small coins values so any input sum mush be less
        -- than coin maxbound.
        coins <- splitCoins outputsN (unsafeIntegerToCoin inputsSum)
        let txOuts = NE.fromList $ zipWith TxOut outputAddrs coins

        ----- TX

        let txE :: Either Text Tx
            txE = mkTx (NE.fromList txIns) txOuts def
        let tx = either (\e -> error $ "genTransaction impossible: couldn't create tx: " <> e)
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
