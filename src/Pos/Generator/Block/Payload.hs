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
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (notElem, (!!))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Vector                as V
import           Formatting                 (sformat, (%))
import           System.Random              (RandomGen (..))

import           Pos.Client.Txp.Util        (makeAbstractTx, overrideTxDistrBoot)
import           Pos.Context                (genesisStakeholdersM)
import           Pos.Core                   (Address (..), Coin, SlotId (..),
                                             addressDetailedF, coinToInteger,
                                             makePubKeyAddress, sumCoins,
                                             unsafeIntegerToCoin)
import           Pos.Crypto                 (SecretKey, SignTag (SignTxIn), WithHash (..),
                                             hash, sign, toPublic)
import           Pos.DB                     (gsIsBootstrapEra)
import           Pos.Generator.Block.Error  (BlockGenError (..))
import           Pos.Generator.Block.Mode   (BlockGenRandMode, MonadBlockGenBase)
import           Pos.Generator.Block.Param  (HasBlockGenParams (..), HasTxGenParams (..),
                                             asSecretKeys)
import qualified Pos.GState                 as DB
import           Pos.Slotting.Class         (MonadSlots (getCurrentSlotBlocking))
import           Pos.Txp.Core               (TxAux (..), TxIn (..), TxInWitness (..),
                                             TxOut (..), TxOutAux (..), TxSigData (..))
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
splitCoins n c0
    | c == (0::Int) = error "splitCoins, c = 0"
    | c == (1::Int) = pure [c0]
    | c < n = error $ "splitCoins: can't split " <> pretty c0 <>
                      " on " <> show n <> " parts"
    | otherwise = do
          splitPoints <- sort <$> selectDistinct (n-1) (1, c - 1)
          -- calculate length of intervals
          let amounts = map (\(a,b) -> b - a) $
                            (0 : splitPoints) `zip` (splitPoints ++ [c])
          -- here we can use unsafeIntegerToCoin, because amount of
          -- subcoin is less than 'c' by design.
          pure $ map (unsafeIntegerToCoin . fromIntegral) amounts
  where
    c :: Integral a => a
    c = fromIntegral $ coinToInteger c0

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
        epoch <- siEpoch <$> lift (lift getCurrentSlotBlocking)
        bootEra <- lift . lift $ gsIsBootstrapEra epoch
        genStakeholders <- toList <$> genesisStakeholdersM
        let dustThd :: Integral a => a
            dustThd = fromIntegral $ length genStakeholders
        utxoSize <- uses gtdUtxoKeys V.length
        when (utxoSize == 0) $
            lift $ throwM $ BGInternal "Utxo is empty when trying to create tx payload"

        secrets <- view asSecretKeys <$> view blockGenParams
        -- Unsafe hashmap resolving is used here because we suppose
        -- utxo contains only related to these secret keys only.
        let resolveSecret stId =
                fromMaybe (error $ "can't find stakeholderId " <>
                           pretty stId <> " in secrets map")
                          (HM.lookup stId secrets)
        let utxoAddresses = map (makePubKeyAddress . toPublic) $ HM.elems secrets

        ----- INPUTS

        let generateInputs = do
                inputsN <- getRandomR (1, min 5 utxoSize)
                inputsIxs <- selectDistinct inputsN (0, utxoSize - 1)
                -- It's alright to use unsafeIndex because length of
                -- gtdUtxoKeys must match utxo size and inputsIxs is selected
                -- prior to length limitation.
                txIns <- forM inputsIxs $ \i -> uses gtdUtxoKeys (`V.unsafeIndex` i)
                inputsResolved <- forM txIns $ \txIn ->
                    -- we're selecting from utxo by 'gtdUtxoKeys'. Inability to resolve
                    -- txin means that 'GenTxData' is malformed.
                    toaOut .
                    fromMaybe (error "genTxPayload: inputsSum can't happen") <$>
                    utxoGet txIn
                let (inputsSum :: Integer) = sumCoins $ map txOutValue inputsResolved
                -- if we've just took inputs that have sum less than number
                -- of stakeholders, it's dust case and we forbid these txs
                -- in boot era
                if bootEra && inputsSum < dustThd
                    -- just retry
                    -- should we also check here that there are inputs in utxo that we can take?
                    then generateInputs
                    else pure (txIns, inputsResolved, inputsSum)
        (txIns,inputsResolved,inputsSum) <- generateInputs

        ----- OUTPUTS

        let generateOutputs = do
                -- this is max number of outputs such that none of
                -- them is less than dust treshold
                let ceilBoot = inputsSum `div` dustThd
                outputsMaxN <-
                    bool identity (min ceilBoot) bootEra .
                    fromIntegral .
                    max 1 <$>
                    lift (view tgpMaxOutputs)
                (outputsN :: Int) <-
                    fromIntegral <$> getRandomR (1, min outputsMaxN inputsSum)
                outputsIxs <-
                    selectDistinct
                        outputsN
                        (0, max outputsN (length utxoAddresses - 1))
                let outputAddrs = map ((cycle utxoAddresses) !!) outputsIxs
                let suchThat cond x =
                        x >>= \y -> bool (suchThat cond x) (pure y) (cond y)
                let moreThanDust (coinToInteger -> c) = c >= dustThd
                -- We operate small coins values so any input sum mush be less
                -- than coin maxbound.
                coins <-
                    suchThat (all moreThanDust) $
                    splitCoins outputsN (unsafeIntegerToCoin inputsSum)
                let txOuts = NE.fromList $ zipWith TxOut outputAddrs coins
                let txOutAuxsPre = map (\o -> TxOutAux o []) txOuts
                either (lift . throwM . BGFailedToCreate) pure =<<
                    runExceptT (overrideTxDistrBoot txOutAuxsPre)
        txOutAuxs <- generateOutputs

        ----- TX

        let resolveSk :: Address -> SecretKey
            resolveSk = \case
                PubKeyAddress stId _ -> resolveSecret stId
                other -> error $
                    sformat ("Found non-pubkey address: "%addressDetailedF) other
        let resolvedSks = map (resolveSk . txOutAddress) inputsResolved
        let txInsWithSks = NE.fromList $ resolvedSks `zip` txIns
        let mkWit :: SecretKey -> TxSigData -> TxInWitness
            mkWit sk txSigData = PkWitness (toPublic sk) (sign SignTxIn sk txSigData)
        let txAux = makeAbstractTx mkWit txInsWithSks txOutAuxs
        let tx = taTx txAux
        let txId = hash tx
        res <- lift . lift $ runExceptT $ txProcessTransaction (txId, txAux)
        case res of
            Left e  -> error $ "genTransaction@txProcessTransaction: got left: " <> pretty e
            Right () -> do
                Utxo.applyTxToUtxo (WithHash tx txId) (taDistribution txAux)
                gtdUtxoKeys %= V.filter (`notElem` txIns)
                let outsAsIns =
                        map (TxIn txId) [0..(fromIntegral $ length txOutAuxs)-1]
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
