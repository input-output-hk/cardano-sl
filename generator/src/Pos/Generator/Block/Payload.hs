{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- TODO Maybe move it somewhere else.
-- | Block payload generation.

module Pos.Generator.Block.Payload
       ( genPayload
       ) where

import           Universum

import           Control.Lens (at, uses, (%=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Random.Class (MonadRandom (..))
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V
import           Formatting (build, sformat, (%))
import           System.Random (RandomGen (..))

import           Pos.AllSecrets (asSecretKeys, asSpendingData, unInvAddrSpendingData,
                                 unInvSecretsMap)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), TxError (..), createGenericTx,
                                      makeMPubKeyTxAddrs)
import           Pos.Core (AddrSpendingData (..), Address (..), Coin, SlotId (..), addressHash,
                           coinToInteger, makePubKeyAddressBoot, unsafeIntegerToCoin)
import           Pos.Core.NetworkMagic (makeNetworkMagic)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (ProtocolMagic, SecretKey, WithHash (..), fakeSigner, hash, toPublic)
import           Pos.Generator.Block.Error (BlockGenError (..))
import           Pos.Generator.Block.Mode (BlockGenMode, BlockGenRandMode, MonadBlockGenBase)
import           Pos.Generator.Block.Param (HasBlockGenParams (..), HasTxGenParams (..))
import qualified Pos.GState as DB
import           Pos.Txp.MemState.Class (MonadTxpLocal (..))
import           Pos.Txp.Toil (Utxo, execUtxoM, utxoToLookup)
import qualified Pos.Txp.Toil.Utxo as Utxo
import qualified Pos.Util.Modifier as Modifier

----------------------------------------------------------------------------
-- Tx payload generation
----------------------------------------------------------------------------

-- | Generates list of distinct ints in the given range [a,b].
selectDistinct :: forall m. (MonadRandom m) => Int -> (Int,Int) -> m [Int]
selectDistinct n0 p@(a, b)
    | b - a < 0 = error $ "selectDistinct: b < a " <> show p
    | otherwise = do
          res <- reverse <$> selectDistinct' [] n (a, b)
          if fromIntegral (length res) /= n
              then error $ "selectDistinct is broken: " <> show res <> " " <> show n
              else pure res
  where
    n :: Int
    n = min (b + 1 - a) n0
    selectDistinct' :: [Int] -> Int -> (Int, Int) -> m [Int]
    selectDistinct' cur 0 _ = pure cur
    selectDistinct' cur leftN (a', b') = do
        let upEdge = b' - leftN + 1
        nextInt <- getRandomR (a', upEdge)
        selectDistinct' (nextInt : cur) (leftN - 1) (nextInt + 1, b')

-- | Selects some number of distinct elements from the list; the number is
-- taken from given range [a, b]
selectSomeFromList :: MonadRandom m => (Int, Int) -> [a] -> m [a]
selectSomeFromList p@(a, b0) ls
    | l == 0 = error "selectSomeFromList: empty list passed"
    | l < a = error $
              "selectSomeFromList: list length < a (" <>
              show l <> " < " <> show a <> ")"
    | b - a < 0 = error $ "selectSomeFromList: b < a " <> show p
    | otherwise = do
          n <- getRandomR (a, b)
          idxs <- selectDistinct n (0, l - 1)
          pure $ map (ls !!) idxs
  where
    l = length ls
    b = min l b0

-- | Separates coin into provided number of coins. All resulting coins
-- are nonzero.
splitCoins :: (MonadRandom m) => Int -> Coin -> m [Coin]
splitCoins n c0
    | c == (0::Int) = error "splitCoins, c = 0"
    | c < n = error $ "splitCoins: can't split " <> pretty c0 <>
                      " on " <> show n <> " parts"
    | c == (1::Int) = pure [c0]
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

-- | State datatype for transaction payload generation. Internal UTXO
-- view only contains the part of utxo that we have access to (own
-- related secret keys).
data GenTxData = GenTxData
    { _gtdUtxo     :: Utxo
      -- ^ Utxo as it is.
    , _gtdUtxoKeys :: V.Vector TxIn
      -- ^ Keys of 'gtdUtxo' to support random selection by index.
    }

makeLenses ''GenTxData

-- TODO: move to txp, think how to unite it with 'Test.Pos.Txp.Arbitrary'.
-- | Generate valid 'TxPayload' using current global state.
genTxPayload
    :: forall ext g m
     . (RandomGen g, MonadBlockGenBase m, MonadTxpLocal (BlockGenMode ext m))
    => ProtocolMagic
    -> BlockGenRandMode ext g m ()
genTxPayload pm = do
    invAddrSpendingData <-
        unInvAddrSpendingData <$> view (blockGenParams . asSpendingData)
    -- We only leave outputs we have secret keys related to. Tx
    -- generation only sends money to, again, keys from
    -- 'invAddrSpendingData' so 'GenTxData' is consistent.
    let knowSecret (toaOut -> txOut) =
            txOutAddress txOut `HM.member` invAddrSpendingData
    utxo <- M.filter knowSecret <$> lift DB.getAllPotentiallyHugeUtxo
    let gtd = GenTxData utxo (V.fromList $ M.keys utxo)
    flip evalStateT gtd $ do
        (a,d) <- lift $ view tgpTxCountRange
        txsN <- fromIntegral <$> getRandomR (a, a + d)
        void $ replicateM txsN genTransaction
  where
    nm = makeNetworkMagic pm
    genTransaction :: StateT GenTxData (BlockGenRandMode ext g m) ()
    genTransaction = do
        utxo <- use gtdUtxo
        utxoSize <- uses gtdUtxoKeys V.length
        when (utxoSize == 0) $
            lift $ throwM $ BGInternal "Utxo is empty when trying to create tx payload"

        secrets <- unInvSecretsMap <$> view (blockGenParams . asSecretKeys)
        invAddrSpendingData <-
            unInvAddrSpendingData <$> view (blockGenParams . asSpendingData)
        let secretsPks = map toPublic $ HM.elems secrets
        let addrToSk :: Address -> SecretKey
            addrToSk addr = do
                let cantResolve =
                        "addrToSk: can't resolve " <> pretty addr <>
                        " probably genTransaction is broken"
                let inconsistent =
                        "addrToSk: resolved sk using invAddrSpendingData " <>
                        "but cant using invSecretsMap"
                let spendingData =
                        fromMaybe (error cantResolve) $ invAddrSpendingData ^. at addr
                case spendingData of
                    PubKeyASD pk ->
                        fromMaybe (error inconsistent) (HM.lookup (addressHash pk) secrets)
                    another -> error $
                        sformat ("addrToSk: ound an address with non-pubkey spending data: "
                                    %build) another

        -- Currently payload generator only uses addresses with
        -- bootstrap era distribution. This is fine, because we don't
        -- have usecases where we switch to reward era.
        let utxoAddresses = map (makePubKeyAddressBoot nm . toPublic) $ HM.elems secrets
            utxoAddrsN = HM.size secrets
        let adder hm TxOutAux { toaOut = TxOut {..} } =
                HM.insertWith (+) txOutAddress (coinToInteger txOutValue) hm
            utxoBalances = foldl' adder mempty utxo
            hasMoney addr = fromMaybe 0 (HM.lookup addr utxoBalances) > 0
            addrsWithMoney = filter hasMoney utxoAddresses

        -- Select input and output addresses
        maxOutputsN <- fromIntegral <$> view tgpMaxOutputs
        let maxInputAddrsN = max 1 $ min 3 $ length addrsWithMoney - 1
        inputAddrs <- selectSomeFromList (1, maxInputAddrsN) addrsWithMoney
        -- Output addresses should differ from input addresses
        let notInputs = filter (`notElem` inputAddrs) utxoAddresses
        when (null notInputs) $ throwM $ BGInternal $
            "Payload generator: no available outputs, probably utxo size is 1"
        outputAddrs <- selectSomeFromList (1, maxOutputsN) notInputs

        let outputsN = length outputAddrs

        -- Select UTXOs belonging to one of input addresses and determine
        -- total amount of money available
        let ownUtxo = Utxo.filterUtxoByAddrs inputAddrs utxo
            totalOwnMoney = coinToInteger $ Utxo.getTotalCoinsInUtxo ownUtxo

        -- We divide total own money by 2 to have space for tx fee
        totalTxAmount <- getRandomR (fromIntegral outputsN, totalOwnMoney `div` 2)

        changeAddrIdx <- getRandomR (0, utxoAddrsN - 1)
        let changeAddrData = makePubKeyAddressBoot nm $ secretsPks !! changeAddrIdx

        -- Prepare tx outputs
        coins <- splitCoins outputsN (unsafeIntegerToCoin totalTxAmount)
        let txOuts :: NonEmpty TxOut
            txOuts = NE.fromList $ zipWith TxOut outputAddrs coins
            txOutAuxs = map TxOutAux txOuts

        -- Form a transaction
        let inputSKs = map addrToSk inputAddrs
            signers = HM.fromList $ zip inputAddrs (map fakeSigner inputSKs)
            getSigner addr =
                note (SafeSignerNotFound addr) $
                HM.lookup addr signers
            makeTestTx i o = makeMPubKeyTxAddrs pm getSigner i o
            groupedInputs = OptimizeForSecurity

        eTx <- lift . lift $
            createGenericTx pm mempty makeTestTx groupedInputs ownUtxo txOutAuxs changeAddrData
        (txAux, _) <- either (throwM . BGFailedToCreate . pretty) pure eTx

        let tx = taTx txAux
        let txId = hash tx
        let txIns = _txInputs tx
        -- @txpProcessTx@ for BlockGenMode should be non-blocking
        res <- lift . lift $ txpProcessTx pm (txId, txAux)
        case res of
            Left e  -> error $ "genTransaction@txProcessTransaction: got left: " <> pretty e
            Right _ -> do
                utxoLookup <- utxoToLookup <$> use gtdUtxo
                let utxoModifier = execUtxoM mempty utxoLookup $
                        Utxo.applyTxToUtxo (WithHash tx txId)
                gtdUtxo %= Modifier.modifyMap utxoModifier
                gtdUtxoKeys %= V.filter (`notElem` txIns)
                let outsAsIns =
                        map (TxInUtxo txId) [0..(fromIntegral $ length txOuts)-1]
                gtdUtxoKeys %= (V.++) (V.fromList outsAsIns)


----------------------------------------------------------------------------
-- Payload generation
----------------------------------------------------------------------------

-- | Generate random payload which is valid with respect to the current
-- global state and mempool and add it to mempool.  Currently we are
-- concerned only about tx payload, later we can add more stuff.
genPayload
    :: forall ext g m
     . (RandomGen g, MonadBlockGenBase m, MonadTxpLocal (BlockGenMode ext m))
    => ProtocolMagic
    -> SlotId
    -> BlockGenRandMode ext g m ()
genPayload pm _ = genTxPayload pm
