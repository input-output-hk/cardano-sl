-- | Pure functions for operations with transactions

module Pos.Client.Txp.Util
       (
       -- * Tx creation
         TxCreateMode
       , makeAbstractTx
       , overrideTxOutDistrBoot
       , overrideTxDistrBoot
       , makePubKeyTx
       , makeMPubKeyTx
       , makeMOfNTx
       , makeRedemptionTx
       , createTx
       , createMTx
       , createMOfNTx
       , createRedemptionTx

       -- * Additional datatypes
       , TxError (..)
       ) where

import           Control.Lens         (traversed, (%=), (.=))
import           Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import           Control.Monad.State  (StateT (..), evalStateT)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (tail)
import qualified Data.Map             as M
import qualified Data.Text.Buildable
import qualified Data.Vector          as V
import           Ether.Internal       (HasLens (..))
import           Formatting           (bprint, build, sformat, stext, (%))
import           Universum

import           Pos.Binary           ()
import           Pos.Context          (GenesisUtxo, genesisStakeholdersM)
import           Pos.Core             (AddressIgnoringAttributes (AddressIA), siEpoch,
                                       unsafeGetCoin, unsafeIntegerToCoin, unsafeSubCoin)
import           Pos.Crypto           (PublicKey, RedeemSecretKey, SafeSigner,
                                       SignTag (SignTxIn), hash, redeemSign,
                                       redeemToPublic, safeSign, safeToPublic)
import           Pos.Data.Attributes  (mkAttributes)
import           Pos.DB               (MonadGState, gsIsBootstrapEra)
import           Pos.Genesis          (genesisSplitBoot)
import           Pos.Script           (Script)
import           Pos.Script.Examples  (multisigRedeemer, multisigValidator)
import           Pos.Slotting.Class   (MonadSlots (getCurrentSlotBlocking))
import           Pos.Txp              (Tx (..), TxAux (..), TxDistribution (..),
                                       TxIn (..), TxInWitness (..), TxOut (..),
                                       TxOutAux (..), TxOutDistribution, TxSigData (..),
                                       Utxo, filterUtxoByAddrs)
import           Pos.Types            (Address, Coin, makePubKeyAddress,
                                       makePubKeyAddress, makeRedeemAddress,
                                       makeScriptAddress, mkCoin, sumCoins)

type TxInputs = NonEmpty TxIn
type TxOwnedInputs owner = NonEmpty (owner, TxIn)
type TxOutputs = NonEmpty TxOutAux

data TxError
    = TxError { unTxError :: !Text }
    deriving (Show, Generic)

instance Exception TxError

instance Buildable TxError where
    build (TxError msg) = bprint ("Transaction creation error ("%stext%")") msg

throwTxError
    :: MonadError TxError m
    => Text -> m a
throwTxError = throwError . TxError

-----------------------------------------------------------------------------
-- Tx creation
-----------------------------------------------------------------------------

-- | Mode for creating transactions. We need to know the bootstrap era
-- status and have access to generic stakeholders to distribute
-- txdistr accordingly.
type TxCreateMode ctx m
     = ( MonadGState m
       , MonadReader ctx m
       , MonadSlots m
       , HasLens GenesisUtxo ctx GenesisUtxo
       )

-- | Generic function to create a transaction, given desired inputs,
-- outputs and a way to construct witness from signature data
makeAbstractTx :: (owner -> TxSigData -> TxInWitness)
               -> TxOwnedInputs owner
               -> TxOutputs
               -> TxAux
makeAbstractTx mkWit txInputs outputs =
    TxAux
    { taTx = UnsafeTx (map snd txInputs) txOutputs txAttributes
    , taWitness = txWitness
    , taDistribution = txDist
    }
  where
    txOutputs = map toaOut outputs
    txAttributes = mkAttributes ()
    txOutHash = hash txOutputs
    txDist = TxDistribution (map toaDistr outputs)
    txDistHash = hash txDist
    txWitness = V.fromList $ toList $ txInputs <&>
        \(addr, txIn) -> mkWit addr $ makeTxSigData txIn
    makeTxSigData txIn =
        TxSigData
        { txSigInput = txIn
        , txSigOutsHash = txOutHash
        , txSigDistrHash = txDistHash
        }

-- | Overrides 'txDistr' with correct ones (according to the boot era
-- stake distribution) or leaves it as it is if in post-boot era.
overrideTxOutDistrBoot ::
       (TxCreateMode ctx m)
    => Coin
    -> TxOutDistribution
    -> ExceptT TxError m TxOutDistribution
overrideTxOutDistrBoot c oldDistr = do
    -- Blocking here should be fine for now (@volhovm)
    -- 1. Code in tx generator must have current slot.
    -- 2. Code in wallet will block on "synchronizing" on the
    --    frontend so it's fine too.
    epoch <- siEpoch <$> lift getCurrentSlotBlocking
    bootEra <- lift $ gsIsBootstrapEra epoch
    genStakeholders <- toList <$> genesisStakeholdersM
    if not bootEra
      then pure oldDistr
      else do
          when (unsafeGetCoin c < fromIntegral (length genStakeholders)) $
               throwTxError $
               sformat ("Can't spend "%build%" coins: amount is too small for boot "%
                        " era and can't be distributed among genStakeholders") c
          -- TODO CSL-1351 boot stakeholders' weights are not used
          pure $ genesisSplitBoot (HM.fromList $ map (,1) genStakeholders) c

-- | Same as 'overrideTxOutDistrBoot' but changes 'TxOutputs' all at once
overrideTxDistrBoot ::
       (TxCreateMode ctx m) => TxOutputs -> ExceptT TxError m TxOutputs
overrideTxDistrBoot outputs = do
    forM outputs $ \TxOutAux{..} -> do
        newStakeDistr <- overrideTxOutDistrBoot (txOutValue toaOut) toaDistr
        pure $ TxOutAux toaOut newStakeDistr

-- | Like 'makePubKeyTx', but allows usage of different signers
makeMPubKeyTx :: (owner -> SafeSigner)
              -> TxOwnedInputs owner
              -> TxOutputs
              -> TxAux
makeMPubKeyTx getSs = makeAbstractTx mkWit
  where mkWit addr sigData =
          let ss = getSs addr
          in PkWitness
              { twKey = safeToPublic ss
              , twSig = safeSign SignTxIn ss sigData
              }

-- | Makes a transaction which use P2PKH addresses as a source
makePubKeyTx :: SafeSigner -> TxInputs -> TxOutputs -> TxAux
makePubKeyTx ss txInputs =
    makeMPubKeyTx (const ss) (map ((), ) txInputs)

makeMOfNTx :: Script -> [Maybe SafeSigner] -> TxInputs -> TxOutputs -> TxAux
makeMOfNTx validator sks txInputs = makeAbstractTx mkWit (map ((), ) txInputs)
  where mkWit _ sigData = ScriptWitness
            { twValidator = validator
            , twRedeemer = multisigRedeemer sigData sks
            }

makeRedemptionTx :: RedeemSecretKey -> TxInputs -> TxOutputs -> TxAux
makeRedemptionTx rsk txInputs = makeAbstractTx mkWit (map ((), ) txInputs)
  where rpk = redeemToPublic rsk
        mkWit _ sigData = RedeemWitness
            { twRedeemKey = rpk
            , twRedeemSig = redeemSign rsk sigData
            }

type FlatUtxo = [(TxIn, TxOutAux)]
type InputPicker = StateT (Coin, FlatUtxo) (Either TxError)

-- | Given Utxo, desired source addresses and desired outputs, prepare lists
-- of correct inputs and outputs to form a transaction
prepareInpsOuts
    :: MonadError TxError m
    => Utxo
    -> NonEmpty Address
    -> TxOutputs
    -> m (TxOwnedInputs Address, TxOutputs)
prepareInpsOuts utxo addrs outputs = do
    when (totalMoney == mkCoin 0) $
        throwTxError "Attempted to send 0 money"
    futxo <- either throwError pure $
        evalStateT (pickInputs []) (totalMoney, sortedUnspent)
    case nonEmpty futxo of
        Nothing       -> throwTxError "Failed to prepare inputs!"
        Just inputsNE -> pure (map formTxInputs inputsNE, outputs)
  where
    totalMoney = unsafeIntegerToCoin $ sumCoins $ map (txOutValue . toaOut) outputs
    allUnspent = M.toList $ filterUtxoByAddrs (toList addrs) utxo
    sortedUnspent =
        sortOn (Down . txOutValue . toaOut . snd) allUnspent

    pickInputs :: FlatUtxo -> InputPicker FlatUtxo
    pickInputs inps = do
        moneyLeft <- use _1
        if moneyLeft == mkCoin 0
            then return inps
            else do
                mNextOut <- head <$> use _2
                case mNextOut of
                    Nothing -> throwTxError "Not enough money to send!"
                    Just inp@(_, (TxOutAux (TxOut {..}) _)) -> do
                        _1 .= unsafeSubCoin moneyLeft (min txOutValue moneyLeft)
                        _2 %= tail
                        pickInputs (inp : inps)
    formTxInputs (inp, TxOutAux TxOut{..} _) = (txOutAddress, inp)

-- | Common use case of 'prepaseInpsOuts' - with single source address
prepareInpOuts
    :: MonadError TxError m
    => Utxo -> Address -> TxOutputs -> m (TxInputs, TxOutputs)
prepareInpOuts utxo addr outputs =
    prepareInpsOuts utxo (one addr) outputs <&>
    _1 . traversed %~ snd

-- | Make a multi-transaction using given secret key and info for outputs.
-- Currently used for HD wallets only, thus `HDAddressPayload` is required
createMTx :: Utxo -> NonEmpty (SafeSigner, Address) -> TxOutputs -> Either TxError TxAux
createMTx utxo hwdSigners outputs =
    let addrs = map snd hwdSigners
    in  uncurry (makeMPubKeyTx getSigner) <$>
        prepareInpsOuts utxo addrs outputs
  where
    signers = HM.fromList . toList $ map (swap . second AddressIA) hwdSigners
    getSigner addr =
        fromMaybe (error "Requested signer for unknown address") $
        HM.lookup (AddressIA addr) signers

-- | Make a multi-transaction using given secret key and info for
-- outputs.
createTx
    :: TxCreateMode ctx m
    => Utxo
    -> SafeSigner
    -> TxOutputs
    -> m (Either TxError TxAux)
createTx utxo ss outputs =
    runExceptT $ do
        properOutputs <- overrideTxDistrBoot outputs
        uncurry (makePubKeyTx ss) <$>
            prepareInpOuts
                utxo
                (makePubKeyAddress $ safeToPublic ss)
                properOutputs

-- | Make a transaction, using M-of-N script as a source
createMOfNTx :: Utxo -> [(PublicKey, Maybe SafeSigner)] -> TxOutputs -> Either TxError TxAux
createMOfNTx utxo keys outputs = uncurry (makeMOfNTx validator sks) <$> inpOuts
  where pks = map fst keys
        sks = map snd keys
        m = length $ filter isJust sks
        validator = multisigValidator m pks
        addr = makeScriptAddress validator
        inpOuts = prepareInpOuts utxo addr outputs

createRedemptionTx :: Utxo -> RedeemSecretKey -> TxOutputs -> Either TxError TxAux
createRedemptionTx utxo rsk outputs =
    uncurry (makeRedemptionTx rsk) <$>
    prepareInpOuts utxo (makeRedeemAddress $ redeemToPublic rsk) outputs
