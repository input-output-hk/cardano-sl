
-- | Functions on client types
module Pos.Wallet.Web.ClientTypes.Functions
      ( mkCCoin
      , addressToCId
      , cIdToAddress
      , encToCId
      , mkCTx
      , mkCTxId
      , ptxCondToCPtxCond
      , txIdToCTxId
      , toCUpdateInfo
      , addrMetaToAccount
      , isTxLocalAddress
      ) where

import           Universum

import           Control.Monad.Error.Class        (throwError)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Set                         as S
import           Data.Text                        (Text)
import           Formatting                       (sformat)
import qualified Formatting                       as F

import           Pos.Aeson.Types                  ()
import           Pos.Client.Txp.History           (TxHistoryEntry (..))
import           Pos.Crypto                       (EncryptedSecretKey, encToPublic,
                                                   hashHexF)
import           Pos.Txp.Core.Types               (Tx (..), TxId, TxOut (..),
                                                   txOutAddress, txOutValue)
import           Pos.Types                        (Address (..), ChainDifficulty, Coin,
                                                   decodeTextAddress,
                                                   makePubKeyAddressBoot, sumCoins,
                                                   unsafeAddCoin, unsafeGetCoin,
                                                   unsafeIntegerToCoin)
import           Pos.Update.Core                  (BlockVersionData (..),
                                                   BlockVersionModifier (..),
                                                   StakeholderVotes, UpdateProposal (..),
                                                   isPositiveVote)
import           Pos.Update.Poll                  (ConfirmedProposalState (..))
import           Pos.Wallet.Web.ClientTypes.Types (AccountId (..), Addr, CCoin (..),
                                                   CHash (..), CId (..),
                                                   CPtxCondition (..), CTx (..),
                                                   CTxId (..), CTxMeta, CUpdateInfo (..),
                                                   CWAddressMeta (..))
import           Pos.Wallet.Web.Pending.Types     (PtxCondition (..))


-- TODO: this is not completely safe. If someone changes
-- implementation of Buildable Address. It should be probably more
-- safe to introduce `class PSSimplified` that would have the same
-- implementation has it is with Buildable Address but then person
-- will know it will probably change something for purescript.
-- | Transform Address into CId
addressToCId :: Address -> CId w
addressToCId = CId . CHash . sformat F.build

cIdToAddress :: CId w -> Either Text Address
cIdToAddress (CId (CHash h)) = decodeTextAddress h

-- TODO: pass extra information to this function and choose
-- distribution based on this information. Currently it's always
-- bootstrap era distribution.
encToCId :: EncryptedSecretKey -> CId w
encToCId = addressToCId . makePubKeyAddressBoot . encToPublic

mkCTxId :: Text -> CTxId
mkCTxId = CTxId . CHash

-- | transform TxId into CTxId
txIdToCTxId :: TxId -> CTxId
txIdToCTxId = mkCTxId . sformat hashHexF

ptxCondToCPtxCond :: Maybe PtxCondition -> CPtxCondition
ptxCondToCPtxCond = maybe CPtxNotTracked $ \case
    PtxApplying{}       -> CPtxApplying
    PtxInNewestBlocks{} -> CPtxInBlocks
    PtxPersisted{}      -> CPtxInBlocks
    PtxWontApply{}      -> CPtxWontApply

-- [CSM-309] This may work until transaction have multiple source accounts
-- | Get all addresses of source account of given transaction.
getTxSourceAccountAddresses
    :: [CWAddressMeta]      -- ^ All addresses in wallet
    -> NonEmpty (CId Addr)  -- ^ Input addresses of transaction
    -> Maybe [CId Addr]     -- ^ `Just` addrs if the wallet is source of
                            --   transaction, `Nothing` otherwise
getTxSourceAccountAddresses walAddrMetas (someInputAddr :| _) = do
    someSrcAddrMeta <- find ((== someInputAddr) . cwamId) walAddrMetas
    let srcAccount = addrMetaToAccount someSrcAddrMeta
    return $
        map cwamId $
        filter ((srcAccount ==) . addrMetaToAccount) walAddrMetas

-- | Produces a function which for a given address says whether this address
-- belongs to the same account as the addresses which are the sources
-- of a given transaction. This assumes that the source addresses belong
-- to the same account.
-- Note that if you apply this function to the outputs of a transaction,
-- you will effectively get /change/ addresses.
isTxLocalAddress
    :: [CWAddressMeta]      -- ^ All addresses in wallet
    -> NonEmpty (CId Addr)  -- ^ Input addresses of transaction
    -> (CId Addr -> Bool)
isTxLocalAddress wAddrMetas inputs = do
    let mLocalAddrs = getTxSourceAccountAddresses wAddrMetas inputs
    case mLocalAddrs of
       Just changeAddrs -> do
           -- if given wallet is source of tx, /local addresses/
           -- can be fetched according to definition
           let changeAddrsSet = S.fromList changeAddrs
           flip S.member changeAddrsSet
       Nothing -> do
           -- if given wallet is *not* source of tx, then it's incoming
           -- transaction, and only addresses of given wallet are *not*
           -- local addresses
           -- [CSM-309] This may work until transaction have multiple
           -- destination addresses
           let nonLocalAddrsSet = S.fromList $ cwamId <$> wAddrMetas
           not . flip S.member nonLocalAddrsSet

mergeTxOuts :: [TxOut] -> [TxOut]
mergeTxOuts = map stick . NE.groupWith txOutAddress
  where stick outs@(TxOut{txOutAddress = addr} :| _) =
            TxOut addr (foldl1 unsafeAddCoin $ fmap txOutValue outs)

mkCTx
    :: ChainDifficulty    -- ^ Current chain difficulty (to get confirmations)
    -> TxHistoryEntry     -- ^ Tx history entry
    -> CTxMeta            -- ^ Transaction metadata
    -> CPtxCondition      -- ^ State of resubmission
    -> (CId Addr -> Bool) -- ^ Whether addresses belongs to the wallet
    -> Either Text CTx
mkCTx diff THEntry {..} meta pc addrBelongsToWallet = do
    let isOurTxAddress = addrBelongsToWallet . addressToCId . txOutAddress

        ownInputs = filter isOurTxAddress inputs
        ownOutputs = filter isOurTxAddress outputs

    when (null ownInputs && null ownOutputs) $
        throwError "Transaction is irrelevant to given wallet!"

    let sumMoney = sumCoins . map txOutValue
        outgoingMoney = sumMoney ownInputs
        incomingMoney = sumMoney ownOutputs
        isOutgoing = outgoingMoney >= incomingMoney
        ctIsOutgoing = isOutgoing
        ctIsLocal = length ownInputs == length inputs
                 && length ownOutputs == length outputs

        ctAmount = mkCCoin . unsafeIntegerToCoin $
            if | ctIsLocal -> outgoingMoney
               | isOutgoing -> outgoingMoney - incomingMoney
               | otherwise -> incomingMoney - outgoingMoney

    return CTx {..}
  where
    ctId = txIdToCTxId _thTxId
    encodeTxOut :: TxOut -> (CId Addr, CCoin)
    encodeTxOut TxOut{..} = (addressToCId txOutAddress, mkCCoin txOutValue)
    inputs = _thInputs
    outputs = toList $ _txOutputs _thTx
    ctInputs = map encodeTxOut $ mergeTxOuts _thInputs
    ctOutputs = map encodeTxOut outputs
    ctConfirmations = maybe 0 fromIntegral $ (diff -) <$> _thDifficulty
    ctMeta = meta
    ctCondition = pc

addrMetaToAccount :: CWAddressMeta -> AccountId
addrMetaToAccount CWAddressMeta{..} = AccountId
    { aiWId  = cwamWId
    , aiIndex = cwamAccountIndex
    }

mkCCoin :: Coin -> CCoin
mkCCoin = CCoin . show . unsafeGetCoin

-- | Return counts of negative and positive votes
countVotes :: StakeholderVotes -> (Int, Int)
countVotes = foldl' counter (0, 0)
  where counter (n, m) vote = if isPositiveVote vote
                              then (n + 1, m)
                              else (n, m + 1)

-- | Creates 'CTUpdateInfo' from 'ConfirmedProposalState'
toCUpdateInfo :: BlockVersionData -> ConfirmedProposalState -> CUpdateInfo
toCUpdateInfo bvd ConfirmedProposalState {..} =
    let UnsafeUpdateProposal {..} = cpsUpdateProposal
        cuiSoftwareVersion  = upSoftwareVersion
        cuiBlockVesion      = upBlockVersion
        cuiScriptVersion    = fromMaybe (bvdScriptVersion bvd)
                                        (bvmScriptVersion upBlockVersionMod)
        cuiImplicit         = cpsImplicit
--        cuiProposed         = cpsProposed
--        cuiDecided          = cpsDecided
--        cuiConfirmed        = cpsConfirmed
--        cuiAdopted          = cpsAdopted
        (cuiVotesFor, cuiVotesAgainst) = countVotes cpsVotes
        cuiPositiveStake    = mkCCoin cpsPositiveStake
        cuiNegativeStake    = mkCCoin cpsNegativeStake
    in CUpdateInfo {..}
