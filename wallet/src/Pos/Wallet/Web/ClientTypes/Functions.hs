{-# LANGUAGE TypeFamilies #-}

-- | Functions on client types
module Pos.Wallet.Web.ClientTypes.Functions
      ( encToCId
      , addressToCId
      , cIdToAddress
      , mkCTx
      , toCUpdateInfo
      , addrMetaToAccount
      , isTxLocalAddress
      ) where

import           Universum

import           Control.Monad.Error.Class (throwError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Data.Text (Text)
import           Formatting (build, sformat)

import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core (Address, ChainDifficulty, decodeTextAddress, makePubKeyAddressBoot,
                           sumCoins, unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.Txp (Tx (..), TxOut (..), txOutAddress, txOutValue)
import           Pos.Core.Update (BlockVersionData (..), BlockVersionModifier (..),
                                  UpdateProposal (..))
import           Pos.Crypto (EncryptedSecretKey, encToPublic)
import           Pos.Update.Poll.Types (ConfirmedProposalState (..), StakeholderVotes,
                                        isPositiveVote)
import           Pos.Util.Servant
import           Pos.Wallet.Web.ClientTypes.Instances ()
import           Pos.Wallet.Web.ClientTypes.Types (AccountId (..), Addr, CCoin, CHash (..),
                                                   CId (..), CPtxCondition (..), CTx (..), CTxMeta,
                                                   CUpdateInfo (..), CWAddressMeta (..))

-- TODO: this is not completely safe. If someone changes
-- implementation of Buildable Address. It should be probably more
-- safe to introduce `class PSSimplified` that would have the same
-- implementation has it is with Buildable Address but then person
-- will know it will probably change something for purescript.
-- | Transform Address into CId
addressToCId :: Address -> CId w
addressToCId = CId . CHash . sformat build

cIdToAddress :: CId w -> Either Text Address
cIdToAddress (CId (CHash h)) = decodeTextAddress h

-- TODO: pass extra information to this function and choose
-- distribution based on this information. Currently it's always
-- bootstrap era distribution.
encToCId :: EncryptedSecretKey -> CId w
encToCId = encodeCType . makePubKeyAddressBoot . encToPublic

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
    -> Set (CId Addr)     -- ^ Addresses of wallet
    -> Either Text CTx
mkCTx diff THEntry {..} meta pc wAddrsSet = do
    let isOurTxAddress = flip S.member wAddrsSet . addressToCId . txOutAddress

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

        ctAmount = encodeCType . unsafeIntegerToCoin $
            if | ctIsLocal -> outgoingMoney
               | isOutgoing -> outgoingMoney - incomingMoney
               | otherwise -> incomingMoney - outgoingMoney

    return CTx {..}
  where
    ctId = encodeCType _thTxId
    encodeTxOut :: TxOut -> (CId Addr, CCoin)
    encodeTxOut TxOut{..} = (encodeCType txOutAddress, encodeCType txOutValue)
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
        cuiPositiveStake    = encodeCType cpsPositiveStake
        cuiNegativeStake    = encodeCType cpsNegativeStake
    in CUpdateInfo {..}
