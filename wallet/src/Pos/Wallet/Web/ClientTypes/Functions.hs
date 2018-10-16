{-# LANGUAGE TypeFamilies #-}

-- | Functions on client types
module Pos.Wallet.Web.ClientTypes.Functions
      ( encToCId
      , addressToCId
      , cIdToAddress
      , mkCTx
      , toCUpdateInfo
      , addrMetaToAccount
      ) where

import           Universum

import           Control.Monad.Error.Class (throwError)
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import           Formatting (build, sformat)

import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core (Address, ChainDifficulty, decodeTextAddress, makePubKeyAddressBoot,
                           sumCoins, unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.NetworkMagic (NetworkMagic (..))
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
encToCId :: NetworkMagic -> EncryptedSecretKey -> CId w
encToCId nm = encodeCType . makePubKeyAddressBoot nm . encToPublic

mergeTxOuts :: [TxOut] -> [TxOut]
mergeTxOuts = map stick . NE.groupWith txOutAddress
  where stick outs@(TxOut{txOutAddress = addr} :| _) =
            TxOut addr (foldl1 unsafeAddCoin $ fmap txOutValue outs)

mkCTx
    :: ChainDifficulty    -- ^ Current chain difficulty (to get confirmations)
    -> TxHistoryEntry     -- ^ Tx history entry
    -> CTxMeta            -- ^ Transaction metadata
    -> CPtxCondition      -- ^ State of resubmission
    -> (Address -> Bool) -- ^ Whether addresses belong to the wallet
    -> Either Text CTx
mkCTx diff THEntry {..} meta pc addrBelongsToWallet = do
    let isOurTxAddress = addrBelongsToWallet . txOutAddress

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
