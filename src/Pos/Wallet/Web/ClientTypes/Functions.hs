{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeFamilies #-}

-- This module is to be moved later anywhere else, just to have a
-- starting point

-- | Types representing client (wallet) requests on wallet API.
module Pos.Wallet.Web.ClientTypes.Types
      ( SyncProgress (..)
      , spLocalCD
      , spNetworkCD
      , spPeers
      , CId (..)
      , CHash (..)
      , CPassPhrase (..)
      , CProfile (..)
      , CPwHash
      , CTx (..)
      , CTxs (..)
      , CTxId
      , CTxMeta (..)
      , CTExMeta (..)
      , CInitialized (..)
      , AccountId (..)
      , CAccountId (..)
      , CWAddressMeta (..)
      , CAddress (..)
      , CAccount (..)
      , CWalletAssurance (..)
      , CAccountMeta (..)
      , CAccountInit (..)
      , CWallet (..)
      , CWalletMeta (..)
      , CWalletInit (..)
      , CUpdateInfo (..)
      , CWalletRedeem (..)
      , CPaperVendWalletRedeem (..)
      , CCoin
      , mkCCoin
      , coinFromCCoin
      , PassPhraseLU
      , CElectronCrashReport (..)
      , WithDerivationPath (..)
      , Wal (..)
      , Addr (..)
      , addressToCId
      , cIdToAddress
      , encToCId
      , mkCTxs
      , mkCTxId
      , txIdToCTxId
      , toCUpdateInfo
      , addrMetaToAccount
      , isTxLocalAddress
      ) where

import           Universum

import           Control.Arrow             ((&&&))
import           Control.Lens              (makeLenses)
import           Control.Monad.Error.Class (throwError)
import qualified Data.ByteArray            as ByteArray
import qualified Data.ByteString           as BS
import           Data.Default              (Default, def)
import           Data.Hashable             (Hashable (..))
import qualified Data.Set                  as S
import           Data.Text                 (Text, splitOn)
import           Data.Text.Buildable       (build)
import           Data.Time.Clock.POSIX     (POSIXTime)
import           Data.Typeable             (Typeable)
import           Formatting                (bprint, int, sformat, (%))
import qualified Formatting                as F
import qualified Prelude
import qualified Serokell.Util.Base16      as Base16
import           Servant.Multipart         (FileData, FromMultipart (..), lookupFile,
                                            lookupInput)

import           Pos.Aeson.Types           ()
import           Pos.Client.Txp.History    (TxHistoryEntry (..))
import           Pos.Core.Coin             (mkCoin)
import           Pos.Core.Types            (ScriptVersion)
import           Pos.Crypto                (EncryptedSecretKey, PassPhrase, encToPublic,
                                            hashHexF, passphraseLength)
import           Pos.Txp.Core.Types        (Tx (..), TxId, TxOut, txOutAddress,
                                            txOutValue)
import           Pos.Types                 (Address (..), BlockVersion, ChainDifficulty,
                                            Coin, SoftwareVersion, decodeTextAddress,
                                            makePubKeyAddress, sumCoins, unsafeGetCoin,
                                            unsafeIntegerToCoin)
import           Pos.Update.Core           (BlockVersionModifier (..), StakeholderVotes,
                                            UpdateProposal (..), isPositiveVote)
import           Pos.Update.Poll           (ConfirmedProposalState (..))
import           Pos.Util.BackupPhrase     (BackupPhrase)
import           Pos.Util.Servant          (FromCType (..), OriginType, ToCType (..))


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

encToCId :: EncryptedSecretKey -> CId w
encToCId = addressToCId . makePubKeyAddress . encToPublic

mkCTxId :: Text -> CTxId
mkCTxId = CTxId . CHash

-- | transform TxId into CTxId
txIdToCTxId :: TxId -> CTxId
txIdToCTxId = mkCTxId . sformat hashHexF

convertTxOutputs :: [TxOut] -> [(CId w, CCoin)]
convertTxOutputs = map (addressToCId . txOutAddress &&& mkCCoin . txOutValue)

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

mkCTxs
    :: ChainDifficulty    -- ^ Current chain difficulty (to get confirmations)
    -> TxHistoryEntry     -- ^ Tx history entry
    -> CTxMeta            -- ^ Transaction metadata
    -> [CWAddressMeta]    -- ^ Addresses of wallet
    -> Either Text CTxs
mkCTxs diff THEntry {..} meta wAddrMetas = do
    let isOurTxOutput = flip S.member wAddrsSet . addressToCId . txOutAddress

        ownInputs = filter isOurTxOutput inputs
        ownOutputs = filter isOurTxOutput outputs

    when (null ownInputs && null ownOutputs) $
        throwError "Transaction is irrelevant to given wallet!"

    let sumMoney = sumCoins . map txOutValue
        outgoingMoney = sumMoney ownInputs
        incomingMoney = sumMoney ownOutputs
        isOutgoing = outgoingMoney >= incomingMoney
        isIncoming = incomingMoney >= outgoingMoney

        ctAmount = mkCCoin . unsafeIntegerToCoin $
            if | isOutgoing && isIncoming -> outgoingMoney
               | isOutgoing -> outgoingMoney - incomingMoney
               | isIncoming -> incomingMoney - outgoingMoney

        mkCTx ctIsOutgoing cond = guard cond $> CTx {..}
        ctsOutgoing = mkCTx True isOutgoing
        ctsIncoming = mkCTx False isIncoming
    return CTxs {..}
  where
    ctId = txIdToCTxId _thTxId
    inputs = _thInputs
    outputs = toList $ _txOutputs _thTx
    ctInputAddrs = map addressToCId _thInputAddrs
    ctOutputAddrs = map addressToCId _thOutputAddrs
    ctConfirmations = maybe 0 fromIntegral $ (diff -) <$> _thDifficulty
    ctMeta = meta
    wAddrsSet = S.fromList $ map cwamId wAddrMetas

addrMetaToAccount :: CWAddressMeta -> AccountId
addrMetaToAccount CWAddressMeta{..} = AccountId
    { aiWId  = cwamWId
    , aiIndex = cwamWalletIndex
    }

mkCCoin :: Coin -> CCoin
mkCCoin = CCoin . show . unsafeGetCoin

coinFromCCoin :: CCoin -> Maybe Coin
coinFromCCoin = fmap mkCoin . readMaybe . toString . getCCoin

-- | Return counts of negative and positive votes
countVotes :: StakeholderVotes -> (Int, Int)
countVotes = foldl' counter (0, 0)
  where counter (n, m) vote = if isPositiveVote vote
                              then (n + 1, m)
                              else (n, m + 1)

-- | Creates 'CTUpdateInfo' from 'ConfirmedProposalState'
toCUpdateInfo :: ConfirmedProposalState -> CUpdateInfo
toCUpdateInfo ConfirmedProposalState {..} =
    let UnsafeUpdateProposal {..} = cpsUpdateProposal
        cuiSoftwareVersion  = upSoftwareVersion
        cuiBlockVesion      = upBlockVersion
        cuiScriptVersion    = bvmScriptVersion upBlockVersionMod
        cuiImplicit         = cpsImplicit
--        cuiProposed         = cpsProposed
--        cuiDecided          = cpsDecided
--        cuiConfirmed        = cpsConfirmed
--        cuiAdopted          = cpsAdopted
        (cuiVotesFor, cuiVotesAgainst) = countVotes cpsVotes
        cuiPositiveStake    = mkCCoin cpsPositiveStake
        cuiNegativeStake    = mkCCoin cpsNegativeStake
    in CUpdateInfo {..}
