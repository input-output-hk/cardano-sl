{-# LANGUAGE TemplateHaskell #-}

-- This module is to be moved later anywhere else, just to have a
-- starting point

-- | Types representing client (wallet) requests on wallet API.
module Pos.Wallet.Web.ClientTypes
      ( SyncProgress (..)
      , CAddress (..)
      , CCurrency (..)
      , CHash (..)
      , CTType (..)
      , CPassPhrase (..)
      , CProfile (..)
      , CPwHash
      , CTx (..)
      , CTxId
      , CTxMeta (..)
      , CTExMeta (..)
      , CInitialized (..)
      , CWalletAddress (..)
      , CAccountAddress (..)
      , CAccount (..)
      , CWallet (..)
      , CWalletType (..)
      , CWalletAssurance (..)
      , CWalletMeta (..)
      , CWalletInit (..)
      , CWalletSet (..)
      , CWalletSetMeta (..)
      , CWalletSetInit (..)
      , CUpdateInfo (..)
      , CWalletRedeem (..)
      , NotifyEvent (..)
      , WithDerivationPath (..)
      , WS (..)
      , Acc (..)
      , addressToCAddress
      , cAddressToAddress
      , passPhraseToCPassPhrase
      , cPassPhraseToPassPhrase
      , mkCTx
      , mkCTxId
      , txIdToCTxId
      , ctTypeMeta
      , txContainsTitle
      , toCUpdateInfo
      , walletAddrByAccount
      ) where

import           Universum

import qualified Data.ByteString.Lazy   as LBS
import           Data.Default           (Default, def)
import           Data.Hashable          (Hashable (..))
import           Data.Text              (Text, isInfixOf, toLower)
import           Data.Text.Buildable    (build)
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Data.Typeable          (Typeable)
import           Formatting             (bprint, sformat, (%))
import qualified Formatting             as F
import           Prelude                (show)
import qualified Serokell.Util.Base16   as Base16

import           Pos.Aeson.Types        ()
import           Pos.Binary.Class       (decodeFull, encodeStrict)
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core.Types         (ScriptVersion)
import           Pos.Crypto             (PassPhrase, hashHexF)
import           Pos.Txp.Core.Types     (Tx (..), TxId, txOutAddress, txOutValue)
import           Pos.Types              (Address (..), BlockVersion, ChainDifficulty,
                                         Coin, SoftwareVersion, decodeTextAddress,
                                         sumCoins, unsafeIntegerToCoin)
import           Pos.Update.Core        (BlockVersionData (..), StakeholderVotes,
                                         UpdateProposal (..), isPositiveVote)
import           Pos.Update.Poll        (ConfirmedProposalState (..))
import           Pos.Util.BackupPhrase  (BackupPhrase, mkBackupPhrase)

data SyncProgress = SyncProgress
    { _spLocalCD   :: ChainDifficulty
    , _spNetworkCD :: Maybe ChainDifficulty
    , _spPeers     :: Word
    } deriving (Show, Generic, Typeable)

instance Default SyncProgress where
    def = SyncProgress 0 mzero 0

-- Notifications
data NotifyEvent
    = ConnectionOpened
    -- _ | NewWalletTransaction CAddress
    -- _ | NewTransaction
    | NetworkDifficultyChanged ChainDifficulty -- ie new block or fork (rollback)
    | LocalDifficultyChanged ChainDifficulty -- ie new block or fork (rollback)
    | ConnectedPeersChanged Word
    | UpdateAvailable
    | ConnectionClosed
    deriving (Show, Generic)

-- | Currencies handled by client.
-- Note: Cardano does not deal with other currency than ADA yet.
data CCurrency
    = ADA
    | BTC
    | ETH
    deriving (Show, Read, Generic)

-- | Client hash
newtype CHash = CHash Text deriving (Show, Eq, Generic, Buildable)

instance Hashable CHash where
    hashWithSalt s (CHash h) = hashWithSalt s h

-- | Client address
-- @w@ is phantom type and stands for type of item this address belongs to.
newtype CAddress w = CAddress CHash deriving (Show, Eq, Generic, Hashable, Buildable)

-- | Marks address as belonging to wallet set.
data WS = WS

-- | Marks address as belonging to account.
data Acc = Acc

-- TODO: this is not complitely safe. If someone changes
-- implementation of Buildable Address. It should be probably more
-- safe to introduce `class PSSimplified` that would have the same
-- implementation has it is with Buildable Address but then person
-- will know it will probably change something for purescript.
-- | Transform Address into CAddress
addressToCAddress :: Address -> (CAddress w)
addressToCAddress = CAddress . CHash . sformat F.build

cAddressToAddress :: CAddress w -> Either Text Address
cAddressToAddress (CAddress (CHash h)) = decodeTextAddress h

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Eq, Generic, Hashable)

mkCTxId :: Text -> CTxId
mkCTxId = CTxId . CHash

-- | transform TxId into CTxId
txIdToCTxId :: TxId -> CTxId
txIdToCTxId = mkCTxId . sformat hashHexF

mkCTx
    :: Address            -- ^ An address for which transaction info is forming
    -> ChainDifficulty    -- ^ Current chain difficulty (to get confirmations)
    -> TxHistoryEntry     -- ^ Tx history entry
    -> CTxMeta            -- ^ Transaction metadata
    -> CTx
mkCTx addr diff THEntry {..} meta = CTx {..}
  where
    ctId = txIdToCTxId _thTxId
    ctAccAddress = addressToCAddress addr
    outputs = toList $ _txOutputs _thTx
    isToItself = all ((== addr) . txOutAddress) outputs
    ctAmount = unsafeIntegerToCoin . sumCoins . map txOutValue $
        filter ((|| isToItself) . xor _thIsOutput . (== addr) . txOutAddress) outputs
    ctConfirmations = maybe 0 fromIntegral $ (diff -) <$> _thDifficulty
    ctType = if _thIsOutput
             then CTOut meta
             else CTIn meta


newtype CPassPhrase = CPassPhrase Text deriving (Eq, Generic)

instance Show CPassPhrase where
    show _ = "<pass phrase>"

passPhraseToCPassPhrase :: PassPhrase -> CPassPhrase
passPhraseToCPassPhrase passphrase =
    CPassPhrase . Base16.encode $ encodeStrict passphrase

cPassPhraseToPassPhrase
    :: CPassPhrase -> Either Text PassPhrase
cPassPhraseToPassPhrase (CPassPhrase text) =
    first toText . decodeFull . LBS.fromStrict =<< Base16.decode text

----------------------------------------------------------------------------
-- Wallet
----------------------------------------------------------------------------

-- | Wallet identifier
data CWalletAddress = CWalletAddress
    { -- | Address of wallet set this wallet belongs to
      cwaWSAddress :: CAddress WS
    , -- | Derivation index of this wallet key
      cwaIndex     :: Word32
    } deriving (Eq, Show, Generic, Typeable)

instance Hashable CWalletAddress

instance Buildable CWalletAddress where
    build CWalletAddress{..} =
        bprint (F.build%"@"%F.build) cwaWSAddress cwaIndex

-- | Account identifier
data CAccountAddress = CAccountAddress
    { -- | Address of wallet set this account belongs to
      caaWSAddress    :: CAddress WS
    , -- | First index in derivation path of this account key
      caaWalletIndex  :: Word32
    , -- | Second index in derivation path of this account key
      caaAccountIndex :: Word32
    , -- | Actual adress of this account
      caaAddress      :: CAddress Acc
    } deriving (Eq, Show, Generic, Typeable)

instance Buildable CAccountAddress where
    build CAccountAddress{..} =
        bprint (F.build%"@"%F.build%"@"%F.build%" ("%F.build%")")
        caaWSAddress caaWalletIndex caaAccountIndex caaAddress

walletAddrByAccount :: CAccountAddress -> CWalletAddress
walletAddrByAccount CAccountAddress{..} = CWalletAddress
    { cwaWSAddress = caaWSAddress
    , cwaIndex     = caaWalletIndex
    }

instance Hashable CAccountAddress

-- | A wallet can be used as personal or shared wallet
data CWalletType
    = CWTPersonal
    | CWTShared
    deriving (Show, Generic)

-- | A level of assurance for the wallet "meta type"
data CWalletAssurance
    = CWAStrict
    | CWANormal
    deriving (Show, Generic)

-- | Single account in a wallet
data CAccount = CAccount
    { caAddress :: !CAccountAddress
    , caAmount  :: !Coin
    } deriving (Show, Generic)

-- Includes data which are not provided by Cardano
data CWalletMeta = CWalletMeta
    { cwType      :: !CWalletType
    , cwCurrency  :: !CCurrency
    , cwName      :: !Text
    , cwAssurance :: !CWalletAssurance
    , cwUnit      :: !Int -- ^ https://issues.serokell.io/issue/CSM-163#comment=96-2480
    } deriving (Show, Generic)

instance Default CWalletMeta where
    def = CWalletMeta CWTPersonal ADA "Personal Wallet" CWANormal 0

-- | Client Wallet (CW)
-- (Flow type: walletType)
data CWallet = CWallet
    { cwAddress  :: !CWalletAddress
    , cwMeta     :: !CWalletMeta
    , cwAccounts :: ![CAccount]
    } deriving (Show, Generic, Typeable)

-- | Query data for wallet creation
data CWalletInit = CWalletInit
    { cwInitMeta   :: !CWalletMeta
    , cwInitWSetId :: !(CAddress WS)
    } deriving (Show, Generic)

-- | Query data for redeem
data CWalletRedeem = CWalletRedeem
    { crWalletId :: !CWalletAddress
    , crSeed     :: !Text -- TODO: newtype!
    } deriving (Show, Generic)

-- | Meta data of 'CWalletSet'
data CWalletSetMeta = CWalletSetMeta
    { cwsName         :: !Text
    , cwsBackupPhrase :: !BackupPhrase
    } deriving (Show, Eq, Generic)

instance Default BackupPhrase where
    def = mkBackupPhrase
        [ "transfer"
        , "uniform"
        , "grunt"
        , "excess"
        , "six"
        , "veteran"
        , "vintage"
        , "warm"
        , "confirm"
        , "vote"
        , "nephew"
        , "allow"
        ]

instance Default CWalletSetMeta where
    def = CWalletSetMeta "Personal Wallet Set" def

-- | Client Wallet (CW)
data CWalletSet = CWalletSet
    { cwsAddress       :: !(CAddress WS)
    , cwsWSetMeta      :: !CWalletSetMeta
    , cwsWalletsNumber :: !Int
    } deriving (Eq, Show, Generic)

-- | Query data for wallet set creation
data CWalletSetInit = CWalletSetInit
    { cwsInitMeta     :: !CWalletSetMeta
    } deriving (Eq, Show, Generic)

class WithDerivationPath a where
    getDerivationPath :: a -> [Word32]

instance WithDerivationPath (CAddress WS) where
    getDerivationPath _ = []

instance WithDerivationPath CWalletAddress where
    getDerivationPath CWalletAddress{..} = [cwaIndex]

instance WithDerivationPath CAccountAddress where
    getDerivationPath CAccountAddress{..} = [caaWalletIndex, caaAccountIndex]

----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

-- | Password hash of client profile
type CPwHash = Text -- or Base64 or something else

-- | Client profile (CP)
-- all data of client are "meta data" - that is not provided by Cardano
-- (Flow type: accountType)
data CProfile = CProfile
    { cpLocale      :: Text
    } deriving (Show, Generic, Typeable)

----------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------

-- | meta data of transactions
data CTxMeta = CTxMeta
    { ctmCurrency    :: CCurrency
    , ctmTitle       :: Text
    , ctmDescription :: Text
    , ctmDate        :: POSIXTime
    } deriving (Show, Generic)

-- | type of transactions
-- It can be an input / output / exchange transaction
-- CTInOut CTExMeta -- Ex == exchange
data CTType
    = CTIn CTxMeta
    | CTOut CTxMeta
    deriving (Show, Generic)

ctTypeMeta :: Lens' CTType CTxMeta
ctTypeMeta f (CTIn meta)  = CTIn <$> f meta
ctTypeMeta f (CTOut meta) = CTOut <$> f meta

-- | Client transaction (CTx)
-- Provides all Data about a transaction needed by client.
-- It includes meta data which are not part of Cardano, too
-- (Flow type: transactionType)
data CTx = CTx
    { ctId            :: CTxId
    , ctAmount        :: Coin
    , ctConfirmations :: Word
    , ctType          :: CTType -- it includes all "meta data"
    , ctAccAddress    :: CAddress Acc
    } deriving (Show, Generic, Typeable)

ctType' :: Lens' CTx CTType
ctType' f (CTx id amount cf tp acc) = f tp <&> \tp' -> CTx id amount cf tp' acc

txContainsTitle :: Text -> CTx -> Bool
txContainsTitle search = isInfixOf (toLower search) . toLower . ctmTitle . view (ctType' . ctTypeMeta)

-- | meta data of exchanges
data CTExMeta = CTExMeta
    { cexCurrency    :: CCurrency
    , cexTitle       :: Text
    , cexDescription :: Text
    , cexDate        :: POSIXTime
    , cexRate        :: Text
    , cexLabel       :: Text -- counter part of client's 'exchange' value
    , cexAddress     :: CAddress Acc
    } deriving (Show, Generic)

-- | Update system data
data CUpdateInfo = CUpdateInfo
    { cuiSoftwareVersion :: !SoftwareVersion
    , cuiBlockVesion     :: !BlockVersion
    , cuiScriptVersion   :: !ScriptVersion
    , cuiImplicit        :: !Bool
--    , cuiProposed        :: !HeaderHash
--    , cuiDecided         :: !HeaderHash
--    , cuiConfirmed       :: !HeaderHash
--    , cuiAdopted         :: !(Maybe HeaderHash)
    , cuiVotesFor        :: !Int
    , cuiVotesAgainst    :: !Int
    , cuiPositiveStake   :: !Coin
    , cuiNegativeStake   :: !Coin
    } deriving (Show, Generic, Typeable)

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
        cuiScriptVersion    = bvdScriptVersion upBlockVersionData
        cuiImplicit         = cpsImplicit
--        cuiProposed         = cpsProposed
--        cuiDecided          = cpsDecided
--        cuiConfirmed        = cpsConfirmed
--        cuiAdopted          = cpsAdopted
        (cuiVotesFor, cuiVotesAgainst) = countVotes cpsVotes
        cuiPositiveStake    = cpsPositiveStake
        cuiNegativeStake    = cpsNegativeStake
    in CUpdateInfo {..}

----------------------------------------------------------------------------
-- Reporting
----------------------------------------------------------------------------

-- | Represents a knowledge about how much time did it take for client
-- (wallet) to initialize. All numbers are milliseconds.
data CInitialized = CInitialized
    { cTotalTime :: Word -- ^ Total time from very start to main
                            -- post-sync screen.
    , cPreInit   :: Word -- ^ Time passed from beginning to network
                            -- connection with peers established.
    } deriving (Show, Generic)
