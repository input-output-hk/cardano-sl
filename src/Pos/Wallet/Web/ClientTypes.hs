{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- This module is to be moved later anywhere else, just to have a
-- starting point

-- | Types representing client (wallet) requests on wallet API.
module Pos.Wallet.Web.ClientTypes
      ( SyncProgress (..)
      , CAddress (..)
      , CCurrency (..)
      , CHash (..)
      , CPassPhrase (..)
      , MCPassPhrase
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
      , CPaperVendWalletRedeem (..)
      , CCoin
      , mkCCoin
      , PassPhraseLU
      , CElectronCrashReport (..)
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
      , txContainsTitle
      , toCUpdateInfo
      , walletAddrByAccount
      , WalletUserSecret (..)
      , readWalletUserSecret
      , writeWalletUserSecret
      ) where

import           Universum

import           Control.Arrow          ((&&&))
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Default           (Default, def)
import           Data.Hashable          (Hashable (..))
import           Data.Text              (Text, isInfixOf, toLower)
import           Data.Text.Buildable    (build)
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Data.Typeable          (Typeable)
import           Formatting             (bprint, sformat, (%))
import qualified Formatting             as F
import qualified Prelude
import qualified Serokell.Util.Base16   as Base16
import           Servant.Multipart      (FileData, FromMultipart (..), lookupFile,
                                         lookupInput)
import           System.IO              (withFile)
import           System.Wlog            (WithLogger)

import           Pos.Aeson.Types        ()
import           Pos.Binary.Class       (Bi (..), decodeFull, encode, encodeStrict, label)
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core.Types         (ScriptVersion)
import           Pos.Crypto             (EncryptedSecretKey, PassPhrase, hashHexF)
import           Pos.Txp.Core.Types     (Tx (..), TxId, TxOut, txOutAddress, txOutValue)
import           Pos.Types              (Address (..), BlockVersion, ChainDifficulty,
                                         Coin, SoftwareVersion, decodeTextAddress,
                                         sumCoins, unsafeGetCoin, unsafeIntegerToCoin)
import           Pos.Update.Core        (BlockVersionData (..), StakeholderVotes,
                                         UpdateProposal (..), isPositiveVote)
import           Pos.Update.Poll        (ConfirmedProposalState (..))
import           Pos.Util.BackupPhrase  (BackupPhrase)
import           Pos.Util.UserSecret    (ensureModeIs600)


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
newtype CHash = CHash Text
    deriving (Show, Eq, Generic, Buildable)

instance Hashable CHash where
    hashWithSalt s (CHash h) = hashWithSalt s h

-- | Client address
-- @w@ is phantom type and stands for type of item this address belongs to.
newtype CAddress w = CAddress CHash
    deriving (Show, Eq, Generic, Hashable, Buildable)

-- | Marks address as belonging to wallet set.
data WS = WS

-- | Marks address as belonging to account.
data Acc = Acc

-- TODO: this is not completely safe. If someone changes
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
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Hashable)

mkCTxId :: Text -> CTxId
mkCTxId = CTxId . CHash

-- | transform TxId into CTxId
txIdToCTxId :: TxId -> CTxId
txIdToCTxId = mkCTxId . sformat hashHexF

convertTxOutputs :: [TxOut] -> [(CAddress w, CCoin)]
convertTxOutputs = map (addressToCAddress . txOutAddress &&& mkCCoin . txOutValue)

mkCTx
    :: ChainDifficulty    -- ^ Current chain difficulty (to get confirmations)
    -> TxHistoryEntry     -- ^ Tx history entry
    -> CTxMeta            -- ^ Transaction metadata
    -> CTx
mkCTx diff THEntry {..} meta = CTx {..}
  where
    ctId = txIdToCTxId _thTxId
    outputs = toList $ _txOutputs _thTx
    ctAmount = mkCCoin . unsafeIntegerToCoin . sumCoins $ map txOutValue outputs
    ctConfirmations = maybe 0 fromIntegral $ (diff -) <$> _thDifficulty
    ctMeta = meta
    ctInputAddrs = map addressToCAddress _thInputAddrs
    ctOutputAddrs = map addressToCAddress _thOutputAddrs

newtype CPassPhrase = CPassPhrase Text
    deriving (Eq, Generic)

-- | This is most common use case for 'CPassPhrase', as there is a default
-- value for it
type MCPassPhrase = Maybe CPassPhrase

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

newtype CCoin = CCoin
    { getCoin :: Text
    } deriving (Show, Generic)

mkCCoin :: Coin -> CCoin
mkCCoin = CCoin . show . unsafeGetCoin

-- | Passphrase last update time
type PassPhraseLU = POSIXTime

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
    { caAddress :: !(CAddress Acc)
    , caAmount  :: !CCoin
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
    { cwsName :: !Text
    } deriving (Show, Eq, Generic)


instance Default CWalletSetMeta where
  def = CWalletSetMeta "Personal Wallet Set"

-- | Client Wallet Set (CW)
data CWalletSet = CWalletSet
    { cwsAddress       :: !(CAddress WS)
    , cwsWSetMeta      :: !CWalletSetMeta
    , cwsWalletsNumber :: !Int
    , cwsHasPassphrase :: !Bool
    , cwsPassphraseLU  :: !PassPhraseLU  -- last update time
    } deriving (Eq, Show, Generic)

-- | Query data for wallet set creation
data CWalletSetInit = CWalletSetInit
    { cwsInitMeta     :: !CWalletSetMeta
    , cwsBackupPhrase :: !BackupPhrase
    } deriving (Eq, Show, Generic)

class WithDerivationPath a where
    getDerivationPath :: a -> [Word32]

instance WithDerivationPath (CAddress WS) where
    getDerivationPath _ = []

instance WithDerivationPath CWalletAddress where
    getDerivationPath CWalletAddress{..} = [cwaIndex]

instance WithDerivationPath CAccountAddress where
    getDerivationPath CAccountAddress{..} = [caaWalletIndex, caaAccountIndex]

-- | Query data for redeem
data CPaperVendWalletRedeem = CPaperVendWalletRedeem
    { pvWalletId     :: !CWalletAddress
    , pvSeed         :: !Text -- TODO: newtype!
    , pvBackupPhrase :: !BackupPhrase
    } deriving (Show, Generic)

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

-- | Added default instance for `testReset`, we need an inital state for
-- @CProfile@
instance Default CProfile where
    def = CProfile mempty

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

-- | Client transaction (CTx)
-- Provides all Data about a transaction needed by client.
-- It includes meta data which are not part of Cardano, too
-- (Flow type: transactionType)
data CTx = CTx
    { ctId            :: CTxId
    , ctAmount        :: CCoin
    , ctConfirmations :: Word
    , ctMeta          :: CTxMeta
    , ctInputAddrs    :: [CAddress Acc]
    , ctOutputAddrs   :: [CAddress Acc]
    } deriving (Show, Generic, Typeable)

txContainsTitle :: Text -> CTx -> Bool
txContainsTitle search = isInfixOf (toLower search) . toLower . ctmTitle . ctMeta

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
    , cuiPositiveStake   :: !CCoin
    , cuiNegativeStake   :: !CCoin
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
        cuiPositiveStake    = mkCCoin cpsPositiveStake
        cuiNegativeStake    = mkCCoin cpsNegativeStake
    in CUpdateInfo {..}

----------------------------------------------------------------------------
-- UserSecret
----------------------------------------------------------------------------

-- | Describes HD wallets keyfile content
data WalletUserSecret = WalletUserSecret
    { wusRootKey  :: EncryptedSecretKey  -- ^ root key of wallet set
    , wusWSetName :: Text                -- ^ name of wallet set
    , wusWallets  :: [(Word32, Text)]    -- ^ coordinates and names wallets
    , wusAccounts :: [(Word32, Word32)]  -- ^ coordinates of accounts
    }

instance Bi WalletUserSecret where
    put WalletUserSecret{..} = do
        put wusRootKey
        put wusWSetName
        put wusWallets
        put wusAccounts
    get = label "WalletUserSecret" $ do
        wusRootKey <- get
        wusWSetName <- get
        wusWallets <- get
        wusAccounts <- get
        return WalletUserSecret{..}

readWalletUserSecret
    :: (MonadIO m, WithLogger m)
    => FilePath -> m (Either Text WalletUserSecret)
readWalletUserSecret path = do
    ensureModeIs600 path
    liftIO $ first toText . decodeFull <$> BSL.readFile path

writeWalletUserSecret :: MonadIO m => FilePath -> WalletUserSecret -> m ()
writeWalletUserSecret path secret = do
    liftIO $ withFile path WriteMode $ \handle ->
        BSL.hPut handle (encode secret)

----------------------------------------------------------------------------
-- Reportin
----------------------------------------------------------------------------

-- | Represents a knowledge about how much time did it take for client
-- (wallet) to initialize. All numbers are milliseconds.
data CInitialized = CInitialized
    { cTotalTime :: Word -- ^ Total time from very start to main
                            -- post-sync screen.
    , cPreInit   :: Word -- ^ Time passed from beginning to network
                            -- connection with peers established.
    } deriving (Show, Generic)


data CElectronCrashReport = CElectronCrashReport
    { cecVersion     :: Text
    , cecPlatform    :: Text
    , cecProcessType :: Text
    , cecGuid        :: Text
    , cecVersionJson :: Text
    , cecProductName :: Text
    , cecProd        :: Text
    , cecCompanyName :: Text
    , cecUploadDump  :: FileData
    } deriving (Show, Generic)

instance FromMultipart CElectronCrashReport where
    fromMultipart form = do
        let look t = lookupInput t form
        CElectronCrashReport
          <$> look "ver"
          <*> look "platform"
          <*> look "process_type"
          <*> look "guid"
          <*> look "_version"
          <*> look "_productName"
          <*> look "prod"
          <*> look "_companyName"
          <*> lookupFile "upload_file_minidump" form
