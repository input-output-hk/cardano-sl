{-# LANGUAGE TypeFamilies #-}

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
      , CTxId (..)
      , NewBatchPayment (..)
      , CTxMeta (..)
      , CTExMeta (..)
      , CPtxCondition (..)
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
      , CCoin (..)
      , PassPhraseLU
      , CElectronCrashReport (..)
      , Wal (..)
      , Addr (..)
      , SinceTime (..)
      , ScrollOffset (..)
      , ScrollLimit (..)
      , CFilePath (..)
      , ApiVersion (..)
      , ClientInfo (..)
      ) where

import           Universum

import           Control.Lens          (makeLenses)
import           Data.Default          (Default, def)
import           Data.Hashable         (Hashable (..))
import           Data.Text             (Text)
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Typeable         (Typeable)
import           Data.Version          (Version)
import           Formatting            (bprint, build, later, (%))
import qualified Formatting            as F
import qualified Prelude
import           Serokell.Util         (mapBuilder)
import           Servant.Multipart     (FileData)

import           Pos.Aeson.Types       ()
import           Pos.Client.Txp.Util   (InputSelectionPolicy)
import           Pos.Core.Types        (Coin, ScriptVersion)
import           Pos.Types             (BlockVersion, ChainDifficulty, SoftwareVersion)
import           Pos.Util.BackupPhrase (BackupPhrase)
import           Pos.Util.LogSafe      (SecureLog, buildUnsecure)

-- TODO [CSM-407] Structurize this mess

data SyncProgress = SyncProgress
    { _spLocalCD   :: ChainDifficulty
    , _spNetworkCD :: Maybe ChainDifficulty
    , _spPeers     :: Word
    } deriving (Show, Generic, Typeable)

makeLenses ''SyncProgress

instance Default SyncProgress where
    def = SyncProgress 0 mzero 0

-- | Client hash
newtype CHash = CHash Text
    deriving (Show, Eq, Ord, Generic, Buildable)

instance Hashable CHash where
    hashWithSalt s (CHash h) = hashWithSalt s h

-- | Client address
-- @w@ is phantom type and stands for type of item this address belongs to.
newtype CId w = CId CHash
    deriving (Show, Eq, Ord, Generic, Hashable, Buildable)

-- | Marks address as belonging to wallet set.
data Wal = Wal
    deriving (Show, Generic)

-- | Marks address as belonging to account.
data Addr = Addr
    deriving (Show, Generic)

-- | Client transaction id
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Hashable, Buildable)

newtype CPassPhrase = CPassPhrase Text
    deriving (Eq, Generic)

instance Show CPassPhrase where
    show _ = "<pass phrase>"

----------------------------------------------------------------------------
-- Wallet
----------------------------------------------------------------------------

-- | Wallet identifier
data AccountId = AccountId
    { -- | Address of wallet this wallet belongs to
      aiWId   :: CId Wal
    , -- | Derivation index of this wallet key
      aiIndex :: Word32
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Hashable AccountId

instance Buildable AccountId where
    build AccountId{..} =
        bprint (F.build%"@"%F.build) aiWId aiIndex

instance Buildable (SecureLog AccountId) where
    build _ = "<account id>"

instance Buildable (SecureLog (Maybe AccountId)) where
    build = buildUnsecure

newtype CAccountId = CAccountId Text
    deriving (Eq, Show, Generic, Buildable)

-- TODO: extract first three fields as @Coordinates@ and use only it where
-- required (maybe nowhere)
-- | Account identifier
data CWAddressMeta = CWAddressMeta
    { -- | Address of wallet this account belongs to
      cwamWId          :: CId Wal
    , -- | First index in derivation path of this account key
      cwamAccountIndex :: Word32
    , -- | Second index in derivation path of this account key
      cwamAddressIndex :: Word32
    , -- | Actual address
      cwamId           :: CId Addr
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Buildable CWAddressMeta where
    build CWAddressMeta{..} =
        bprint (F.build%"@"%F.build%"@"%F.build%" ("%F.build%")")
        cwamWId cwamAccountIndex cwamAddressIndex cwamId

instance Hashable CWAddressMeta

newtype CCoin = CCoin
    { getCCoin :: Text
    } deriving (Show, Eq, Generic, Buildable)

-- | Passphrase last update time
type PassPhraseLU = POSIXTime

-- | A level of assurance for the wallet "meta type"
data CWalletAssurance
    = CWAStrict
    | CWANormal
    deriving (Show, Eq, Generic)

-- | Single address in a account
data CAddress = CAddress
    { cadId       :: !(CId Addr)
    , cadAmount   :: !CCoin
    , cadIsUsed   :: !Bool
    , cadIsChange :: !Bool -- ^ Is this a change address
    } deriving (Show, Generic)

-- Includes data which are not provided by Cardano
data CAccountMeta = CAccountMeta
    { caName      :: !Text
    } deriving (Show, Generic)

instance Default CAccountMeta where
    def = CAccountMeta "Personal Wallet"

-- | Client Account (CA)
-- (Flow type: accountType)
data CAccount = CAccount
    { caId        :: !CAccountId
    , caMeta      :: !CAccountMeta
    , caAddresses :: ![CAddress]
    , caAmount    :: !CCoin
    } deriving (Show, Generic, Typeable)

-- | Query data for account creation
data CAccountInit = CAccountInit
    { caInitMeta :: !CAccountMeta
    , caInitWId  :: !(CId Wal)
    } deriving (Show, Generic)

-- | Query data for redeem
data CWalletRedeem = CWalletRedeem
    { crWalletId :: !CAccountId
    , crSeed     :: !Text -- TODO: newtype!
    } deriving (Show, Generic)

-- | Meta data of 'CWallet'
data CWalletMeta = CWalletMeta
    { cwName      :: !Text
    , cwAssurance :: !CWalletAssurance
    , cwUnit      :: !Int -- ^ https://issues.serokell.io/issue/CSM-163#comment=96-2480
    } deriving (Show, Eq, Generic)

instance Default CWalletMeta where
    def = CWalletMeta "Personal Wallet Set" CWANormal 0

-- | Client Wallet (CW)
data CWallet = CWallet
    { cwId             :: !(CId Wal)
    , cwMeta           :: !CWalletMeta
    , cwAccountsNumber :: !Int
    , cwAmount         :: !CCoin
    , cwHasPassphrase  :: !Bool
    , cwPassphraseLU   :: !PassPhraseLU  -- last update time
    } deriving (Eq, Show, Generic)

-- | Query data for wallet creation
data CWalletInit = CWalletInit
    { cwInitMeta     :: !CWalletMeta
    , cwBackupPhrase :: !BackupPhrase
    } deriving (Eq, Show, Generic)

-- | Query data for redeem
data CPaperVendWalletRedeem = CPaperVendWalletRedeem
    { pvWalletId     :: !CAccountId
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
-- TODO: Newtype?
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
    { ctmDate        :: POSIXTime
    } deriving (Show, Generic)

-- | State of transaction, corresponding to
-- 'Pos.Wallet.Web.Pending.Types.PtxCondition'.
-- @PtxInNewestBlocks@ and @PtxPersisted@ states are merged into one
-- not to provide information which conflicts with 'ctConfirmations'.
data CPtxCondition
    = CPtxApplying
    | CPtxInBlocks
    | CPtxWontApply
    | CPtxNotTracked  -- ^ tx was made not in this life
    deriving (Show, Eq, Generic, Typeable)

-- | Client transaction (CTx)
-- Provides all Data about a transaction needed by client.
-- It includes meta data which are not part of Cardano, too
-- (Flow type: transactionType)
data CTx = CTx
    { ctId            :: CTxId
    , ctAmount        :: CCoin
    , ctConfirmations :: Word
    , ctMeta          :: CTxMeta
    , ctInputs        :: [(CId Addr, CCoin)]
    , ctOutputs       :: [(CId Addr, CCoin)]
    , ctIsLocal       :: Bool
    , ctIsOutgoing    :: Bool
    , ctCondition     :: CPtxCondition
    } deriving (Show, Generic, Typeable)

-- | meta data of exchanges
data CTExMeta = CTExMeta
    { cexTitle       :: Text
    , cexDescription :: Text
    , cexDate        :: POSIXTime
    , cexRate        :: Text
    , cexLabel       :: Text -- counter part of client's 'exchange' value
    , cexId          :: CId Addr
    } deriving (Show, Generic)

data NewBatchPayment = NewBatchPayment
    { npbFrom                 :: CAccountId
    , npbTo                   :: NonEmpty (CId Addr, Coin)
    , npbPolicy               :: InputSelectionPolicy
    } deriving (Show, Generic)

instance Buildable NewBatchPayment where
    build NewBatchPayment{..} =
        bprint ("{ from="%build%" to="%later mapBuilder%" policy="%build%" }")
               npbFrom npbTo npbPolicy

instance Buildable (SecureLog NewBatchPayment) where
    build = buildUnsecure

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

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

newtype SinceTime = SinceTime Word
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic, Typeable,
              Buildable)

newtype ScrollOffset = ScrollOffset Word
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic, Typeable,
              Buildable)

newtype ScrollLimit = ScrollLimit Word
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic, Typeable,
              Buildable)

newtype CFilePath = CFilePath Text
    deriving (Eq, Ord, Generic, Typeable, Buildable)

----------------------------------------------------------------------------
-- Version and client info
----------------------------------------------------------------------------

-- | Version of wallet API. Currently we have only 0-th version. We
-- will add new constructors when new versions appear.
data ApiVersion =
    ApiVersion0
    deriving (Show, Generic)

-- | Information about this client.
data ClientInfo = ClientInfo
    { ciApiVersion      :: !ApiVersion
    -- ^ Version of wallet API.
    , ciSoftwareVersion :: !SoftwareVersion
    -- ^ Software version (from the blockchain's point of view).
    , ciCabalVersion    :: !Version
    -- ^ Version specified in cabal file.
    , ciGitRevision     :: !Text
    -- ^ Git revision from which this software was built.
    } deriving (Show, Generic)
