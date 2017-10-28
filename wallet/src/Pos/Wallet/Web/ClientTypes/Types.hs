{-# LANGUAGE TypeFamilies #-}

-- | Types representing client (wallet) requests on wallet API.
module Pos.Wallet.Web.ClientTypes.Types
      ( SyncProgress (..)
      , spLocalCD
      , spNetworkCD
      , spPeers

        -- * Identifiers & primitives
      , CId (..)
      , CHash (..)
      , CPassPhrase (..)
      , CTxId (..)
      , CAccountId (..)
      , CCoin (..)
      , Wal (..)
      , Addr (..)
      , PassPhraseLU

        -- * Wallets
        -- ** Structure & metas
      , CWalletAssurance (..)
      , CWalletMeta (..)
      , AccountId (..)
      , CAccountMeta (..)
      , CWAddressMeta (..)

        -- ** Requests
      , CWalletInit (..)
      , CWalletRedeem (..)
      , CPaperVendWalletRedeem (..)
      , CAccountInit (..)

        -- ** Responses
      , CWallet (..)
      , CAccount (..)
      , CAddress (..)

        -- * Transactions
      , CPtxCondition (..)
      , CTxMeta (..)
      , CTx (..)
      , CTExMeta (..)
      , CUpdateInfo (..)
      , mkCTxId

        -- * Profile
      , CProfile (..)
      , CPwHash

        -- * Reporting
      , CInitialized (..)
      , CElectronCrashReport (..)

        -- * Misc
      , ScrollOffset (..)
      , ScrollLimit (..)
      , CFilePath (..)
      ) where

import           Universum

import           Control.Lens          (makeLenses)
import           Data.Default          (Default, def)
import           Data.Hashable         (Hashable (..))
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Typeable         (Typeable)
import           Formatting            (bprint, build, builder, shown, (%))
import qualified Prelude
import           Serokell.Util         (listJsonIndent)
import           Servant.Multipart     (FileData)

import           Pos.Core.Types        (ScriptVersion)
import           Pos.Types             (BlockVersion, ChainDifficulty, SoftwareVersion)
import           Pos.Util.BackupPhrase (BackupPhrase)
import           Pos.Util.LogSafe      (SecureLog, buildUnsecure)
import           Pos.Util.Servant      (HasTruncateLogPolicy, WithTruncatedLog (..))

data SyncProgress = SyncProgress
    { _spLocalCD   :: ChainDifficulty
    , _spNetworkCD :: Maybe ChainDifficulty
    , _spPeers     :: Word
    } deriving (Show, Generic, Typeable)

makeLenses ''SyncProgress

instance Buildable SyncProgress where
    build SyncProgress{..} =
        bprint ("progress="%build%"/"%build%" peers="%build)
               _spLocalCD _spNetworkCD _spPeers

instance Default SyncProgress where
    def = SyncProgress 0 mzero 0

----------------------------------------------------------------------------
-- Identifiers & primitives
----------------------------------------------------------------------------

-- | Client hash
newtype CHash = CHash Text
    deriving (Show, Eq, Ord, Generic, Buildable)

instance Hashable CHash where
    hashWithSalt s (CHash h) = hashWithSalt s h

-- | Client address
-- @w@ is phantom type and stands for type of item this id belongs to.
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

mkCTxId :: Text -> CTxId
mkCTxId = CTxId . CHash

newtype CPassPhrase = CPassPhrase Text
    deriving (Eq, Generic)

instance Show CPassPhrase where
    show _ = "<pass phrase>"

newtype CAccountId = CAccountId Text
    deriving (Eq, Show, Generic, Buildable)

newtype CCoin = CCoin
    { getCCoin :: Text
    } deriving (Show, Eq, Generic, Buildable)

-- | Passphrase last update time
type PassPhraseLU = POSIXTime

----------------------------------------------------------------------------
-- Wallet structure & metas
----------------------------------------------------------------------------

data AccountId = AccountId
    { -- | Address of wallet this account belongs to
      aiWId   :: CId Wal
    , -- | Derivation index of this account key
      aiIndex :: Word32
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Hashable AccountId

instance Buildable AccountId where
    build AccountId{..} =
        bprint (build%"@"%build) aiWId aiIndex

instance Buildable (SecureLog AccountId) where
    build _ = "<account id>"

instance Buildable (SecureLog (Maybe AccountId)) where
    build = buildUnsecure

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
        bprint (build%"@"%build%"@"%build%" ("%build%")")
        cwamWId cwamAccountIndex cwamAddressIndex cwamId

instance Hashable CWAddressMeta

-- | A level of assurance for the wallet "meta type"
data CWalletAssurance
    = CWAStrict
    | CWANormal
    deriving (Show, Eq, Generic)

instance Buildable CWalletAssurance where
    build = bprint shown

-- | Meta data of 'CWallet'
data CWalletMeta = CWalletMeta
    { cwName      :: !Text
    , cwAssurance :: !CWalletAssurance
    , cwUnit      :: !Int -- ^ https://issues.serokell.io/issue/CSM-163#comment=96-2480
    } deriving (Show, Eq, Generic)

instance Buildable CWalletMeta where
    build CWalletMeta{..} =
        bprint ("("%build%"/"%build%")")
               cwAssurance cwUnit

instance Default CWalletMeta where
    def = CWalletMeta "Personal Wallet Set" CWANormal 0

-- Includes data which are not provided by Cardano
data CAccountMeta = CAccountMeta
    { caName      :: !Text
    } deriving (Eq, Show, Generic)

instance Buildable CAccountMeta where
    build CAccountMeta{..} = "<meta>"

instance Default CAccountMeta where
    def = CAccountMeta "Personal Wallet"

----------------------------------------------------------------------------
-- Wallet structure - requests
----------------------------------------------------------------------------

-- | Query data for wallet creation
data CWalletInit = CWalletInit
    { cwInitMeta     :: !CWalletMeta
    , cwBackupPhrase :: !BackupPhrase
    } deriving (Eq, Show, Generic)

instance Buildable CWalletInit where
    build CWalletInit{..} =
        bprint (build%" / "%build)
               cwBackupPhrase cwInitMeta

-- | Query data for redeem
data CWalletRedeem = CWalletRedeem
    { crWalletId :: !CAccountId
    , crSeed     :: !Text -- TODO: newtype!
    } deriving (Show, Generic)

instance Buildable CWalletRedeem where
    build CWalletRedeem{..} =
        bprint (build%" <- "%build)
               crWalletId crSeed

-- | Query data for redeem
data CPaperVendWalletRedeem = CPaperVendWalletRedeem
    { pvWalletId     :: !CAccountId
    , pvSeed         :: !Text -- TODO: newtype!
    , pvBackupPhrase :: !BackupPhrase
    } deriving (Show, Generic)

instance Buildable CPaperVendWalletRedeem where
    build CPaperVendWalletRedeem{..} =
        bprint (build%" <- "%build%" / "%build)
               pvWalletId pvSeed pvBackupPhrase

-- | Query data for account creation
data CAccountInit = CAccountInit
    { caInitMeta :: !CAccountMeta
    , caInitWId  :: !(CId Wal)
    } deriving (Show, Generic)

instance Buildable CAccountInit where
    build CAccountInit{..} =
        bprint (build%" / "%build)
               caInitWId caInitMeta

----------------------------------------------------------------------------
-- Wallet struture - responses
----------------------------------------------------------------------------

-- | Client Wallet (CW)
data CWallet = CWallet
    { cwId             :: !(CId Wal)
    , cwMeta           :: !CWalletMeta
    , cwAccountsNumber :: !Int
    , cwAmount         :: !CCoin
    , cwHasPassphrase  :: !Bool
    , cwPassphraseLU   :: !PassPhraseLU  -- last update time
    } deriving (Eq, Show, Generic)

instance Buildable CWallet where
    build CWallet{..} =
        bprint ("{ id="%build
                %" meta="%build
                %" accs="%build
                %" amount="%build
                %" pass="%build
                %" passlu="%build
                %" }")
        cwId
        cwMeta
        cwAccountsNumber
        cwAmount
        cwHasPassphrase
        cwPassphraseLU

-- | Client Account (CA)
-- (Flow type: accountType)
data CAccount = CAccount
    { caId        :: !CAccountId
    , caMeta      :: !CAccountMeta
    , caAddresses :: ![CAddress]
    , caAmount    :: !CCoin
    } deriving (Show, Generic, Typeable)

instance Buildable CAccount where
    build CAccount{..} =
        bprint ("{ id="%build
                %" meta="%build
                %" amount="%build%"\n"
                %" addrs="%listJsonIndent 4
                %" }")
        caId
        caMeta
        caAmount
        caAddresses

instance HasTruncateLogPolicy CAddress =>
         Buildable (WithTruncatedLog CAccount) where
    build (WithTruncatedLog CAccount {..}) =
        bprint
            ("{ id=" %build % " meta=" %build % " amount=" %build % "\n" %
             " addrs=" %build %
             " }")
            caId
            caMeta
            caAmount
            (WithTruncatedLog caAddresses)

-- | Single address in a account
data CAddress = CAddress
    { cadId       :: !(CId Addr)
    , cadAmount   :: !CCoin
    , cadIsUsed   :: !Bool
    , cadIsChange :: !Bool -- ^ Is this a change address
    } deriving (Show, Generic)

instance Buildable CAddress where
    build CAddress{..} =
        bprint ("{ id="%build%"\n"
                %" amount="%build
                %" used="%build
                %" change="%build
                %" }")
        cadId
        cadAmount
        cadIsUsed
        cadIsChange

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
    } deriving (Eq, Show, Generic, Typeable)

instance Buildable CProfile where
    build CProfile{..} =
        bprint ("{ cpLocale="%build%" }") cpLocale

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
    } deriving (Eq, Show, Generic)

instance Buildable CTxMeta where
    build CTxMeta{..} = bprint ("{ date="%build%" }") ctmDate

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

instance Buildable CPtxCondition where
    build = bprint shown

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

instance Buildable CTx where
    build CTx{..} =
        bprint ("{ id="%build
                %" amount="%build
                %" confirms="%build
                %" meta="%build%"\n"
                %" inputs="%builder%"\n"
                %" outputs="%builder%"\n"
                %" local="%build
                %" outgoing="%build
                %" condition="%build
                %" }")
        ctId
        ctAmount
        ctConfirmations
        ctMeta
        (buildTxEnds ctInputs)
        (buildTxEnds ctOutputs)
        ctIsLocal
        ctIsOutgoing
        ctCondition
      where
        buildTxEnds =
            mconcat . intersperse ", " .
            map (uncurry $ bprint (build%" - "%build))

-- | meta data of exchanges
data CTExMeta = CTExMeta
    { cexTitle       :: Text
    , cexDescription :: Text
    , cexDate        :: POSIXTime
    , cexRate        :: Text
    , cexLabel       :: Text -- counter part of client's 'exchange' value
    , cexId          :: CId Addr
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
    } deriving (Eq, Show, Generic, Typeable)

instance Buildable CUpdateInfo where
    build CUpdateInfo{..} =
        bprint ("{ softver="%build
                %" blockver="%build
                %" scriptver="%build
                %" implicit="%build
                %" for="%build
                %" against="%build
                %" pos stake="%build
                %" neg stake="%build
                %" }")
        cuiSoftwareVersion
        cuiBlockVesion  -- TODO [CSM-407] lol what is it?
        cuiScriptVersion
        cuiImplicit
        cuiVotesFor
        cuiVotesAgainst
        cuiPositiveStake
        cuiNegativeStake

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

instance Buildable CInitialized where
    build CInitialized{..} =
        bprint (build%"/"%build)
               cPreInit cTotalTime

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

newtype ScrollOffset = ScrollOffset Word
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic, Typeable,
              Buildable)

newtype ScrollLimit = ScrollLimit Word
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic, Typeable,
              Buildable)

newtype CFilePath = CFilePath Text
    deriving (Eq, Ord, Generic, Typeable, Buildable)
