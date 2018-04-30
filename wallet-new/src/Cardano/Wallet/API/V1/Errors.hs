{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Cardano.Wallet.API.Response.JSend (ResponseStatus (ErrorStatus))
import           Data.Aeson
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Generics.SOP.TH (deriveGeneric)
import qualified Network.HTTP.Types as HTTP
import           Servant
import           Test.QuickCheck (Arbitrary (..), oneof)

import qualified Pos.Client.Txp.Util as TxError
import qualified Pos.Core as Core
import qualified Pos.Crypto.Hashing as Crypto
import qualified Pos.Data.Attributes as Core

import           Cardano.Wallet.API.V1.Generic (gparseJsend, gtoJsend)
import           Cardano.Wallet.API.V1.Types (SyncPercentage, SyncProgress (..), V1 (..),
                                              mkEstimatedCompletionTime, mkSyncPercentage,
                                              mkSyncThroughput)

--
-- Error handling
--

-- | Type representing any error which might be thrown by wallet.
--
-- Errors are represented in JSON in the JSend format (<https://labs.omniti.com/labs/jsend>):
-- ```
-- {
--     "status": "error"
--     "message" : <constr_name>,
--     "diagnostic" : <data>
-- }
-- ```
-- where `<constr_name>` is a string containing name of error's constructor (e. g. `NotEnoughMoney`),
-- and `<data>` is an object containing additional error data.
-- Additional data contains constructor fields, field names are record field names without
-- a `we` prefix, e. g. for `OutputIsRedeem` error "diagnostic" field will be the following:
-- ```
-- {
--     "address" : <address>
-- }
-- ```
--
-- Additional data in constructor should be represented as record fields.
-- Otherwise TemplateHaskell will raise an error.
--
-- If constructor does not have additional data (like in case of `WalletNotFound` error),
-- then "diagnostic" field will be empty object.
--
-- TODO: change fields' types to actual Cardano core types, like `Coin` and `Address`
data WalletError =
      NotEnoughMoney { weNeedMore :: !Int }
    | OutputIsRedeem { weAddress :: !(V1 Core.Address) }
    | MigrationFailed { weDescription :: !Text }
    | JSONValidationFailed { weValidationError :: !Text }
    | UnknownError { weMsg :: !Text }
    | InvalidAddressFormat { weMsg :: !Text }
    | WalletNotFound
    -- FIXME(akegalj): https://iohk.myjetbrains.com/youtrack/issue/CSL-2496
    | WalletAlreadyExists
    | AddressNotFound
    | TxFailedToStabilize
    | TxRedemptionDepleted
    | TxSafeSignerNotFound { weAddress :: V1 Core.Address }
    | MissingRequiredParams { requiredParams :: NonEmpty (Text, Text) }
    | WalletIsNotReadyToProcessPayments { weStillRestoring :: SyncProgress }
    -- ^ The @Wallet@ where a @Payment@ is being originated is not fully
    -- synced (its 'WalletSyncState' indicates it's either syncing or
    -- restoring) and thus cannot accept new @Payment@ requests.
    | NodeIsStillSyncing { wenssStillSyncing :: SyncPercentage }
    -- ^ The backend couldn't process the incoming request as the underlying
    -- node is still syncing with the blockchain.
    deriving (Show, Eq)

convertTxError :: TxError.TxError -> WalletError
convertTxError err = case err of
    TxError.NotEnoughMoney coin ->
        NotEnoughMoney . fromIntegral . Core.getCoin $ coin
    TxError.NotEnoughAllowedMoney coin ->
        NotEnoughMoney . fromIntegral . Core.getCoin $ coin
    TxError.FailedToStabilize ->
        TxFailedToStabilize
    TxError.OutputIsRedeem addr ->
        OutputIsRedeem (V1 addr)
    TxError.RedemptionDepleted ->
        TxRedemptionDepleted
    TxError.SafeSignerNotFound addr ->
        TxSafeSignerNotFound (V1 addr)
    TxError.GeneralTxError txt ->
        UnknownError txt

--
-- Instances for `WalletError`

-- deriveWalletErrorJSON ''WalletError
deriveGeneric ''WalletError

instance ToJSON WalletError where
    toJSON = gtoJsend ErrorStatus

instance FromJSON WalletError where
    parseJSON = gparseJsend

instance Exception WalletError where

-- TODO: generate `Arbitrary` instance with TH too?
instance Arbitrary WalletError where
    arbitrary = oneof (map pure sample)


--
-- Helpers
--

type ErrorName = Text
type ErrorCode = Int
type ErrorExample = Value


sampleSyncProgress :: SyncProgress
sampleSyncProgress = SyncProgress {
    spEstimatedCompletionTime = mkEstimatedCompletionTime 3000
  , spThroughput              = mkSyncThroughput (Core.BlockCount 400)
  , spPercentage              = mkSyncPercentage 80
}

sampleAddress :: V1 Core.Address
sampleAddress = V1 $ Core.Address
    { Core.addrRoot =
        Crypto.unsafeAbstractHash ("asdfasdf" :: String)
    , Core.addrAttributes =
        Core.mkAttributes $ Core.AddrAttributes Nothing Core.BootstrapEraDistr
    , Core.addrType =
        Core.ATPubKey
    }

-- | Sample of errors we use for documentation
sample :: [WalletError]
sample =
  [ NotEnoughMoney 1400
  , OutputIsRedeem sampleAddress
  , MigrationFailed "Migration failed"
  , JSONValidationFailed "Expected String, found Null."
  , UnknownError "Unknown error"
  , InvalidAddressFormat "Invalid base58 representation."
  , WalletNotFound
  , WalletAlreadyExists
  , AddressNotFound
  , MissingRequiredParams (("wallet_id", "walletId") :| [])
  , WalletIsNotReadyToProcessPayments sampleSyncProgress
  , NodeIsStillSyncing (mkSyncPercentage 42)
  ]


-- | Give a short description of an error
describe :: WalletError -> String
describe = \case
    NotEnoughMoney _ ->
         "Not enough available coins to proceed."
    OutputIsRedeem _ ->
         "One of the TX outputs is a redemption address."
    MigrationFailed _ ->
         "Error while migrating a legacy type into the current version."
    JSONValidationFailed _ ->
         "Couldn't decode a JSON input."
    UnknownError _ ->
         "Unexpected internal error."
    InvalidAddressFormat _ ->
         "Provided address format is not valid."
    WalletNotFound ->
         "Reference to an unexisting wallet was given."
    WalletAlreadyExists ->
         "Can't create or restore a wallet. The wallet already exists."
    AddressNotFound ->
         "Reference to an unexisting address was given."
    MissingRequiredParams _ ->
         "Missing required parameters in the request payload."
    WalletIsNotReadyToProcessPayments _ ->
         "This wallet is restoring, and it cannot send new transactions until restoration completes."
    NodeIsStillSyncing _ ->
         "The node is still syncing with the blockchain, and cannot process the request yet."
    TxRedemptionDepleted ->
        "The redemption address was already used."
    TxSafeSignerNotFound _ ->
        "The safe signer at the specified address was not found."
    TxFailedToStabilize ->
        "We were unable to find a set of inputs to satisfy this transaction."


-- | Convert wallet errors to Servant errors
toServantError :: WalletError -> ServantErr
toServantError err =
    mkServantErr $ case err of
        NotEnoughMoney{} ->
            err403
        OutputIsRedeem{} ->
            err403
        MigrationFailed{} ->
            err422
        JSONValidationFailed{} ->
            err400
        UnknownError{} ->
            err500
        WalletNotFound{} ->
            err404
        WalletAlreadyExists{} ->
            err403
        InvalidAddressFormat{} ->
            err401
        AddressNotFound{} ->
            err404
        MissingRequiredParams{} ->
            err400
        WalletIsNotReadyToProcessPayments{} ->
            err403
        NodeIsStillSyncing{} ->
            err412 -- Precondition failed
        TxFailedToStabilize{} ->
            err500
        TxRedemptionDepleted{} ->
            err400
        TxSafeSignerNotFound{} ->
            err400
  where
    mkServantErr serr@ServantErr{..} = serr
        { errBody    = encode err
        , errHeaders = applicationJson : errHeaders
        }

toHttpStatus :: WalletError -> HTTP.Status
toHttpStatus err = HTTP.Status (errHTTPCode $ toServantError err)
                               (encodeUtf8 $ describe err)

-- | Generates the @Content-Type: application/json@ 'HTTP.Header'.
applicationJson :: HTTP.Header
applicationJson =
    let [hdr] = getHeaders (addHeader "application/json" mempty :: (Headers '[Header "Content-Type" String] String))
    in hdr
