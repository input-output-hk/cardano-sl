{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Data.Aeson
import           Generics.SOP.TH (deriveGeneric)
import           Servant
import           Test.QuickCheck (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen (oneof)

import           Cardano.Wallet.API.Response.JSend (ResponseStatus (ErrorStatus))
import           Cardano.Wallet.API.V1.Generic (gparseJsend, gtoJsend)
import           Cardano.Wallet.API.V1.Headers (applicationJson)
import           Cardano.Wallet.API.V1.Types (SyncPercentage, SyncProgress (..), V1 (..), WalletId,
                                              exampleWalletId, mkEstimatedCompletionTime,
                                              mkSyncPercentage, mkSyncThroughput)

import qualified Network.HTTP.Types as HTTP


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
data WalletError address syncProgress syncPercentage =
      NotEnoughMoney { weNeedMore :: !Int }
    | OutputIsRedeem { weAddress :: !address }
    | MigrationFailed { weDescription :: !Text }
    | JSONValidationFailed { weValidationError :: !Text }
    | UnknownError { weMsg :: !Text }
    | InvalidAddressFormat { weMsg :: !Text }
    | WalletNotFound
    -- FIXME(akegalj): https://iohk.myjetbrains.com/youtrack/issue/CSL-2496
    | WalletAlreadyExists { weWalletId :: WalletId }
    | AddressNotFound
    | TxFailedToStabilize
    | TxRedemptionDepleted
    | TxSafeSignerNotFound { weAddress :: address }
    | MissingRequiredParams { requiredParams :: NonEmpty (Text, Text) }
    | WalletIsNotReadyToProcessPayments { weStillRestoring :: syncProgress }
    -- ^ The @Wallet@ where a @Payment@ is being originated is not fully
    -- synced (its 'WalletSyncState' indicates it's either syncing or
    -- restoring) and thus cannot accept new @Payment@ requests.
    | NodeIsStillSyncing { wenssStillSyncing :: syncPercentage }
    -- ^ The backend couldn't process the incoming request as the underlying
    -- node is still syncing with the blockchain.
    deriving (Show, Eq)


--
-- Instances for `WalletError`

-- deriveWalletErrorJSON ''WalletError
deriveGeneric ''WalletError

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (WalletError a b c) where
    toJSON = gtoJsend ErrorStatus

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (WalletError a b c) where
    parseJSON = gparseJsend

instance (Typeable a, Show a, Typeable b, Show b, Typeable c, Show c) =>
    Exception (WalletError a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (WalletError a b c) where
    arbitrary = oneof
        [ NotEnoughMoney <$> arbitrary
        , OutputIsRedeem <$> arbitrary
        , pure (MigrationFailed "Migration failed.")
        , pure (JSONValidationFailed "Expected String, found Null.")
        , pure (UnknownError "Unknown error.")
        , pure (InvalidAddressFormat "Invalid Base58 representation.")
        , pure WalletNotFound
        , pure WalletAlreadyExists
        , pure AddressNotFound
        , pure TxFailedToStabilize
        , pure TxRedemptionDepleted
        , TxSafeSignerNotFound <$> arbitrary
        , pure (MissingRequiredParams (("wallet_id", "walletId") :| []))
        , WalletIsNotReadyToProcessPayments <$> arbitrary
        , NodeIsStillSyncing <$> arbitrary
        ]


--
-- Helpers
--

-- | Give a short description of an error
describe :: forall a b c. WalletError a b c -> String
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
    WalletAlreadyExists _ ->
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
toServantError
    :: forall a b c. (ToJSON a, ToJSON b, ToJSON c)
    => WalletError a b c
    -> ServantErr
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

-- |
toHttpStatus
    :: forall a b c. (ToJSON a, ToJSON b, ToJSON c)
    => WalletError a b c
    -> HTTP.Status
toHttpStatus err = HTTP.Status (errHTTPCode $ toServantError err)
                               (encodeUtf8 $ describe err)
