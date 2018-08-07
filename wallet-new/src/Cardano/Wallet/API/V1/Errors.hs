{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Wallet.API.V1.Errors where

import           Universum

import           Cardano.Wallet.API.V1.Types (SyncPercentage, SyncProgress (..),
                     V1 (..), mkEstimatedCompletionTime, mkSyncPercentage,
                     mkSyncThroughput)
import           Data.Aeson
import           Data.Aeson.Encoding (pairStr)
import           Data.Aeson.Types (Value (..), typeMismatch)
import qualified Data.HashMap.Strict as HMS
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Generics.SOP.TH (deriveGeneric)
import qualified Network.HTTP.Types as HTTP
import           Servant
import           Test.QuickCheck (Arbitrary (..), oneof)

import qualified Pos.Client.Txp.Util as TxError
import qualified Pos.Core as Core
import qualified Pos.Core.Attributes as Core
import qualified Pos.Crypto.Hashing as Crypto
import           Pos.Util.Util (aesonError)

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
    -- | NotEnoughMoney weNeedMore
      NotEnoughMoney !Int
    -- | OutputIsRedeem weAddress
    | OutputIsRedeem !(V1 Core.Address)
    -- | MigrationFailed weDescription
    | MigrationFailed !Text
    -- | JSONValidationFailed weValidationError
    | JSONValidationFailed !Text
    -- | UnknownError weMsg
    | UnknownError !Text
    -- | InvalidAddressFormat weMsg
    | InvalidAddressFormat !Text
    | WalletNotFound
    -- FIXME(akegalj): https://iohk.myjetbrains.com/youtrack/issue/CSL-2496
    | WalletAlreadyExists
    | AddressNotFound
    | TxFailedToStabilize
    | TxRedemptionDepleted
    -- | TxSafeSignerNotFound weAddress
    | TxSafeSignerNotFound !(V1 Core.Address)
    -- | MissingRequiredParams requiredParams
    | MissingRequiredParams !(NonEmpty (Text, Text))
    -- | WalletIsNotReadyToProcessPayments weStillRestoring
    | WalletIsNotReadyToProcessPayments !SyncProgress
    -- ^ The @Wallet@ where a @Payment@ is being originated is not fully
    -- synced (its 'WalletSyncState' indicates it's either syncing or
    -- restoring) and thus cannot accept new @Payment@ requests.
    -- | NodeIsStillSyncing wenssStillSyncing
    | NodeIsStillSyncing !SyncPercentage
    -- ^ The backend couldn't process the incoming request as the underlying
    -- node is still syncing with the blockchain.
    deriving (Generic, Show, Eq)

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

deriveGeneric ''WalletError

instance ToJSON WalletError where
    toEncoding (NotEnoughMoney weNeedMore) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic"
                 (pairs $ pairStr "needMore" (toEncoding weNeedMore))
             <> "message" .= String "NotEnoughMoney"
    toEncoding (OutputIsRedeem weAddress) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic"
                 (pairs $ pairStr "address" (toEncoding weAddress))
             <> "message" .= String "OutputIsRedeem"
    toEncoding (MigrationFailed weDescription) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic"
                 (pairs $ pairStr "description" (toEncoding weDescription))
             <> "message" .= String "MigrationFailed"
    toEncoding (JSONValidationFailed weValidationError) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic"
                 (pairs $ pairStr "validationError"
                     (toEncoding weValidationError))
             <> "message" .= String "JSONValidationFailed"
    toEncoding (UnknownError weMsg) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic" (pairs $ pairStr "msg" (toEncoding weMsg))
             <> "message" .= String "UnknownError"
    toEncoding (InvalidAddressFormat weMsg) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic" (pairs $ pairStr "msg" (toEncoding weMsg))
             <> "message" .= String "InvalidAddressFormat"
    toEncoding (WalletNotFound) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic" (pairs $ mempty)
             <> "message" .= String "WalletNotFound"
    toEncoding (WalletAlreadyExists) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic" (pairs $ mempty)
             <> "message" .= String "WalletAlreadyExists"
    toEncoding (AddressNotFound) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic" (pairs $ mempty)
             <> "message" .= String "AddressNotFound"
    toEncoding (TxFailedToStabilize) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic" (pairs $ mempty)
             <> "message" .= String "TxFailedToStabilize"
    toEncoding (TxRedemptionDepleted) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic" (pairs $ mempty)
             <> "message" .= String "TxRedemptionDepleted"
    toEncoding (TxSafeSignerNotFound weAddress) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic"
                 (pairs $ pairStr "address" (toEncoding weAddress))
             <> "message" .= String "TxSafeSignerNotFound"
    toEncoding (MissingRequiredParams requiredParams) =
        pairs $ pairStr "status" (toEncoding $ String "error")
             <> pairStr "diagnostic"
                 (pairs $ pairStr "params" (toEncoding requiredParams))
             <> "message" .= String "MissingRequiredParams"
    toEncoding (WalletIsNotReadyToProcessPayments weStillRestoring) =
        toEncoding $ toJSON weStillRestoring
    toEncoding (NodeIsStillSyncing wenssStillSyncing) =
        toEncoding $ toJSON wenssStillSyncing

instance FromJSON WalletError where
    parseJSON (Object o)
        | HMS.member "message" o =
              case HMS.lookup "message" o of
                Just "NotEnoughMoney"        ->
                    NotEnoughMoney
                        <$> ((o .: "diagnostic") >>= (.: "needMore"))
                Just "OutputIsRedeem"        ->
                    OutputIsRedeem <$> ((o .: "diagnostic") >>= (.: "address"))
                Just "MigrationFailed"       ->
                    MigrationFailed
                        <$> ((o .: "diagnostic") >>= (.: "description"))
                Just "JSONValidationFailed"  ->
                    JSONValidationFailed
                        <$> ((o .: "diagnostic") >>= (.: "validationError"))
                Just "UnknownError"          ->
                    UnknownError <$> ((o .: "diagnostic") >>= (.: "msg"))
                Just "InvalidAddressFormat"  ->
                    InvalidAddressFormat
                        <$> ((o .: "diagnostic") >>= (.: "msg"))
                Just "WalletNotFound"        -> pure WalletNotFound
                Just "WalletAlreadyExists"   -> pure WalletAlreadyExists
                Just "AddressNotFound"       -> pure AddressNotFound
                Just "TxFailedToStabilize"   -> pure TxFailedToStabilize
                Just "TxRedemptionDepleted"  -> pure TxRedemptionDepleted
                Just "TxSafeSignerNotFound"  ->
                    TxSafeSignerNotFound
                        <$> ((o .: "diagnostic") >>= (.: "address"))
                Just "MissingRequiredParams" ->
                    MissingRequiredParams
                        <$> ((o .: "diagnostic") >>= (.: "params"))
                Just _                       ->
                    fail "Incorrect JSON encoding for WalletError"
                Nothing                      ->
                    fail "Incorrect JSON encoding for WalletError"
        -- WalletIsNotReadyToProcessPayments
        | HMS.member "estimatedCompletionTime" o = do
            estCompTO <- (o .: "estimatedCompletionTime")
            sThroughPO <- (o .: "throughput")
            prctO <- (o .: "percentage")
            estCompT <- parseJSON estCompTO
            sThroughP <- parseJSON sThroughPO
            prct <- parseJSON prctO
            return . WalletIsNotReadyToProcessPayments
                $ SyncProgress estCompT sThroughP prct
        -- NodeIsStillSyncing
        | HMS.member "quantity" o = do
            quantityO <-  o .: "quantity"
            quantity <- parseJSON quantityO
            return . NodeIsStillSyncing $ mkSyncPercentage quantity
        | otherwise = aesonError "Incorrect JSON encoding for WalletError"
    parseJSON invalid = typeMismatch "WalletError" invalid

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
