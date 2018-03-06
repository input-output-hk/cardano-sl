{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.API.V1.Swagger where

import           Universum

import           Cardano.Wallet.API.Request.Filter
import           Cardano.Wallet.API.Request.Pagination
import           Cardano.Wallet.API.Request.Sort
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Errors as Errors
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Swagger.Example
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols (..))
import           Pos.Client.Txp.Util (InputSelectionPolicy)
import qualified Pos.Core as Core
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo, ctiGitRevision)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot)
import           Pos.Wallet.Web.Swagger.Instances.Schema ()

import qualified Data.Char as Char
import           Control.Lens ((?~), ix)
import           Data.Aeson (ToJSON (..), Value (Number, Object))
import           Data.Aeson.Encode.Pretty
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHM
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Conv
import           Data.Swagger hiding (Example, Header, example)
import qualified Data.Swagger as S
import           Data.Swagger.Declare
import qualified Data.Text as T
import           Data.Typeable
import           NeatInterpolation
import           Servant.API.Sub
import           Servant.Swagger
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

--
-- Helper functions
--

-- | Generates an example for type `a` with a static seed.
genExample :: (ToJSON a, Example a) => a
genExample = (unGen (resize 3 example)) (mkQCGen 42) 42

-- | Generates a `NamedSchema` exploiting the `ToJSON` instance in scope,
-- by calling `sketchSchema` under the hood.
fromExampleJSON :: (ToJSON a, Typeable a, Example a)
                  => proxy a
                  -> Declare (Definitions Schema) NamedSchema
fromExampleJSON (_ :: proxy a) = do
    let (randomSample :: a) = genExample
    return $ NamedSchema (Just $ fromString $ show $ typeOf randomSample) (sketchSchema randomSample)

-- | Renders the inner type of a proxy as a `Text`, using Typeable's `typeRep` internally.
renderType :: Typeable a => proxy a -> T.Text
renderType = fromString . show . typeRep

-- | Adds a randomly-generated but valid example to the spec, formatted as a JSON.
withExample :: (ToJSON a, Example a) => proxy a -> T.Text -> T.Text
withExample (_ :: proxy a) desc =
  desc <> " Here's an example:<br><br><pre>" <> toS (encodePretty $ toJSON @a genExample) <> "</pre>"

-- | Generates a description suitable to be used for "Update" types.
updateDescr :: Typeable a => proxy a -> T.Text
updateDescr (p :: proxy a) =
    "A type represending an update for an existing " <> renderType p <> "."

-- | Generates a description suitable to be used for "New" types.
newDescr :: Typeable a => proxy a -> T.Text
newDescr (p :: proxy a) =
    "A type represending an request for creating a(n) " <> renderType p <> "."

-- | Automatically derives the subset of readOnly fields by diffing the JSON representations of the
-- given types.
readOnlyFieldsFromJSON :: forall a b proxy. (Update a ~ b, Example a, ToJSON a, Example b, ToJSON b)
                       => proxy a -> Set T.Text
readOnlyFieldsFromJSON _ =
    case (toJSON (genExample @a), toJSON (genExample @b)) of
        (Object o1, Object o2) -> (Set.fromList $ HM.keys o1) `Set.difference` (Set.fromList $ HM.keys o2)
        _                      -> mempty

-- | Enrich a Swagger `Schema` with a list of readOnly fields.
setReadOnlyFields :: ToDocs a
                  => proxy a
                  -> (InsOrdHashMap Text (Referenced Schema))
                  -> (InsOrdHashMap Text (Referenced Schema))
setReadOnlyFields p hm =
  let fields = readOnlyFields p
  in InsOrdHM.mapWithKey (setRO fields) hm
  where
    setRO :: Set (T.Text) -> T.Text -> Referenced Schema -> Referenced Schema
    setRO _ _  r@(Ref _)    = r
    setRO flds f r@(Inline s) =
      if f `Set.member` flds then Inline (s & readOnly ?~ (f `Set.member` flds)) else r

--
-- Extra Typeclasses
--


-- TODO: Writing instances this way is a bit verbose. Is there a better way?
class (ToJSON a, Typeable a, Example a) => ToDocs a where
  annotate :: (proxy a -> Declare (Definitions Schema) NamedSchema)
           -> proxy a
           -> Declare (Definitions Schema) NamedSchema
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ descriptionFor p)
               . (schema . S.example ?~ toJSON @a genExample)
               . (over (schema . properties) (setReadOnlyFields p))

  descriptionFor :: proxy a -> T.Text
  descriptionFor p = "A " <> renderType p <> "."

  readOnlyFields :: proxy a -> Set T.Text
  readOnlyFields _ = mempty

--
-- Instances
--

instance HasSwagger (apiType a :> res) =>
         HasSwagger (WithDefaultApiArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))

instance HasSwagger (argA a :> argB a :> res) =>
         HasSwagger (AlternativeApiArg argA argB a :> res) where
    toSwagger _ = toSwagger (Proxy @(argA a :> argB a :> res))

instance (KnownSymbols tags, HasSwagger subApi) => HasSwagger (Tags tags :> subApi) where
    toSwagger _ =
        let newTags    = map toS (symbolVals (Proxy @tags))
            swgr       = toSwagger (Proxy @subApi)
        in swgr & over (operationsOf swgr . tags) (mappend (Set.fromList newTags))

instance (Typeable res, KnownSymbols syms, HasSwagger subApi) => HasSwagger (FilterBy syms res :> subApi) where
    toSwagger _ =
        let swgr       = toSwagger (Proxy @subApi)
            allOps     = map toText $ symbolVals (Proxy @syms)
        in swgr & over (operationsOf swgr . parameters) (addFilterOperations allOps)
          where
            addFilterOperations :: [Text] -> [Referenced Param] -> [Referenced Param]
            addFilterOperations ops xs = map (Inline . newParam) ops <> xs

            newParam :: Text -> Param
            newParam opName =
                let typeOfRes = fromString $ show $ typeRep (Proxy @ res)
                in Param {
                  _paramName = opName
                , _paramRequired = Nothing
                , _paramDescription = Just $ "A **FILTER** operation on a " <> typeOfRes <> "."
                , _paramSchema = ParamOther ParamOtherSchema {
                         _paramOtherSchemaIn = ParamQuery
                       , _paramOtherSchemaAllowEmptyValue = Nothing
                       , _paramOtherSchemaParamSchema = mempty
                       }
                }

instance (Typeable res, KnownSymbols syms, HasSwagger subApi) => HasSwagger (SortBy syms res :> subApi) where
    toSwagger _ =
        let swgr       = toSwagger (Proxy @subApi)
        in swgr & over (operationsOf swgr . parameters) addSortOperation
          where
            addSortOperation :: [Referenced Param] -> [Referenced Param]
            addSortOperation xs = (Inline newParam) : xs

            newParam :: Param
            newParam =
                let typeOfRes = fromString $ show $ typeRep (Proxy @ res)
                    allowedKeys = T.intercalate "," (map toText $ symbolVals (Proxy @syms))
                in Param {
                  _paramName = "sort_by"
                , _paramRequired = Just False
                , _paramDescription = Just (sortDescription typeOfRes allowedKeys)
                , _paramSchema = ParamOther ParamOtherSchema {
                         _paramOtherSchemaIn = ParamQuery
                       , _paramOtherSchemaAllowEmptyValue = Just True
                       , _paramOtherSchemaParamSchema = mempty
                       }
                }

sortDescription :: Text -> Text -> Text
sortDescription resource allowedKeys = [text|
A **SORT** operation on this $resource. Allowed keys: `$allowedKeys`.
|]

instance (HasSwagger subApi) => HasSwagger (WalletRequestParams :> subApi) where
    toSwagger _ =
        let swgr       = toSwagger (Proxy @(WithWalletRequestParams subApi))
        in swgr & over (operationsOf swgr . parameters) (map toDescription)
          where
            toDescription :: Referenced Param -> Referenced Param
            toDescription (Inline p@(_paramName -> pName)) =
                case M.lookup pName requestParameterToDescription of
                    Nothing -> Inline p
                    Just d  -> Inline (p & description .~ Just d)
            toDescription x = x

requestParameterToDescription :: Map T.Text T.Text
requestParameterToDescription = M.fromList [
    ("page", pageDescription)
  , ("per_page", perPageDescription (fromString $ show maxPerPageEntries) (fromString $ show defaultPerPageEntries))
  ]

pageDescription :: T.Text
pageDescription = [text|
The page number to fetch for this request. The minimum is **1**.
If nothing is specified, **this value defaults to 1** and always shows the first
entries in the requested collection.
|]

perPageDescription :: T.Text -> T.Text -> T.Text
perPageDescription maxValue defaultValue = [text|
The number of entries to display for each page. The minimum is **1**, whereas the maximum
is **$maxValue**. If nothing is specified, **this value defaults to $defaultValue**.
|]

instance ToParamSchema PerPage where
  toParamSchema _ = mempty
    & type_ .~ SwaggerInteger
    & default_ ?~ (Number $ fromIntegral defaultPerPageEntries)
    & minimum_ ?~ 1
    & maximum_ ?~ (fromIntegral maxPerPageEntries)

instance ToParamSchema Page where
  toParamSchema _ = mempty
    & type_ .~ SwaggerInteger
    & default_ ?~ (Number 1) -- Always show the first page by default.
    & minimum_ ?~ 1

instance ToParamSchema WalletId

instance ToParamSchema Core.Address where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString

instance ToParamSchema (V1 Core.Address) where
  toParamSchema _ = toParamSchema (Proxy @Core.Address)

instance ToDocs Metadata where
  descriptionFor _ = "Metadata returned as part of an <b>WalletResponse</b>."

instance ToDocs Account where
  readOnlyFields   = readOnlyFieldsFromJSON
  descriptionFor _ = "An Account."

instance ToDocs WalletAddress where
  readOnlyFields   = readOnlyFieldsFromJSON
  descriptionFor _ = "An Address with meta information related to it."

instance ToDocs AccountUpdate where
  descriptionFor _ = updateDescr (Proxy @Account)

instance ToDocs NewAccount where
  descriptionFor _ = newDescr (Proxy @Account)

instance ToDocs AddressValidity where
  descriptionFor _ = "Verifies that an address is base58 decodable."

instance ToDocs (V1 Core.Address) where
  descriptionFor _ = "A base58-encoded Address."

instance ToDocs Core.Address where
  descriptionFor _ = "A base58-encoded Address."

instance ToDocs WalletId where
  descriptionFor _ = "A Wallet ID."

instance ToDocs Wallet where
  readOnlyFields = readOnlyFieldsFromJSON

instance ToDocs NewWallet where
  descriptionFor _ = newDescr (Proxy @Wallet)

instance ToDocs NewAddress where
  descriptionFor _ = newDescr (Proxy @WalletAddress)

instance ToDocs WalletUpdate where
  descriptionFor _ = updateDescr (Proxy @Wallet)

instance ToDocs PasswordUpdate where
  descriptionFor _ = "A PasswordUpdate incapsulate a request for changing a Wallet's password."

instance ToDocs EstimatedFees where
  descriptionFor _ = "Estimated fees for a `Payment`."

instance ToDocs Payment where
  descriptionFor _ = "A transfer of `Coin`(s) from one source to one or more destinations."

instance ToDocs PaymentDistribution where
  descriptionFor _ = "Maps an `Address` to some `Coin`s and it's typically "
                  <> "used to specify where to send money during a `Payment`."

instance ToDocs Transaction where
  descriptionFor _ = "A Wallet Transaction."

instance ToDocs WalletSoftwareUpdate where
  descriptionFor _ = "A programmed update to the system."

instance ToDocs NodeSettings where
  descriptionFor _ = "A collection of static settings for this wallet node."

instance ToDocs BlockchainHeight where
  descriptionFor _ = "The height of the blockchain."

instance ToDocs SyncProgress where
  descriptionFor _ = "The sync progress with the blockchain."

instance ToDocs SlotDuration where
  descriptionFor _ = "The duration for a slot."

instance ToDocs LocalTimeDifference where
  descriptionFor _ = "The time difference between this node clock and the NTP server."

instance ToDocs NodeInfo where
  descriptionFor _ = "A collection of dynamic information for this wallet node."

instance ToDocs WalletStateSnapshot where
  descriptionFor _ = "Dump current wallet state"

instance ToDocs (V1 InputSelectionPolicy) where
  descriptionFor _ = "A policy to be passed to each new `Payment` request to "
                  <> "determine how a `Transaction` is assembled. "
                  <> "Possible values: [" <> possibleValuesOf @(V1 InputSelectionPolicy) Proxy <> "]."

possibleValuesOf :: (Show a, Enum a, Bounded a) => Proxy a -> T.Text
possibleValuesOf (Proxy :: Proxy a) = T.intercalate "," . map show $ ([minBound..maxBound] :: [a])

-- ToSchema instances

instance ToSchema Account where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema WalletAddress where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema AccountUpdate where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema NewAccount where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema AddressValidity where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema (V1 Core.Address) where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema Metadata where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema Wallet where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema NewWallet where
    declareNamedSchema =
        genericDeclareNamedSchema defaultSchemaOptions
            { fieldLabelModifier =
                over (ix 0) Char.toLower . drop 6 {- length "newwal" -}
            }

instance ToSchema WalletUpdate where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema PasswordUpdate where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema EstimatedFees where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema Transaction where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema Payment where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema WalletSoftwareUpdate where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema NodeSettings where
  declareNamedSchema = annotate fromExampleJSON

instance ToSchema NodeInfo where
  declareNamedSchema = annotate fromExampleJSON

instance ToDocs a => ToDocs (WalletResponse a) where
  annotate f p = (f p)

instance (ToJSON a, ToDocs a, Typeable a, Example a) => ToSchema (WalletResponse a) where
  declareNamedSchema = annotate fromExampleJSON

instance (ToDocs a) => ToDocs [a] where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ "A list of " <> renderType p <> ".")

--
-- The API
--

highLevelDescription :: DescriptionEnvironment -> T.Text
highLevelDescription DescriptionEnvironment{..} = [text|

This is the specification for the Cardano Wallet API, automatically generated
as a [Swagger](https://swagger.io/) spec from the [Servant](http://haskell-servant.readthedocs.io/en/stable/) API
of [Cardano](https://github.com/input-output-hk/cardano-sl).

Git revision: **$deGitRevision**
Software version: **$deSoftwareVersion**


## Request format (all versions)

Issuing requests against this API is conceptually not very different from any other web service out there. The API
is **versioned**, meaning that is possible to access different versions of the API by adding the _version number_
in the URL.

**For the sake of backward compatibility, we expose the legacy version of the API, available simply as
unversioned endpoints.**

This means that _omitting_ the version number would call the old version of the API. Examples:

```
/api/version
```

Compatibility between major versions is not _guaranteed_, i.e. the request & response formats might differ.

## Response format (V1 onwards)

**All GET requests of the API are paginated by default**. Whilst this can be a source of surprise, is
the best way of ensuring the performance of GET requests is not affected by the size of the data storage.

Version `V1` introduced a different way of requesting information to the API. In particular, GET requests
which returns a _collection_ (i.e. typically a JSON array of resources) lists extra parameters which can be
used to modify the shape of the response. In particular, those are:

* `page`: (Default value: **1**).
* `per_page`: (Default value: **$deDefaultPerPage**)

For a more accurate description, see the section `Parameters` of each GET request, but as a brief overview
the first two control how many results and which results to access in a paginated request.

This is an example of a typical (successful) response from the API:

``` json
$deWalletResponseExample
```

## Filtering and sorting

`GET` endpoints which list collection of resources supports filters & sort operations, which are clearly marked
in the swagger docs with the `FILTER` or `SORT` labels. The query format is quite simple, and it goes this way:

### Filter operators

| Operator | Description                                                               | Example                |
|----------|---------------------------------------------------------------------------|------------------------|
| -        | If **no operator** is passed, this is equivalent to `EQ` (see below).     | `balance=10`           |
| `EQ`     | Retrieves the resources with index _equal to the one provided.            | `balance=EQ[10]`       |
| `LT`     | Retrieves the resources with index _less than_ the one provided.          | `balance=LT[10]`       |
| `LTE`    | Retrieves the resources with index _less than equal_ the one provided.    | `balance=LTE[10]`      |
| `GT`     | Retrieves the resources with index _greater than_ the one provided.       | `balance=GT[10]`       |
| `GTE`    | Retrieves the resources with index _greater than equal_ the one provided. | `balance=GTE[10]`      |
| `RANGE`  | Retrieves the resources with index _within the inclusive range_ [k,k].    | `balance=RANGE[10,20]` |

### Sort operators

| Operator | Description                                                               | Example                |
|----------|---------------------------------------------------------------------------|------------------------|
| `ASC`    | Sorts the resources with the given index in _ascending_ order.            | `sort_by=ASC[balance]` |
| `DES`    | Sorts the resources with the given index in _descending_ order.           | `sort_by=DES[balance]` |
| -        | If **no operator** is passed, this is equivalent to `DES` (see above).    | `sort_by=balance`      |

## Dealing with errors (V1 onwards)

In case a request cannot be served by the API, a non-2xx HTTP response will be issue, together with a
[JSend-compliant](https://labs.omniti.com/labs/jsend) JSON Object describing the error in detail together
with a numeric error code which can be used by API consumers to implement proper error handling in their
application. For example, here's a typical error which might be issued:

``` json
$deErrorExample
```

### Existing wallet errors

$deWalletErrorTable

|]

type TableHeader = [T.Text]
type TableRow = [T.Text]

-- | Creates markdown table
-- TODO: test edge cases:
--  * TableHeader == []
--  * no TableRow
--  * when list of rows contains elements with different length (different number of columns)
markdownTable :: TableHeader -> [TableRow] -> T.Text
markdownTable h rows = unlines $ header:headerSplitter:(map makeRow rows)
  where
    header = makeRow h                             -- corresponds to "a|b|c"
    headerSplitter = makeRow $ map (const "---") h -- corresponds to "---|---|---"
    makeRow = T.intercalate "|"

data DescriptionEnvironment = DescriptionEnvironment {
    deErrorExample          :: !T.Text
  , deDefaultPerPage        :: !T.Text
  , deWalletResponseExample :: !T.Text
  , deWalletErrorTable      :: !T.Text
  , deGitRevision           :: !T.Text
  , deSoftwareVersion       :: !T.Text
  }

api :: ( HasCompileInfo
       , HasUpdateConfiguration
       , HasSwagger a)
    => Proxy a
    -> Swagger
api walletApi = toSwagger walletApi
  & info.title   .~ "Cardano Wallet API"
  & info.version .~ "2.0"
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ (highLevelDescription $ DescriptionEnvironment {
      deErrorExample = toS $ encodePretty Errors.WalletNotFound
    , deDefaultPerPage = fromString (show defaultPerPageEntries)
    , deWalletResponseExample = toS $ encodePretty (genExample @(WalletResponse [Account]))
    , deWalletErrorTable = markdownTable ["Error Name", "HTTP Error code", "Example"] $ map makeRow Errors.allErrorsList
    , deGitRevision = ctiGitRevision compileInfo
    , deSoftwareVersion = fromString $ show curSoftwareVersion
    })
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")
  where
    makeRow err = [surroundedBy "`" err, "-", "-"]
    surroundedBy wrap context = wrap <> context <> wrap
