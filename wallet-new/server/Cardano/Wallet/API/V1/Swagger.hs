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

import           Cardano.Wallet.API
import           Cardano.Wallet.API.Request.Pagination
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Errors as Errors
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types
import           Pos.Wallet.Web.Swagger.Instances.Schema ()

import           Control.Lens ((?~))
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
import           Data.Swagger hiding (Header)
import           Data.Swagger.Declare
import qualified Data.Text as T
import           Data.Typeable
import           GHC.TypeLits
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
genExample :: (ToJSON a, Arbitrary a) => a
genExample = (unGen (resize 3 arbitrary)) (mkQCGen 42) 42

-- | Generates a `NamedSchema` exploiting the `ToJSON` instance in scope,
-- by calling `sketchSchema` under the hood.
fromArbitraryJSON :: (ToJSON a, Typeable a, Arbitrary a)
                  => proxy a
                  -> Declare (Definitions Schema) NamedSchema
fromArbitraryJSON (_ :: proxy a) = do
    let (randomSample :: a) = genExample
    return $ NamedSchema (Just $ fromString $ show $ typeOf randomSample) (sketchSchema randomSample)

-- | Renders the inner type of a proxy as a `Text`, using Typeable's `typeRep` internally.
renderType :: Typeable a => proxy a -> T.Text
renderType = fromString . show . typeRep

-- | Adds a randomly-generated but valid example to the spec, formatted as a JSON.
withExample :: (ToJSON a, Arbitrary a) => proxy a -> T.Text -> T.Text
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
readOnlyFieldsFromJSON :: forall a b proxy. (Update a ~ b, Arbitrary a, ToJSON a, Arbitrary b, ToJSON b)
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
class (ToJSON a, Typeable a, Arbitrary a) => ToDocs a where
  annotate :: (proxy a -> Declare (Definitions Schema) NamedSchema)
           -> proxy a
           -> Declare (Definitions Schema) NamedSchema
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ descriptionFor p)
               . (schema . example ?~ toJSON @a genExample)
               . (over (schema . properties) (setReadOnlyFields p))

  descriptionFor :: proxy a -> T.Text
  descriptionFor p = "A " <> renderType p <> "."

  readOnlyFields :: proxy a -> Set T.Text
  readOnlyFields _ = mempty

-- | Shamelessly copied from:
-- <https://stackoverflow.com/questions/37364835/how-to-get-the-type-level-values-of-string-in-haskell>
-- The idea is to extend `KnownSymbol` to a type-level list, so that it's possibly to reify at the value-level
-- a `'[Symbol]` into a `[String]`.
class KnownSymbols (xs :: [Symbol]) where
  symbolVals :: proxy xs -> [String]

--
-- Instances
--

instance KnownSymbols ('[]) where
  symbolVals _ = []

instance (KnownSymbol a, KnownSymbols as) => KnownSymbols (a ': as) where
  symbolVals _ =
    symbolVal (Proxy :: Proxy a) : symbolVals (Proxy :: Proxy as)

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

instance (HasSwagger subApi) => HasSwagger (WalletRequestParams :> subApi) where
    toSwagger _ =
        let swgr       = toSwagger (Proxy @(WithWalletRequestParams subApi))
        in swgr & over (operationsOf swgr . parameters) (map toDescription)
          where
            toDescription :: Referenced Param -> Referenced Param
            toDescription (Inline p@(_paramName -> pName)) =
              Inline (p & description .~ M.lookup pName requestParameterToDescription)
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

instance ToDocs Address where
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

instance ToDocs TransactionGroupingPolicy where
  descriptionFor _ = "A policy to be passed to each new `Payment` request to "
                  <> "determine how a `Transaction` is assembled. "
                  <> "Possible values: [" <> possibleValuesOf @TransactionGroupingPolicy Proxy <> "]."

possibleValuesOf :: (Show a, Enum a, Bounded a) => Proxy a -> T.Text
possibleValuesOf (Proxy :: Proxy a) = T.intercalate "," . map show $ ([minBound..maxBound] :: [a])

-- ToSchema instances

instance ToSchema Account where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema WalletAddress where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema AccountUpdate where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema NewAccount where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema AddressValidity where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema Address where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema WalletId where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema Metadata where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema Wallet where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema NewWallet where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema NewAddress where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema WalletUpdate where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema PasswordUpdate where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema EstimatedFees where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema Transaction where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema Payment where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema WalletSoftwareUpdate where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema NodeSettings where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema NodeInfo where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToDocs a => ToDocs (WalletResponse a) where
  annotate f p = (f p)

instance (ToJSON a, ToDocs a, Typeable a, Arbitrary a) => ToSchema (WalletResponse a) where
  declareNamedSchema = annotate fromArbitraryJSON

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

### Request format (all versions)

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

### Response format (V1 onwards)

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

### Dealing with errors (V1 onwards)

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
  }

api :: Swagger
api = toSwagger walletAPI
  & info.title   .~ "Cardano Wallet API"
  & info.version .~ "2.0"
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ (highLevelDescription $ DescriptionEnvironment {
      deErrorExample = toS $ encodePretty Errors.WalletNotFound
    , deDefaultPerPage = fromString (show defaultPerPageEntries)
    , deWalletResponseExample = toS $ encodePretty (genExample @(WalletResponse [Account]))
    , deWalletErrorTable = markdownTable ["Error Name", "HTTP Error code", "Example"] $ map makeRow Errors.allErrorsList
    })
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")
  where
    makeRow err = [surroundedBy "_" err, "-", "-"]
    surroundedBy wrap context = wrap <> context <> wrap
