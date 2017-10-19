{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.API.V1.Swagger where

import           Universum

import           Cardano.Wallet.API
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types

import           Control.Lens                     ((?~))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Map                         (Map)
import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as Set
import           Data.String.Conv
import           Data.Swagger                     hiding (Header)
import           Data.Swagger.Declare
import qualified Data.Text                        as T
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

-- | Adds a randomly-generated but valid example to the spec, formatted as a JSON.
withExample :: (ToJSON a, Arbitrary a) => proxy a -> T.Text -> T.Text
withExample (_ :: proxy a) desc =
  desc <> " Here's an example:<br><br><pre>" <> toS (encodePretty $ toJSON @a genExample) <> "</pre>"

--
-- Extra Typeclasses
--

-- TODO: Writing instances this way is a bit verbose. Is there a better way?
class (ToJSON a, Typeable a, Arbitrary a) => ToDocs a where
  annotate :: (proxy a -> Declare (Definitions Schema) NamedSchema)
           -> proxy a
           -> Declare (Definitions Schema) NamedSchema

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

instance ( KnownSymbol summary , HasSwagger subApi) => HasSwagger (Summary summary :> subApi) where
    toSwagger _ =
        let summaryTxt = toS (symbolVal (Proxy @summary))
            swgr       = toSwagger (Proxy @subApi)
        in swgr & (operationsOf swgr) . summary ?~ summaryTxt

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
  , ("extended", extendedDescription)
  , ("Daedalus-Response-Format", responseFormatDescription)
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

extendedDescription :: T.Text
extendedDescription = [text|
Informs the backend that the fetched data should be wrapped in an `ExtendedResponse`
(see the Models section). An `ExtendedResponse` includes useful metadata
which can be used by clients to support pagination.
|]

responseFormatDescription :: T.Text
responseFormatDescription = [text|
It has the same effect of setting `extended=true` in the URL as a query parameter.
If the header `Daedalus-Response-Format` is present in the HTTP request with a value set to
`extended`, the fetched data will be wrapped in an `ExtendedResponse`.
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

instance ToDocs APIVersion where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ (withExample p "The API version. We currently support v0 and v1."))
               . (schema . example ?~ toJSON @APIVersion genExample)

instance ToDocs WalletVersion where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ (withExample p "The Wallet version, including the API version and the Git revision."))
               . (schema . example ?~ toJSON @WalletVersion genExample)

instance ToSchema APIVersion where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema WalletVersion where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToDocs Account where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ "An Account")
               . (schema . example ?~ toJSON @Account genExample)

instance ToSchema Account where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToDocs Address where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ withExample p "An Address.")
               . (schema . example ?~ toJSON @Address genExample)

instance ToSchema Address where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema Metadata where
  declareNamedSchema = fromArbitraryJSON

instance ToDocs a => ToDocs (ExtendedResponse a) where
  annotate f p = (f p)

instance (ToJSON a, ToDocs a, Typeable a, Arbitrary a) => ToSchema (ExtendedResponse a) where
  declareNamedSchema = annotate fromArbitraryJSON

instance (ToDocs a) => ToDocs [a] where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ withExample p ("A list of " <> fromString (show $ typeOf (genExample @ a)) <> "."))

instance (ToDocs a, ToDocs b) => ToDocs (OneOf a b) where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ desc)
    where
      typeOfA, typeOfB :: T.Text
      typeOfA = fromString (show $ typeOf @b exampleOfB)
      typeOfB = fromString (show $ typeOf @a exampleOfA)
      exampleOfA = genExample
      exampleOfB = genExample

      desc :: T.Text
      desc = "OneOf <b>a</b> <b>b</b> is a type introduced to limit with Swagger 2.0's limitation of returning " <>
             "different types depending on the parameter of the request. While this has been fixed " <>
             "in OpenAPI 3, we effectively mimick its behaviour in 2.x. The idea is to return either " <>
             typeOfA <> " or " <> typeOfB <>
             " depending on whether or not the extended response format has been requested. Whilst using the " <>
             " api this type is erased away in the HTTP response, so that, in case the user requested the 'normal' " <>
             (withExample (Proxy @ a) " response format, an 'a' will be returned.") <>
             (withExample (Proxy @ b) " In case the user selected the extended format, a full 'ExtendedResponse' will be yielded.")

instance ( ToDocs a, ToDocs b) => ToSchema (OneOf a b) where
  declareNamedSchema = annotate fromArbitraryJSON

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
is versioned, meaning is possible to access different versions of the latter by changing the _version number_ in the URL.

For example, _omitting_ the version number of passing `v0` would call the version 0 of the API. Examples:

```
/api/version
/api/v0/version
```

Both URL above will return the same result. Compatibility between major versions is not _guaranteed_, i.e. the
request & response formats might differ.

### Response format (V1 onwards)

**All GET requests of the API are paginated by default**. Whilst this can be a source of surprise, is
the best way of ensuring the performance of GET requests is not affected by the size of the data storage.

Version `V1` introduced a different way of requesting information to the API. In particular, GET requests
which returns a _collection_ (i.e. typically a JSON array of resources) lists extra parameters which can be
used to modify the shape of the response. In particular, those are:

* `page`: (Default value: **1**).
* `per_page`: (Default value: **$defaultPerPage**)
* `extended`: (Default value: `false`)
* `Daedalus-Response-Format`: (Default value: `null`)

For a more accurate description, see the section `Parameters` of each GET request, but as a brief overview
the first two control how many results and which results to access in a paginated request. The other two
(one to be passed as a query parameter, the other as an HTTP Header) controls the response format. By omitting
both, the "naked" collection will be returned. For example, requesting for a list of _Accounts_ might issue,
in this case:

``` json
$accountExample
```

In the second case, instead:

``` json
$accountExtendedExample
```

### Dealing with errors (V1 onwards)

In case a request cannot be served by the API, a non-2xx HTTP response will be issue, together with a
[JSend-compliant](https://labs.omniti.com/labs/jsend) JSON Object describing the error in detail together
with a numeric error code which can be used by API consumers to implement proper error handling in their
application. For example, here's a typical error which might be issued:

``` json
$errorExample
```

|]

data DescriptionEnvironment = DescriptionEnvironment {
    errorExample           :: !T.Text
  , defaultPerPage         :: !T.Text
  , accountExample         :: !T.Text
  , accountExtendedExample :: !T.Text
  }

api :: Swagger
api = toSwagger walletAPI
  & info.title   .~ "Cardano Wallet API"
  & info.version .~ "2.0"
  & info.description ?~ (highLevelDescription $ DescriptionEnvironment {
      errorExample = toS $ encodePretty (genExample @WalletError)
    , defaultPerPage = fromString (show defaultPerPageEntries)
    , accountExample = toS $ encodePretty (genExample @[Account])
    , accountExtendedExample = toS $ encodePretty (genExample @(ExtendedResponse [Account]))
    })
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")
