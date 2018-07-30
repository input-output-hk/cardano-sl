{-# LANGUAGE DataKinds            #-}
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

import           Cardano.Wallet.API.Indices (ParamNames)
import           Cardano.Wallet.API.Request.Filter
import           Cardano.Wallet.API.Request.Pagination
import           Cardano.Wallet.API.Request.Sort
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1.Errors as Errors
import           Cardano.Wallet.API.V1.Generic (gconsName)
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Swagger.Example
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols (..))
import qualified Pos.Core as Core
import           Pos.Core.Update (SoftwareVersion)
import           Pos.Util.CompileInfo (CompileTimeInfo, ctiGitRevision)
import           Pos.Util.Servant (LoggingApi)
import           Pos.Wallet.Web.Swagger.Instances.Schema ()

import           Control.Lens ((?~))
import           Data.Aeson (ToJSON (..), encode)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Swagger hiding (Example, Header, example)
import           Data.Swagger.Declare
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           NeatInterpolation
import           Servant (Handler, ServantErr (..), Server)
import           Servant.API.Sub
import           Servant.Swagger
import           Servant.Swagger.UI (SwaggerSchemaUI')
import           Servant.Swagger.UI.ReDoc (redocSchemaUIServer)
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

--
-- Helper functions
--

-- | Generates an example for type `a` with a static seed.
genExample :: Example a => a
genExample = (unGen (resize 3 example)) (mkQCGen 42) 42

-- | Generates a `NamedSchema` exploiting the `ToJSON` instance in scope,
-- by calling `sketchSchema` under the hood.
fromExampleJSON :: (ToJSON a, Typeable a, Example a)
                  => proxy a
                  -> Declare (Definitions Schema) NamedSchema
fromExampleJSON (_ :: proxy a) = do
    let (randomSample :: a) = genExample
    return $ NamedSchema (Just $ fromString $ show $ typeOf randomSample) (sketchSchema randomSample)

-- | Surround a Text with another
surroundedBy :: Text -> Text -> Text
surroundedBy wrap context = wrap <> context <> wrap

-- | Display a multi-line code-block inline (e.g. in tables)
inlineCodeBlock :: Text -> Text
inlineCodeBlock txt = "<pre>" <> replaceNewLines (replaceWhiteSpaces txt) <> "</pre>"
  where
    replaceNewLines    = T.replace "\n" "<br/>"
    replaceWhiteSpaces = T.replace " " "&nbsp;"


--
-- Instances
--

instance HasSwagger a => HasSwagger (LoggingApi config a) where
    toSwagger _ = toSwagger (Proxy @a)

instance HasSwagger (apiType a :> res) =>
         HasSwagger (WithDefaultApiArg apiType a :> res) where
    toSwagger _ = toSwagger (Proxy @(apiType a :> res))

instance HasSwagger (argA a :> argB a :> res) =>
         HasSwagger (AlternativeApiArg argA argB a :> res) where
    toSwagger _ = toSwagger (Proxy @(argA a :> argB a :> res))

instance (KnownSymbols tags, HasSwagger subApi) => HasSwagger (Tags tags :> subApi) where
    toSwagger _ =
        let newTags    = map toText (symbolVals (Proxy @tags))
            swgr       = toSwagger (Proxy @subApi)
        in swgr & over (operationsOf swgr . tags) (mappend (Set.fromList newTags))

instance
    ( Typeable res
    , KnownSymbols syms
    , HasSwagger subApi
    , syms ~ ParamNames res params
    ) => HasSwagger (FilterBy params res :> subApi) where
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
                , _paramDescription = Just $ filterDescription typeOfRes
                , _paramSchema = ParamOther ParamOtherSchema {
                         _paramOtherSchemaIn = ParamQuery
                       , _paramOtherSchemaAllowEmptyValue = Nothing
                       , _paramOtherSchemaParamSchema = mempty
                       }
                }

filterDescription :: Text -> Text
filterDescription typeOfRes = mconcat
    [ "A **FILTER** operation on a " <> typeOfRes <> ". "
    , "Filters support a variety of queries on the resource. "
    , "These are: \n\n"
    , "- `EQ[value]`    : only allow values equal to `value`\n"
    , "- `LT[value]`    : allow resource with attribute less than the `value`\n"
    , "- `GT[value]`    : allow objects with an attribute greater than the `value`\n"
    , "- `GTE[value]`   : allow objects with an attribute at least the `value`\n"
    , "- `LTE[value]`   : allow objects with an attribute at most the `value`\n"
    , "- `RANGE[lo,hi]` : allow objects with the attribute in the range between `lo` and `hi`\n"
    , "- `IN[a,b,c,d]`  : allow objects with the attribute belonging to one provided.\n\n"
    ]

instance
    ( Typeable res
    , KnownSymbols syms
    , syms ~ ParamNames res params
    , HasSwagger subApi
    ) => HasSwagger (SortBy params res :> subApi) where
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

instance ToParamSchema WalletId

instance ToSchema Core.Address where
    declareNamedSchema = pure . paramSchemaToNamedSchema defaultSchemaOptions

instance ToParamSchema Core.Address where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString

instance ToParamSchema (V1 Core.Address) where
  toParamSchema _ = toParamSchema (Proxy @Core.Address)


--
-- Descriptions
--

requestParameterToDescription :: Map T.Text T.Text
requestParameterToDescription = M.fromList [
    ("page", pageDescription)
  , ("per_page", perPageDescription (fromString $ show maxPerPageEntries) (fromString $ show defaultPerPageEntries))
  ]

pageDescription :: T.Text
pageDescription = [text|
The page number to fetch for this request. The minimum is **1**.  If nothing is specified, **this value defaults to 1** and always shows the first entries in the requested collection.
|]

perPageDescription :: T.Text -> T.Text -> T.Text
perPageDescription maxValue defaultValue = [text|
The number of entries to display for each page. The minimum is **1**, whereas the maximum is **$maxValue**. If nothing is specified, **this value defaults to $defaultValue**.
|]

sortDescription :: Text -> Text -> Text
sortDescription resource allowedKeys = [text|
A **SORT** operation on this $resource. Allowed keys: `$allowedKeys`.
|]

errorsDescription :: Text
errorsDescription = [text|
Error Name / Description | HTTP Error code | Example
-------------------------|-----------------|---------
$errors
|] where
  errors = T.intercalate "\n" rows
  rows = map (mkRow errToDescription) Errors.sample
  mkRow fmt err = T.intercalate "|" (fmt err)
  errToDescription err =
    [ surroundedBy "`" (gconsName err) <> "<br/>" <> toText (Errors.describe err)
    , show $ errHTTPCode $ Errors.toServantError err
    , inlineCodeBlock (T.decodeUtf8 $ BL.toStrict $ encodePretty err)
    ]


-- | Shorter version of the doc below, only for Dev & V0 documentations
highLevelShortDescription :: DescriptionEnvironment -> T.Text
highLevelShortDescription DescriptionEnvironment{..} = [text|
This is the specification for the Cardano Wallet API, automatically generated as a [Swagger](https://swagger.io/) spec from the [Servant](http://haskell-servant.readthedocs.io/en/stable/) API of [Cardano](https://github.com/input-output-hk/cardano-sl).

Software Version   | Git Revision
-------------------|-------------------
$deSoftwareVersion | $deGitRevision
|]


-- | Provide additional insights on V1 documentation
highLevelDescription :: DescriptionEnvironment -> T.Text
highLevelDescription DescriptionEnvironment{..} = [text|
This is the specification for the Cardano Wallet API, automatically generated as a [Swagger](https://swagger.io/) spec from the [Servant](http://haskell-servant.readthedocs.io/en/stable/) API of [Cardano](https://github.com/input-output-hk/cardano-sl).

Software Version   | Git Revision
-------------------|-------------------
$deSoftwareVersion | $deGitRevision

> **Warning**: This version is currently a **BETA-release** which is still under testing before
> its final stable release. Should you encounter any issues or have any remarks, please let us
> know; your feedback is highly appreciated.


Getting Started
===============

In the following examples, we will use *curl* to illustrate request to an API running on the default port **8090**.

Please note that wallet web API uses TLS for secure communication. Requests to the API need to
send a client CA certificate that was used when launching the node and identifies the client as
being permitted to invoke the server API.

Creating a New Wallet
---------------------

You can create your first wallet using the [`POST /api/v1/wallets`](#tag/Wallets%2Fpaths%2F~1api~1v1~1wallets%2Fpost) endpoint as follow:

```
curl -X POST https://localhost:8090/api/v1/wallets \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  -d '{
  "operation": "create",
  "backupPhrase": [$deMnemonicExample],
  "assuranceLevel": "normal",
  "name": "MyFirstWallet",
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```

> **Warning**: Those 12 mnemonic words given for the backup phrase act as an example. **Do
> not** use them on a production system. See the section below about mnemonic codes for more
> information.

The `spendingPassword` is optional but highly recommended. It a string of 32
characters, encoded in base 16, yielding to an hexadecimal sequence of 64 bytes.
This passphrase is required for sensitive operations on the wallet and adds
an extra security layer to it.

To generate a valid `spendingPassword`, please follow the following steps:

- Pick a long sentence using a wide variety of characters (uppercase, lowercase,
  whitespace, punctuation, etc). Using a computer to randomly generate
  a passphrase is best, as humans aren't a good source of randomness.

- Compute an appropriate hash of this passphrase. You'll need to use an
  algorithm that yields a 32-byte long string (e.g. *SHA256* or *BLAKE2b*).

- Hex-encode the 32-byte hash into a 64-byte sequence of bytes.

As a response, the API provides you with a unique wallet `id` to be used in subsequent
requests. Make sure to store it / write it down. Note that every API response is
[jsend-compliant](https://labs.omniti.com/labs/jsend); Cardano also augments responses with
meta-data specific to pagination. More details in the section below about [Pagination](#section/Pagination)

```json
$createWallet
```

You have just created your first wallet. Information about this wallet can be retrieved using the [`GET /api/v1/wallets/{walletId}`](#tag/Wallets%2Fpaths%2F~1api~1v1~1wallets~1{walletId}%2Fget)
endpoint as follows:

```
curl -X GET https://localhost:8090/api/v1/wallets/{{walletId}} \
     -H "Accept: application/json; charset=utf-8" \
     --cacert ./scripts/tls-files/ca.crt
```

Receiving ADA
-------------

To receive _ADA_ from other users you should provide your address. This address can be obtained
from an account. Each wallet contains at least one account. An account is like a pocket inside
of your wallet. Vew all existing accounts of a wallet by using the [`GET /api/v1/wallets/{{walletId}}/accounts`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts%2Fget)
endpoint:

```
curl -X GET https://localhost:8090/api/v1/wallets/{{walletId}}/accounts?page=1&per_page=10 \
  -H "Accept: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt
```

Since you have, for now, only a single wallet, you'll see something like this:

```json
$readAccounts
```

All the wallet's accounts are listed under the `addresses` field. You can communicate one of
these addresses to receive _ADA_ on the associated account.


Sending ADA
-----------

In order to send _ADA_ from one of your accounts to another address, you must create a new
payment transaction using the [`POST /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fpost)
endpoint as follows:

```
curl -X POST https://localhost:8090/api/v1/transactions \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  -d '{
  "destinations": [{
    "amount": 14,
    "address": "A7k5bz1QR2...Tx561NNmfF"
  }],
  "source": {
    "accountIndex": 0,
    "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"
  },
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```

Note that, in order to perform a transaction, you need to have enough existing _ADA_ on the
source account! The Cardano API is designed to accomodate multiple recipients payments
out-of-the-box; notice how `destinations` is a list of addresses (and corresponding amounts).

When the transaction succeeds, funds are no longer available in the sources addresses, and are
soon made available to the destinations within a short delay. Note that, you can at any time see
the status of your wallets by using the [`GET /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fget)
endpoint as follows:

```
curl -X GET https://localhost:8090/api/v1/transactions?wallet_id=Ae2tdPwUPE...8V3AVTnqGZ\
     -H "Accept: application/json; charset=utf-8" \
     --cacert ./scripts/tls-files/ca.crt \
```

Here we constrained the request to a specific account. After our previous transaction the output
should look roughly similar to this:

```json
$readTransactions
```

In addition, and because it is not possible to _preview_ a transaction, one can lookup a
transaction's fees using the [`POST /api/v1/transactions/fees`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions~1fees%2Fpost)
endpoint to get an estimation of those fees.
See [Estimating Transaction Fees](#section/Common-Use-Cases/Estimating-Transaction-Fees) for more details.


Pagination
==========

**All GET requests of the API are paginated by default**. Whilst this can be a source of
surprise, is the best way of ensuring the performance of GET requests is not affected by the
size of the data storage.

Version `V1` introduced a different way of requesting information to the API. In particular,
GET requests which returns a _collection_ (i.e. typically a JSON array of resources) lists
extra parameters which can be used to modify the shape of the response. In particular, those
are:

* `page`: (Default value: **1**).
* `per_page`: (Default value: **$deDefaultPerPage**)

For a more accurate description, see the section `Parameters` of each GET request, but as a
brief overview the first two control how many results and which results to access in a
paginated request.


Filtering and Sorting
=====================

`GET` endpoints which list collection of resources supports filters & sort operations, which
are clearly marked in the swagger docs with the `FILTER` or `SORT` labels. The query format is
quite simple, and it goes this way:


Filter Operators
----------------

| Operator | Description                                                               | Example                |
|----------|---------------------------------------------------------------------------|------------------------|
| -        | If **no operator** is passed, this is equivalent to `EQ` (see below).     | `balance=10`           |
| `EQ`     | Retrieves the resources with index _equal_ to the one provided.           | `balance=EQ[10]`       |
| `LT`     | Retrieves the resources with index _less than_ the one provided.          | `balance=LT[10]`       |
| `LTE`    | Retrieves the resources with index _less than equal_ the one provided.    | `balance=LTE[10]`      |
| `GT`     | Retrieves the resources with index _greater than_ the one provided.       | `balance=GT[10]`       |
| `GTE`    | Retrieves the resources with index _greater than equal_ the one provided. | `balance=GTE[10]`      |
| `RANGE`  | Retrieves the resources with index _within the inclusive range_ [k,k].    | `balance=RANGE[10,20]` |

Sort Operators
--------------

| Operator | Description                                                               | Example                |
|----------|---------------------------------------------------------------------------|------------------------|
| `ASC`    | Sorts the resources with the given index in _ascending_ order.            | `sort_by=ASC[balance]` |
| `DES`    | Sorts the resources with the given index in _descending_ order.           | `sort_by=DES[balance]` |
| -        | If **no operator** is passed, this is equivalent to `DES` (see above).    | `sort_by=balance`      |


Errors
======

In case a request cannot be served by the API, a non-2xx HTTP response will be issued, together
with a [JSend-compliant](https://labs.omniti.com/labs/jsend) JSON Object describing the error
in detail together with a numeric error code which can be used by API consumers to implement
proper error handling in their application. For example, here's a typical error which might be
issued:

``` json
$deErrorExample
```

Existing Wallet Errors
----------------------

$deWalletErrorTable


Monetary Denomination & Units
=============================

Cardano's currency is called _ADA_ ( ₳ ). _ADA_ has up to **6** decimal places; hence the
smallest monetary unit that can be represented in the Cardano's blockhain is: 0.000001₳. This
is also called a _Lovelace_ (Cardano's currency is named after the mathematician and computer
scientist [Ada Lovelace](https://en.wikipedia.org/wiki/Ada_Lovelace)). Put in another way, one
_ADA_ is equal to one million _Lovelace_.

ADA        | Lovelace
-----------|----------
`1`        | `1 000 000`
`.000 001` | `1`

> **Warning**: All amounts manipulated in the API are given and expected in Lovelace.


Mnemonic Codes
==============

The full list of accepted mnemonic codes to secure a wallet is defined by the [BIP-39
specifications](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki). Note that
picking up 12 random words from the list **is not enough** and leads to poor security. Make
sure to carefully follow the steps described in the protocol when you generate words for a new
wallet.


Versioning & Legacy
===================

The API is **versioned**, meaning that is possible to access different versions of the API by adding the _version number_ in the URL.

**For the sake of backward compatibility, we expose the legacy version of the API, available simply as unversioned endpoints.**

This means that _omitting_ the version number would call the old version of the API. Deprecated
endpoints are currently grouped under an appropriate section; they would be removed in upcoming
released, if you're starting a new integration with Cardano-SL, please ignore these.

Note that Compatibility between major versions is not _guaranteed_, i.e. the request & response formats might differ.


Disable TLS (Not Recommended)
-----------------------------

If needed, you can disable TLS by providing the `--no-tls` flag to the wallet or by running a wallet in debug mode with `--wallet-debug` turned on.


Common Use-Cases
================

Sending Money to Multiple Recipients
------------------------------------

As seen in [Sending ADA](#section/Getting-Started/Sending-ADA), you can send _ADA_ to
another party using the [`POST /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fpost) endpoint.
Important to notice is the type of the field `destinations`: it's a list, enabling you to provide more
than one destination. Each destination is composed of:

- An address
- A corresponding amount

The overall transaction corresponds to the sum of each outputs. For instance, to send money to
two parties simultaneously:

```
curl -X POST https://localhost:8090/api/v1/transactions \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  -d '{
  "destinations": [
    {
      "amount": 14,
      "address": "A7k5bz1QR2...Tx561NNmfF"
    },
    {
      "amount": 42,
      "address": "B56n78WKE8...jXAa34NUFz"
    }
  ],
  "source": {
    "accountIndex": 0,
    "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"
  },
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```


Estimating Transaction Fees
---------------------------

When you submit a transaction to the network, some fees apply depending on, but not only, the
selected grouping policy and the available inputs on the source wallet. There's actually a
trade-off between fees, cryptographic security, throughput and privacy. The more inputs are
selected, the bigger is the payload, the bigger are the fees.

The API lets you estimate fees for a given transaction via the [`POST /api/v1/transaction/fees`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions~1fees%2Fpost)
endpoint. The request payload is identical to the one you would make to create a transaction:

```
curl -X POST https://localhost:8090/api/v1/transactions/fees \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  -d '{
  "destinations": [{
      "amount": 14,
      "address": "A7k5bz1QR2...Tx561NNmfF"
  }],
  "source": {
    "accountIndex": 0,
    "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"
  }
}'
```

The API resolves with an estimated amount in _ADA_. This estimation highly depends on the
current state of the ledger and diverges with time.

```json
$readFees
```


Managing Accounts
-----------------

A wallet isn't limited to one account. It can actually be useful to have more than one account
in order to separate business activities. With the API, you can retrieve a specific account,
create new ones, list all existing accounts of a wallet or edit a few things on an existing
account. By default, your wallet comes with a provided account. Let's see how to create a fresh
new account on a wallet using [`POST /api/v1/wallets/{{walletId}}/accounts`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts%2Fpost):

```
curl -X POST \
  https://localhost:8090/api/v1/Ae2tdPwUPE...8V3AVTnqGZ/accounts \
  -H 'Content-Type: application/json;charset=utf-8' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  -d '{
  "name": "MyOtherAccount",
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```

Note that the `spendingPassword` here should match the one provided earlier in [Creating a
New Wallet](#section/Getting-Started/Creating-a-New-Wallet).


```json
$createAccount
```

You can always retrieve this account description later if needed via [`GET /api/v1/wallets/{{walletId}}/accounts/{{accountId}}`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts~1{accountId}%2Fget).

For example:

```
curl -X GET \
  https://127.0.0.1:8090/api/v1/wallets/Ae2tdPwUPE...8V3AVTnqGZ/accounts/2902829384 \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt
```

For a broader view, the full list of accounts of a given wallet can be retrieved using [`GET /api/v1/wallets/{{walletId}}/accounts`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts%2Fget)
```
curl -X GET \
  https://127.0.0.1:8090/api/v1/wallets/Ae2tdPwUPE...8V3AVTnqGZ/accounts \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt
```

```json
$readAccounts
```


Managing Addresses
------------------

By default, wallets you create are provided with an account which has one default address. It
is possible (and recommended) for an account to manage multiple addresses. Address reuse
actually reduces privacy for it tights more transactions to a small set of addresses.

When paying, the wallet makes many of these choices for you. Addresses are
selected from a wallet's account based on several different strategies and
policies.

To create a new address, use the [`POST /api/v1/addresses`](#tag/Addresses%2Fpaths%2F~1api~1v1~1addresses%2Fpost)
endpoint:

```
curl -X POST \
  https://localhost:8090/api/v1/addresses \
  -H 'Content-Type: application/json;charset=utf-8' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  -d '{
	"walletId": "Ae2tdPwUPE...V3AVTnqGZ4",
	"accountIndex": 2147483648
}'
```

```json
$createAddress
```

If your wallet is protected with a password, this password is also required in order to create
new addresses for that wallet. In such case, the field `spendingPassword` should match the one
defined earlier to protect your wallet.

Addresses generated as just described are always valid. When the API encounters
an invalid address however (e.g. when provided by another party), it will fail with a
client error.

You can always view all your available addresses across all your wallets by using
[`GET /api/v1/addresses`](#tag/Addresses%2Fpaths%2F~1api~1v1~1addresses%2Fget):

```
curl -X GET https://localhost:8090/api/v1/addresses \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt
```

```json
$readAddresses
```

Checking Synchronization Progress
---------------------------------

You can control the synchronization progress of the underlying node hosting the wallet's server
via [`GET /api/v1/node-info`](#tag/Info%2Fpaths%2F~1api~1v1~1node-info%2Fget). The output is
rather verbose and gives real-time progress updates about the current node.

```
curl -X GET https://localhost:8090/api/v1/node-info \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt
```

```json
$readNodeInfo
```


Retrieving Transaction History
------------------------------

If needed, applications may regularly poll the wallet's backend to retrieve the history of
transactions of a given wallet. Using the [`GET /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fget)
endpoint, you can view the status of all transactions that ever sent or took money from the
wallet.

The following table sums up the available filters (also detailed in the endpoint documentation details):

Filter On                   | Corresponding Query Parameter(s)
----------------------------| ------------------------------
Wallet                      | `wallet_id`
Wallet's account            | `account_index` + `wallet_id`
Address                     | `address`
Transaction's creation time | `created_at`
Transaction's id            | `id`

For example, in order to retrieve the last 50 transactions of a particular account,
ordered by descending date:

```
curl -X GET https://127.0.0.1:8090/api/v1/transactions?wallet_id=Ae2tdPwU...3AVTnqGZ&account_index=2902829384&sort_by=DES\[created_at\]&per_page=50' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt
```
For example, in order to retrieve the last 50 transactions, ordered by descending date:

```
curl -X GET 'https://127.0.0.1:8090/api/v1/transactions?wallet_id=Ae2tdPwU...3AVTnqGZ &sort_by=DES\[created_at\]&per_page=50' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt
```


Another example, if you were to look for all transactions made since the 1st of January 2018:

```
curl -X GET 'https://127.0.0.1:8090/api/v1/transactions?wallet_id=Ae2tdPwU...3AVTnqGZ&created_at=GT\[2018-01-01T00:00:00.00000\]' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt
```

Make sure to carefully read the section about [Pagination](#section/Pagination) to fully
leverage the API capabilities.
|]
  where
    createAccount    = decodeUtf8 $ encodePretty $ genExample @(WalletResponse Account)
    createAddress    = decodeUtf8 $ encodePretty $ genExample @(WalletResponse WalletAddress)
    createWallet     = decodeUtf8 $ encodePretty $ genExample @(WalletResponse Wallet)
    readAccounts     = decodeUtf8 $ encodePretty $ genExample @(WalletResponse [Account])
    readAddresses    = decodeUtf8 $ encodePretty $ genExample @(WalletResponse [Address])
    readFees         = decodeUtf8 $ encodePretty $ genExample @(WalletResponse EstimatedFees)
    readNodeInfo     = decodeUtf8 $ encodePretty $ genExample @(WalletResponse NodeInfo)
    readTransactions = decodeUtf8 $ encodePretty $ genExample @(WalletResponse [Transaction])


-- | Provide an alternative UI (ReDoc) for rendering Swagger documentation.
swaggerSchemaUIServer
    :: (Server api ~ Handler Swagger)
    => Swagger -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer = redocSchemaUIServer

--
-- The API
--

data DescriptionEnvironment = DescriptionEnvironment
  { deErrorExample     :: !T.Text
  , deDefaultPerPage   :: !T.Text
  , deWalletErrorTable :: !T.Text
  , deGitRevision      :: !T.Text
  , deSoftwareVersion  :: !T.Text
  , deMnemonicExample  :: !T.Text
  }

api :: HasSwagger a
    => (CompileTimeInfo, SoftwareVersion)
    -> Proxy a
    -> (DescriptionEnvironment -> T.Text)
    -> Swagger
api (compileInfo, curSoftwareVersion) walletAPI mkDescription = toSwagger walletAPI
  & info.title   .~ "Cardano Wallet API"
  & info.version .~ fromString (show curSoftwareVersion)
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ (mkDescription $ DescriptionEnvironment
    { deErrorExample          = decodeUtf8 $ encodePretty Errors.WalletNotFound
    , deMnemonicExample       = decodeUtf8 $ encode (genExample @BackupPhrase)
    , deDefaultPerPage        = fromString (show defaultPerPageEntries)
    , deWalletErrorTable      = errorsDescription
    , deGitRevision           = ctiGitRevision compileInfo
    , deSoftwareVersion       = fromString $ show curSoftwareVersion
    })
  & info.license ?~ ("MIT" & url ?~ URL "https://raw.githubusercontent.com/input-output-hk/cardano-sl/develop/lib/LICENSE")
