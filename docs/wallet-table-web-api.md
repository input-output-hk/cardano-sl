## Wallet Backend API

Currently, the wallet's API provides a series of methods to work with wallets. The `servant` Haskell library that provides a modular approach to API-building was used. This library uses combinators to both build atomic HTTP actions and to glue these atomic methods together to form larger and more complete APIs.

If the event requests fail, there is a `WalletError` type, which is simply a wrapper over `Text` to show what happened.

Please note that:

* The code `Post '[JSON]` and `Get '[JSON]` indicates that the type of the contents in the message will be `[JSON]`. 
* `ReqBody '[JSON] t` extracts the request body `[JSON]` as a value of type `t`.

Currently, the wallet's API supports the following operations (see Comments below):

| API | Endpoint | Parameter | Optional parameters | Description |
|-----|----------|-----------|---------------------|-------------|
| GET | /api/addresses |  |  | Returns all addresses contained in wallet.<br/>  | 
| GET | /api/addresses/:address/currencies/:currency | `address` - Address<br/> `currency` - Currency<br/>  |  | Reply with `True` if the address is valid, and `False` otherwise. `[4]`<br/>  | 
| GET | /api/profile |  |  | Fetch the client’s current user profile - the datatype CProfile. [5]<br/>  | 
| POST | /api/profile |  |  | Update the user’s profile, returning the new one in the process.<br/>  | 
| POST | /api/redemptions/ada |  |  | Redeem ADA from a token `[6]`, create and return a wallet with the redeemded ADA.<br/>  | 
| POST | /api/reporting/initialized |  |  | Initialize reporting.<br/>  | 
| GET | /api/settings/slots/duration |  |  | Fetch the value of current slot duration.<br/>  | 
| GET | /api/settings/sync/progress |  |  | Synchronization progress.<br/>  | 
| GET | /api/settings/version |  |  | Fetch the system’s version.<br/>  | 
| POST | /api/test/reset |  |  | The key reset when running `dev` mode.<br/>  | 
| GET | /api/txs/histories/:address | `address` - Address, history of which should be fetched<br/>  | `skip` - Skip this many transactions<br/> `limit` - Max numbers of transactions to return<br/>  | Fetch a tuple with the list of transactions where the address took part in the index interval [skip + 1, limit], and its length. `[2]`<br/>  | 
| GET | /api/txs/histories/:address/:search | `address` - Address, history of which should be fetched<br/> `search` - Wallet title search pattern<br/>  | `skip` - Skip this many transactions<br/> `limit` - Max numbers of transactions to return<br/>  | Fetch a tuple with the list of transactions whose title has search as an infix, in the index interval [skip + 1, limit], and its length. `[2]`<br/>  | 
| POST | /api/txs/payments/:address/:transaction | `address` - Address, history of which should be fetched<br/> `transaction` - Transaction id<br/>  |  | Add the transaction which has the given ID to the wallet’s transaction history, if such a transaction exists.<br/>  | 
| POST | /api/txs/payments/:from/:to/:amount | `from` - Address from which coins should be sent.<br/> `to` - Destination address.<br/> `amount` - Amount of coins to send.<br/>  |  | Send coins in the default currency (presently, `ADA`) from an origin address to a destination address, without any transaction message or description. `[1]`<br/>  | 
| POST | /api/txs/payments/:from/:to/:amount/:currency/:title/:description | `from` - Address from which coins should be sent.<br/> `to` - Destination address.<br/> `amount` - Amount of coins to send.<br/> `currency` - Currency<br/> `title` - Transaction title<br/> `description` - Transaction description<br/>  |  | Send coins with currency (presently, `ADA`) from an origin address to a destination address, with title and description.<br/>  | 
| GET | /api/update |  |  | Fetch information related to the next update.<br/>  | 
| POST | /api/update |  |  | Apply the system’s most recent update.<br/>  | 
| GET | /api/wallets |  |  | Fetch all wallets to which the system has access to.<br/>  | 
| POST | /api/wallets |  |  | Create a new wallet.<br/>  | 
| DELETE | /api/wallets/:walletId | `walletId` - WalletId, walletId = address, future versions should have HD wallets, and then it should have multiple addresses<br/>  |  | Delete the wallet associated to an address.<br/>  | 
| GET | /api/wallets/:walletId | `walletId` - WalletId, walletId = address, future versions should have HD wallets, and then it should have multiple addresses<br/>  |  | Fetch the wallet related to a given address address, if it exists.<br/>  | 
| PUT | /api/wallets/:walletId | `walletId` - WalletId, walletId = address, future versions should have HD wallets, and then it should have multiple addresses<br/>  |  | Given an address and wallet meta-information, update the address’ wallet.<br/>  | 
| POST | /api/wallets/keys |  |  | Import wallet from a key.<br/>  | 
| POST | /api/wallets/restore |  |  | Recover the wallet associated to the given backup information `[3]`, if it exists.<br/>  | 
