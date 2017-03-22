## Explorer Backend API

Currently, the explorer's API provides a series of methods to work with `cardano-sl`. The `servant` Haskell library that provides a modular approach to API-building was used. This library uses combinators to both build atomic HTTP actions and to glue these atomic methods together to form larger and more complete APIs.

If the event requests fail, there is a `ExplorerError` type, which is simply a wrapper over `Text` to show what happened.

Currently, the explorer's API supports the following operations (see Comments below):

| API | Endpoint | Parameter | Optional parameters | Description |
|-----|----------|-----------|---------------------|-------------|
| GET | /api/addresses/summary/:address | `address` - Address<br/>  |  | Get address summary.<br/>  | 
| GET | /api/blocks/last |  | `limit` - Max numbers of transactions to return<br/> `offset` - Offset this many transactions<br/>  | Get last block.<br/>  | 
| GET | /api/blocks/summary/:hash | `hash` - Hash<br/>  |  | Get block summary.<br/>  | 
| GET | /api/blocks/txs/:hash | `hash` - Hash<br/>  | `limit` - Max numbers of transactions to return<br/> `offset` - Offset this many transactions<br/>  | Get block transactions.<br/>  | 
| GET | /api/search/:hash | `hash` - Search id by which the user can find address, block or transaction<br/>  |  | Search for transaction, block or address.<br/>  | 
| GET | /api/txs/last |  | `limit` - Max numbers of transactions to return<br/> `offset` - Offset this many transactions<br/>  | Get last transaction.<br/>  | 
| GET | /api/txs/summary/:txid | `txid` - Transaction id<br/>  |  | Get transaction summary.<br/>  | 
