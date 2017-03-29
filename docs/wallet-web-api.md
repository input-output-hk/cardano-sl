## Wallet Backend API

Currently, the wallet's API provides a series of methods to work with wallets. The `servant` Haskell library that provides a modular approach to API-building was used. This library uses combinators to both build atomic HTTP actions and to glue these atomic methods together to form larger and more complete APIs.

If the event requests fail, there is a `WalletError` type, which is simply a wrapper over `Text` to show what happened.

Please note that:

* The code `Post '[JSON]` and `Get '[JSON]` indicates that the type of the contents in the message will be `[JSON]`. 
* `ReqBody '[JSON] t` extracts the request body `[JSON]` as a value of type `t`.

Currently, the wallet's API supports the following operations (see Comments below):

## GET /api/addresses

#### Description

Returns all addresses contained in wallet.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- No response body

## GET /api/addresses/:address/currencies/:currency

#### Description

Reply with `True` if the address is valid, and `False` otherwise. `[4]`

#### Authentication



Clients must supply the following data


#### Captures:

- *address*: Address
- *currency*: Currency

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": false
}
```

- 

```javascript
{
    "Right": true
}
```

## GET /api/profile

#### Description

Fetch the client’s current user profile - the datatype CProfile. [5]

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cpLocale": ""
    }
}
```

## POST /api/profile

#### Description

Update the user’s profile, returning the new one in the process.

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "cpLocale": ""
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cpLocale": ""
    }
}
```

## POST /api/redemptions/ada

#### Description

Redeem ADA from a token `[6]`, create and return a wallet with the redeemded ADA.

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "crWalletId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
    "crSeed": "1354644684681"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "ctType": {
            "tag": "CTOut",
            "contents": {
                "ctmDescription": "Transaction from A to B",
                "ctmDate": 1512259200,
                "ctmTitle": "Transaction",
                "ctmCurrency": "ADA"
            }
        },
        "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "ctConfirmations": 10,
        "ctAmount": {
            "getCoin": 0
        }
    }
}
```

## POST /api/reporting/initialized

#### Description

Initialize reporting.

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "cTotalTime": 123,
    "cPreInit": 456
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": []
}
```

## GET /api/settings/slots/duration

#### Description

Fetch the value of current slot duration.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": 101
}
```

## GET /api/settings/sync/progress

#### Description

Synchronization progress.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "_spPeers": 0,
        "_spNetworkCD": null,
        "_spLocalCD": {
            "getChainDifficulty": 0
        }
    }
}
```

## GET /api/settings/version

#### Description

Fetch the system’s version.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "svAppName": {
            "getApplicationName": "cardano"
        },
        "svNumber": 0
    }
}
```

## POST /api/test/reset

#### Description

The key reset when running `dev` mode.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": []
}
```

## GET /api/txs/histories/:address

#### Description

Fetch a tuple with the list of transactions where the address took part in the index interval [skip + 1, limit], and its length. `[2]`

#### Authentication



Clients must supply the following data


#### Captures:

- *address*: Address, history of which should be fetched

#### GET Parameters:

- skip
     - **Values**: *0, 100*
     - **Description**: Skip this many transactions

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": [
        [],
        101
    ]
}
```

- 

```javascript
{
    "Right": [
        [
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            }
        ],
        101
    ]
}
```

- 

```javascript
{
    "Right": [
        [
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            },
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            }
        ],
        101
    ]
}
```

- 

```javascript
{
    "Right": [
        [
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            },
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            },
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            }
        ],
        101
    ]
}
```

## GET /api/txs/histories/:address/:search

#### Description

Fetch a tuple with the list of transactions whose title has search as an infix, in the index interval [skip + 1, limit], and its length. `[2]`

#### Authentication



Clients must supply the following data


#### Captures:

- *address*: Address, history of which should be fetched
- *search*: Wallet title search pattern

#### GET Parameters:

- skip
     - **Values**: *0, 100*
     - **Description**: Skip this many transactions

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": [
        [],
        101
    ]
}
```

- 

```javascript
{
    "Right": [
        [
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            }
        ],
        101
    ]
}
```

- 

```javascript
{
    "Right": [
        [
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            },
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            }
        ],
        101
    ]
}
```

- 

```javascript
{
    "Right": [
        [
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            },
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            },
            {
                "ctType": {
                    "tag": "CTOut",
                    "contents": {
                        "ctmDescription": "Transaction from A to B",
                        "ctmDate": 1512259200,
                        "ctmTitle": "Transaction",
                        "ctmCurrency": "ADA"
                    }
                },
                "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                "ctConfirmations": 10,
                "ctAmount": {
                    "getCoin": 0
                }
            }
        ],
        101
    ]
}
```

## POST /api/txs/payments/:address/:transaction

#### Description

Add the transaction which has the given ID to the wallet’s transaction history, if such a transaction exists.

#### Authentication



Clients must supply the following data


#### Captures:

- *address*: Address, history of which should be fetched
- *transaction*: Transaction id

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "ctmDescription": "Transaction from A to B",
    "ctmDate": 1512259200,
    "ctmTitle": "Transaction",
    "ctmCurrency": "ADA"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": []
}
```

## POST /api/txs/payments/:from/:to/:amount

#### Description

Send coins in the default currency (presently, `ADA`) from an origin address to a destination address, without any transaction message or description. `[1]`

#### Authentication



Clients must supply the following data


#### Captures:

- *from*: Address from which coins should be sent.
- *to*: Destination address.
- *amount*: Amount of coins to send.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "ctType": {
            "tag": "CTOut",
            "contents": {
                "ctmDescription": "Transaction from A to B",
                "ctmDate": 1512259200,
                "ctmTitle": "Transaction",
                "ctmCurrency": "ADA"
            }
        },
        "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "ctConfirmations": 10,
        "ctAmount": {
            "getCoin": 0
        }
    }
}
```

## POST /api/txs/payments/:from/:to/:amount/:currency/:title/:description

#### Description

Send coins with currency (presently, `ADA`) from an origin address to a destination address, with title and description.

#### Authentication



Clients must supply the following data


#### Captures:

- *from*: Address from which coins should be sent.
- *to*: Destination address.
- *amount*: Amount of coins to send.
- *currency*: Currency
- *title*: Transaction title
- *description*: Transaction description

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "ctType": {
            "tag": "CTOut",
            "contents": {
                "ctmDescription": "Transaction from A to B",
                "ctmDate": 1512259200,
                "ctmTitle": "Transaction",
                "ctmCurrency": "ADA"
            }
        },
        "ctId": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "ctConfirmations": 10,
        "ctAmount": {
            "getCoin": 0
        }
    }
}
```

## GET /api/update

#### Description

Fetch information related to the next update.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cuiNegativeStake": {
            "getCoin": 3
        },
        "cuiVotesFor": 2,
        "cuiSoftwareVersion": {
            "svAppName": {
                "getApplicationName": "cardano"
            },
            "svNumber": 0
        },
        "cuiImplicit": false,
        "cuiVotesAgainst": 3,
        "cuiPositiveStake": {
            "getCoin": 10
        },
        "cuiScriptVersion": 15,
        "cuiBlockVesion": {
            "bvAlt": 3,
            "bvMajor": 25,
            "bvMinor": 12
        }
    }
}
```

## POST /api/update

#### Description

Apply the system’s most recent update.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": []
}
```

## GET /api/wallets

#### Description

Fetch all wallets to which the system has access to.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": []
}
```

- 

```javascript
{
    "Right": [
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwUnit": 0,
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet",
                "cwAssurance": "CWANormal"
            }
        }
    ]
}
```

- 

```javascript
{
    "Right": [
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwUnit": 0,
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet",
                "cwAssurance": "CWANormal"
            }
        },
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwUnit": 0,
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet",
                "cwAssurance": "CWANormal"
            }
        }
    ]
}
```

- 

```javascript
{
    "Right": [
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwUnit": 0,
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet",
                "cwAssurance": "CWANormal"
            }
        },
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwUnit": 0,
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet",
                "cwAssurance": "CWANormal"
            }
        },
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwUnit": 0,
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet",
                "cwAssurance": "CWANormal"
            }
        }
    ]
}
```

## POST /api/wallets

#### Description

Create a new wallet.

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "cwBackupPhrase": {
        "bpToList": [
            "transfer",
            "uniform",
            "grunt",
            "excess",
            "six",
            "veteran",
            "vintage",
            "warm",
            "confirm",
            "vote",
            "nephew",
            "allow"
        ]
    },
    "cwInitMeta": {
        "cwUnit": 0,
        "cwType": "CWTPersonal",
        "cwCurrency": "ADA",
        "cwName": "Personal Wallet",
        "cwAssurance": "CWANormal"
    }
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cwAmount": {
            "getCoin": 0
        },
        "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "cwMeta": {
            "cwUnit": 0,
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet",
            "cwAssurance": "CWANormal"
        }
    }
}
```

## DELETE /api/wallets/:walletId

#### Description

Delete the wallet associated to an address.

#### Authentication



Clients must supply the following data


#### Captures:

- *walletId*: WalletId, walletId = address, future versions should have HD wallets, and then it should have multiple addresses

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": []
}
```

## GET /api/wallets/:walletId

#### Description

Fetch the wallet related to a given address address, if it exists.

#### Authentication



Clients must supply the following data


#### Captures:

- *walletId*: WalletId, walletId = address, future versions should have HD wallets, and then it should have multiple addresses

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cwAmount": {
            "getCoin": 0
        },
        "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "cwMeta": {
            "cwUnit": 0,
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet",
            "cwAssurance": "CWANormal"
        }
    }
}
```

## PUT /api/wallets/:walletId

#### Description

Given an address and wallet meta-information, update the address’ wallet.

#### Authentication



Clients must supply the following data


#### Captures:

- *walletId*: WalletId, walletId = address, future versions should have HD wallets, and then it should have multiple addresses

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "cwUnit": 0,
    "cwType": "CWTPersonal",
    "cwCurrency": "ADA",
    "cwName": "Personal Wallet",
    "cwAssurance": "CWANormal"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cwAmount": {
            "getCoin": 0
        },
        "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "cwMeta": {
            "cwUnit": 0,
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet",
            "cwAssurance": "CWANormal"
        }
    }
}
```

## POST /api/wallets/keys

#### Description

Import wallet from a key.

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
"Sample CORS"
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cwAmount": {
            "getCoin": 0
        },
        "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "cwMeta": {
            "cwUnit": 0,
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet",
            "cwAssurance": "CWANormal"
        }
    }
}
```

## POST /api/wallets/restore

#### Description

Recover the wallet associated to the given backup information `[3]`, if it exists.

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "cwBackupPhrase": {
        "bpToList": [
            "transfer",
            "uniform",
            "grunt",
            "excess",
            "six",
            "veteran",
            "vintage",
            "warm",
            "confirm",
            "vote",
            "nephew",
            "allow"
        ]
    },
    "cwInitMeta": {
        "cwUnit": 0,
        "cwType": "CWTPersonal",
        "cwCurrency": "ADA",
        "cwName": "Personal Wallet",
        "cwAssurance": "CWANormal"
    }
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "Left": "Sample error"
}
```

- 

```javascript
{
    "Right": {
        "cwAmount": {
            "getCoin": 0
        },
        "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "cwMeta": {
            "cwUnit": 0,
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet",
            "cwAssurance": "CWANormal"
        }
    }
}
```

