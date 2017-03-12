## Documentation of cardano-wallet web API

This is very first version, don't expect it to be smart.

## GET /addresses

#### Description

Returns all addresses contained in wallet.

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- No response body

## GET /api/addresses/:address/currencies/:currency

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
        "cpPwCreated": 1512259200,
        "cpPhoneNumber": "",
        "cpPicture": "",
        "cpLocale": "",
        "cpName": "",
        "cpEmail": "",
        "cpPwHash": ""
    }
}
```

## POST /api/profile

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "cpPwCreated": 1512259200,
    "cpPhoneNumber": "",
    "cpPicture": "",
    "cpLocale": "",
    "cpName": "",
    "cpEmail": "",
    "cpPwHash": ""
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
        "cpPwCreated": 1512259200,
        "cpPhoneNumber": "",
        "cpPicture": "",
        "cpLocale": "",
        "cpName": "",
        "cpEmail": "",
        "cpPwHash": ""
    }
}
```

## POST /api/redemptions/ada

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
        "cwAmount": {
            "getCoin": 0
        },
        "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
        "cwMeta": {
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet"
        }
    }
}
```

## GET /api/settings/slots/duration

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
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet"
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
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet"
            }
        },
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet"
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
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet"
            }
        },
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet"
            }
        },
        {
            "cwAmount": {
                "getCoin": 0
            },
            "cwAddress": "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
            "cwMeta": {
                "cwType": "CWTPersonal",
                "cwCurrency": "ADA",
                "cwName": "Personal Wallet"
            }
        }
    ]
}
```

## POST /api/wallets

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
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me"
        ]
    },
    "cwInitMeta": {
        "cwType": "CWTPersonal",
        "cwCurrency": "ADA",
        "cwName": "Personal Wallet"
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
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet"
        }
    }
}
```

## DELETE /api/wallets/:walletId

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
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet"
        }
    }
}
```

## PUT /api/wallets/:walletId

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
    "cwType": "CWTPersonal",
    "cwCurrency": "ADA",
    "cwName": "Personal Wallet"
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
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet"
        }
    }
}
```

## POST /api/wallets/keys

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
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet"
        }
    }
}
```

## POST /api/wallets/restore

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
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me",
            "me"
        ]
    },
    "cwInitMeta": {
        "cwType": "CWTPersonal",
        "cwCurrency": "ADA",
        "cwName": "Personal Wallet"
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
            "cwType": "CWTPersonal",
            "cwCurrency": "ADA",
            "cwName": "Personal Wallet"
        }
    }
}
```

## POST /send

#### Description

Send coins from one address from wallet to arbitrary address

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- No response body

