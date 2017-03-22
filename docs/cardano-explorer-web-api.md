## Documentation of cardano-explorer web API

This is very first version, don't expect it to be smart.

## GET /api/addresses/summary/:address

#### Authentication



Clients must supply the following data


#### Captures:

- *address*: Address

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{
    "Left": "This is an example error"
}
```

- Sample address summary

```javascript
{
    "Right": {
        "caTxList": [],
        "caBalance": {
            "getCoin": 0
        },
        "caAddress": "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv",
        "caTxNum": 0
    }
}
```

## GET /api/blocks/last

#### Authentication



Clients must supply the following data


#### GET Parameters:

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return

- offset
     - **Values**: *0, 100*
     - **Description**: Offset this many transactions


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{
    "Left": "This is an example error"
}
```

- 

```javascript
{
    "Right": []
}
```

- Sample block entry

```javascript
{
    "Right": [
        {
            "cbeTimeIssued": null,
            "cbeBlkHash": "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d",
            "cbeTotalSent": {
                "getCoin": 0
            },
            "cbeRelayedBy": null,
            "cbeHeight": 10,
            "cbeTxNum": 0,
            "cbeSize": 390
        }
    ]
}
```

- Sample block entry, Sample block entry

```javascript
{
    "Right": [
        {
            "cbeTimeIssued": null,
            "cbeBlkHash": "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d",
            "cbeTotalSent": {
                "getCoin": 0
            },
            "cbeRelayedBy": null,
            "cbeHeight": 10,
            "cbeTxNum": 0,
            "cbeSize": 390
        },
        {
            "cbeTimeIssued": null,
            "cbeBlkHash": "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d",
            "cbeTotalSent": {
                "getCoin": 0
            },
            "cbeRelayedBy": null,
            "cbeHeight": 10,
            "cbeTxNum": 0,
            "cbeSize": 390
        }
    ]
}
```

- Sample block entry, Sample block entry, Sample block entry

```javascript
{
    "Right": [
        {
            "cbeTimeIssued": null,
            "cbeBlkHash": "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d",
            "cbeTotalSent": {
                "getCoin": 0
            },
            "cbeRelayedBy": null,
            "cbeHeight": 10,
            "cbeTxNum": 0,
            "cbeSize": 390
        },
        {
            "cbeTimeIssued": null,
            "cbeBlkHash": "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d",
            "cbeTotalSent": {
                "getCoin": 0
            },
            "cbeRelayedBy": null,
            "cbeHeight": 10,
            "cbeTxNum": 0,
            "cbeSize": 390
        },
        {
            "cbeTimeIssued": null,
            "cbeBlkHash": "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d",
            "cbeTotalSent": {
                "getCoin": 0
            },
            "cbeRelayedBy": null,
            "cbeHeight": 10,
            "cbeTxNum": 0,
            "cbeSize": 390
        }
    ]
}
```

## GET /api/blocks/summary/:hash

#### Authentication



Clients must supply the following data


#### Captures:

- *hash*: Hash

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{
    "Left": "This is an example error"
}
```

- Sample block summary

```javascript
{
    "Right": {
        "cbsMerkleRoot": "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9",
        "cbsPrevHash": "d36710c918da4c4a3e0ff42e1049d81cc7bcbacc789c8583ea1c9afd8da3c24e",
        "cbsNextHash": "d3bb988e57356b706f7b8f1fe29591ab0d1bdfac4aa08836475783973e4cf7c1",
        "cbsEntry": {
            "cbeTimeIssued": null,
            "cbeBlkHash": "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d",
            "cbeTotalSent": {
                "getCoin": 0
            },
            "cbeRelayedBy": null,
            "cbeHeight": 10,
            "cbeTxNum": 0,
            "cbeSize": 390
        }
    }
}
```

## GET /api/blocks/txs/:hash

#### Authentication



Clients must supply the following data


#### Captures:

- *hash*: Hash

#### GET Parameters:

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return

- offset
     - **Values**: *0, 100*
     - **Description**: Offset this many transactions


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{
    "Left": "This is an example error"
}
```

- 

```javascript
{
    "Right": []
}
```

- Sample transaction entry

```javascript
{
    "Right": [
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        }
    ]
}
```

- Sample transaction entry, Sample transaction entry

```javascript
{
    "Right": [
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        },
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        }
    ]
}
```

- Sample transaction entry, Sample transaction entry, Sample transaction entry

```javascript
{
    "Right": [
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        },
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        },
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        }
    ]
}
```

## GET /api/search/:hash

#### Authentication



Clients must supply the following data


#### Captures:

- *hash*: Search id by which the user can find address, block or transaction

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{
    "Left": "This is an example error"
}
```

- Sample search result, address found

```javascript
{
    "Right": {
        "tag": "AddressFound",
        "contents": {
            "caTxList": [],
            "caBalance": {
                "getCoin": 0
            },
            "caAddress": "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv",
            "caTxNum": 0
        }
    }
}
```

## GET /api/txs/last

#### Authentication



Clients must supply the following data


#### GET Parameters:

- limit
     - **Values**: *0, 100*
     - **Description**: Max numbers of transactions to return

- offset
     - **Values**: *0, 100*
     - **Description**: Offset this many transactions


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{
    "Left": "This is an example error"
}
```

- 

```javascript
{
    "Right": []
}
```

- Sample transaction entry

```javascript
{
    "Right": [
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        }
    ]
}
```

- Sample transaction entry, Sample transaction entry

```javascript
{
    "Right": [
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        },
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        }
    ]
}
```

- Sample transaction entry, Sample transaction entry, Sample transaction entry

```javascript
{
    "Right": [
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        },
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        },
        {
            "cteId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
            "cteTimeIssued": 1512259200,
            "cteAmount": {
                "getCoin": 33333
            }
        }
    ]
}
```

## GET /api/txs/summary/:txid

#### Authentication



Clients must supply the following data


#### Captures:

- *txid*: Transaction id

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Sample error

```javascript
{
    "Left": "This is an example error"
}
```

- Sample transaction summary

```javascript
{
    "Right": {
        "ctsBlockTimeIssued": null,
        "ctsOutputs": [
            [
                "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ",
                {
                    "getCoin": 33333
                }
            ]
        ],
        "ctsTxTimeIssued": 1512259200,
        "ctsId": "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02",
        "ctsBlockHeight": 11,
        "ctsRelayedBy": null,
        "ctsTotalInput": {
            "getCoin": 33333
        },
        "ctsFees": {
            "getCoin": 0
        },
        "ctsInputs": [
            [
                "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv",
                {
                    "getCoin": 33333
                }
            ]
        ],
        "ctsTotalOutput": {
            "getCoin": 33333
        }
    }
}
```

