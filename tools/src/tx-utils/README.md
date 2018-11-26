# Cardano Tx Utils

> Yet another set of extra (lightweight) tools for cardano-sl.


```
Usage:
  cardano-sl-tx-utils decode [options] <bytes>
  cardano-sl-tx-utils verify signatures [options] <bytes>

Options:
  -h, --help            Show this help
  --requires-magic      Requires network magic (e.g. testnet)
  --protocol-magic=INT  Protocol magic id [default: 764824073]
```


# Examples

Example of hex-encoded transaction with witnesses:

<pre>82839f8200d818582482582007ae8806188c46519f9aaf378b52ac0ea158cfcac023a18f2739bfa08c72329f018200d8185824825820c4fc6963205ca84670bc8ee2b2b9f8c43346585b1246d3de7e1512dfe2c05eec008200d8185824825820223ce2872de6e3b890f343757b776e541be3bbf75cb5c543fd0ab8f2c9d5a15600ff9f8282d818582183581cb26db170824bb3d6056ac031ce99242e7b82f9d8e180184eddccbd5ba0001a2a19c5981b000000027ef0dc4bffa0838200d8185885825840b0b914c48c7f6b618543e6cb8516f93c850c363f01fb02ef3f66984be7f9d54f758750ce308e2babfa4ca7e73286c0367f0574f8a8d5b26ef6d7e7acc24cea9b5840d2e1691d2d6323f8d271406a63b4daa0eb0955666a97954e85864cca210d1f0623217f5b49f173c37ff4b56241e05556c3ad5e3604637f37a0b76cf91e851a038200d81858858258405d0874c9e101f844f3d7a5cd7c763d55cbc4d24b775a78f6caeca22123ddb2c3e63e3cd3c75c055fbbe7f76cd30bde082eadd18ef71418049767a3c147ba53cb58404d75ee119f872e17f3dcc68c48532d028cf226e2a401fc1208c196dee32c6d796cedc34cd79fa7b8db242957f0a5488578defb40af17ecb4f03c09d63fdab40d8200d81858858258405061236159d578ea71b33defd8d782d4fc56797babe4b2e2ee6283700f54ae7a8275786137e812896efaa941e2cb51f36587eca01a01b8bef76060fb099655ef58407b681776669d75757ad28a31c1de2e0427fd440d7bddb0bf04eba5be515a31f94389f90a9f02e8f84456e3f93417b239111824bc60f4fb877a8aed40d48a0600</pre>


<details>
  <summary><code>$ stack exec -- cardano-sl-tx-utils decode 8283...0600</code></summary>

  ```json
  {
      "taWitness": [
          {
              "tag": "PkWitness",
              "key": "sLkUxIx/a2GFQ+bLhRb5PIUMNj8B+wLvP2aYS+f51U91h1DOMI4rq/pMp+cyhsA2fwV0+KjVsm721+eswkzqmw==",
              "sig": "d2e1691d2d6323f8d271406a63b4daa0eb0955666a97954e85864cca210d1f0623217f5b49f173c37ff4b56241e05556c3ad5e3604637f37a0b76cf91e851a03"
          },
          {
              "tag": "PkWitness",
              "key": "XQh0yeEB+ETz16XNfHY9VcvE0kt3Wnj2yuyiISPdssPmPjzTx1wFX7vn92zTC94ILq3RjvcUGASXZ6PBR7pTyw==",
              "sig": "4d75ee119f872e17f3dcc68c48532d028cf226e2a401fc1208c196dee32c6d796cedc34cd79fa7b8db242957f0a5488578defb40af17ecb4f03c09d63fdab40d"
          },
          {
              "tag": "PkWitness",
              "key": "UGEjYVnVeOpxsz3v2NeC1PxWeXur5LLi7mKDcA9UrnqCdXhhN+gSiW76qUHiy1HzZYfsoBoBuL73YGD7CZZV7w==",
              "sig": "7b681776669d75757ad28a31c1de2e0427fd440d7bddb0bf04eba5be515a31f94389f90a9f02e8f84456e3f93417b239111824bc60f4fb877a8aed40d48a0600"
          }
      ],
      "taTx": {
          "_txAttributes": {
              "attrRemain": {},
              "attrData": []
          },
          "_txInputs": [
              "TxInUtxo_07ae8806188c46519f9aaf378b52ac0ea158cfcac023a18f2739bfa08c72329f_1",
              "TxInUtxo_c4fc6963205ca84670bc8ee2b2b9f8c43346585b1246d3de7e1512dfe2c05eec_0",
              "TxInUtxo_223ce2872de6e3b890f343757b776e541be3bbf75cb5c543fd0ab8f2c9d5a156_0"
          ],
          "_txOutputs": [
              {
                  "coin": 10719648843,
                  "address": "Ae2tdPwUPEZEcNV863gEDZQ7Kd7shoYut9wuzQR1puMVPgvrpAxwEnzWFKM"
              }
          ]
      }
  }
  ```
</details>

<details>
  <summary><code>$ stack exec -- cardano-sl-tx-utils verify signatures 8283...0600</code></summary>

  ```json
  [
      "ValidSignature",
      "sLkUxIx/a2GFQ+bLhRb5PIUMNj8B+wLvP2aYS+f51U91h1DOMI4rq/pMp+cyhsA2fwV0+KjVsm721+eswkzqmw=="
  ]
  [
      "ValidSignature",
      "XQh0yeEB+ETz16XNfHY9VcvE0kt3Wnj2yuyiISPdssPmPjzTx1wFX7vn92zTC94ILq3RjvcUGASXZ6PBR7pTyw=="
  ]
  [
      "ValidSignature",
      "UGEjYVnVeOpxsz3v2NeC1PxWeXur5LLi7mKDcA9UrnqCdXhhN+gSiW76qUHiy1HzZYfsoBoBuL73YGD7CZZV7w=="
  ]
  ```
</details>

<details>
  <summary><code>$ stack exec -- cardano-sl-tx-utils verify signatures --protocol-magic 42 8283...0600</code></summary>

  ```json
  [
      "InvalidSignature",
      "sLkUxIx/a2GFQ+bLhRb5PIUMNj8B+wLvP2aYS+f51U91h1DOMI4rq/pMp+cyhsA2fwV0+KjVsm721+eswkzqmw=="
  ]
  [
      "InvalidSignature",
      "XQh0yeEB+ETz16XNfHY9VcvE0kt3Wnj2yuyiISPdssPmPjzTx1wFX7vn92zTC94ILq3RjvcUGASXZ6PBR7pTyw=="
  ]
  [
      "InvalidSignature",
      "UGEjYVnVeOpxsz3v2NeC1PxWeXur5LLi7mKDcA9UrnqCdXhhN+gSiW76qUHiy1HzZYfsoBoBuL73YGD7CZZV7w=="
  ]
  ```
</details>
