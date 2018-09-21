# Cardano-SL-x509

> Pure Haskell 'replacement' for OpenSSL to generate certificates for a TLS Private Key Infrastructure

## How to Use

```hs
import           Crypto.PubKey.RSA (PrivateKey, PublicKey)

import           Cardano.X509.Configuration (DirConfiguration(..), CertDescription(..),
                     decodeConfigFile, fromConfiguration, genCertificate)
import           Data.X509.Extra (genRSA256KeyPair)


main :: IO ()
main = do
    confFile <-
        decodeConfigFile "dev" "lib/configuration.yaml"

    let dirConf =
          DirConfiguration "server" "client" Nothing

    (caDesc, certDescs) <-
        fromConfiguration confFile dirConf genRSA256KeyPair <$> genRSA256KeyPair

    (caKey, caCert) <- 
        genCertificate caDesc

    (clientKey, clientCert) <-
        genCertificate (findCert "client" certDescs)

    (serverKey, serverCert) <-
        genCertificate (findCert "server" certDescs)

    -- Do something with the Private Key Infrastructure
  where
    findCert 
      :: String 
      -> [CertDescription IO PublicKey PrivateKey String]
      -> CertDescription IO PublicKey PrivateKey String
    findCert outDir =
        head . find ((== outDir) . certOutDir)
```

Alternatively, have a look at [gencerts/Main.hs](https://github.com/input-output-hk/cardano-sl/tools/src/gencerts/Main.hs)


## Configuration

| Configuration Key   | Description                                        |
| ------------------- | -------------                                      |
| \*.\*.organization  | Organization name identified by the certificate    |
| \*.\*.commonName    | Name of the service identified by the certificate  |
| \*.\*.expiryDays    | Number of days after which the certificate expires |
| tls.server.altDNS   | Alternative Subject Names for the server           |

Note that the `clients` section requires (and accepts) a list of clients. You may define more
than one client certificates at once.

For example:

<details>
<summary><strong>configuration.yaml</strong></summary>

```yaml
dev: 
  tls: 
    ca:
      organization: Input Output HK
      commonName: Cardano SL Self-Signed Root CA
      expiryDays: 3650

    server:
      organization: Input Output HK
      commonName: Cardano SL Server Node
      expiryDays: 365
      altDNS:
        - "localhost"
        - "localhost.localdomain"
        - "127.0.0.1"
        - "::1"

    clients:
      - organization: Input Output HK
        commonName: Daedalus Wallet
        expiryDays: 365
```
</details>
