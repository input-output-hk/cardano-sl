# Authentication

To defend against impersonation attacks, as well as to maintain an encrypted communication
between clients and servers, both parties can rely on TLS certificates and mututal verification
of their identity. 


## Table of Contents

  * [Motivation and Design](#motivation-and-design)
  * [Prerequisites](#prerequisites)
  * [Generating Certificates](#generating-certificates)
  * [Using Certificates](#using-certificates)
    + [Setting Up Cardano-SL Backend](#setting-up-cardano-sl-backend)
    + [Contacting Cardano-SL Backend](#contacting-cardano-sl-backend)


## Motivation and Design

Communication between _daedalus_ and _cardano-sl_ is done via http server. There are various
attacks possible to this kind of environment. In order to secure the connection between these
two services, we rely on TLS and X.509 public key certificates signed by a known authority. 

Hence, the backend server expects clients to provide valid X.509 v3 certificates signed by an
approved authority (identified by another certificate, provided on initialization) to verify
their identity. On the other hand, clients must check the certificate provided by the server to
make sure they're dealing with the expected party and not a malicious service trying to hijack
the backend's identity. 

Certificates (and corresponding private keys) are generated off-band, and signed by a same CA's
(e.g. _Certificate Authority_) private key. The steps below describe how to securely generate
them. Securing the access to the provided private keys is of vitale importance in order to keep
the system safe. **Any process or machine with access to the private keys may be able to
usurp either the server or the clients identities**.


## Prerequisites

- Build the cardano's tools using nix or stack


## Generating TLS certificates

> **NOTE** If you build the application using Nix 
> 
> e.g. `$ nix-build -A connectScripts.stagingWallet -o connect-to-staging`
>
> There's nothing to do, certificates will be generated upon launching the cluster
> if they're missing, using the default configuration. 


Under the hood, the cardano launcher uses a tool `cardano-x509-certificates` to generate 
X.509 certificates and keys required for TLS authentication and encryption. Using stack, 
you can run the following:

```
$ stack exec -- cardano-x509-certificates --help

Usage: cardano-x509-certificates --server-out-dir FILEPATH
                                 --clients-out-dir FILEPATH
                                 [--ca-out-dir FILEPATH]
                                 (-k|--configuration-key KEY)
                                 (-c|--configuration-file FILEPATH)

Available options:
  --server-out-dir        FILEPATH Output dir for Server certificate & private key
  --clients-out-dir       FILEPATH Output dir for Client certificate(s) & private key
  --ca-out-dir            FILEPATH Optional, output dir for the CA certificate & private key
  -k,--configuration-key  KEY      Configuration key within the config file 
  -c,--configuration-file FILEPATH Configuration file describing the PKI
  -h,--help                        Show this help text
```

A complete walkthrough of this tool is available here: [tools/src/gencerts/README.md](../tools/src/gencerts/README.md). 

The `tls` section from the configuration allows you to tweak the expiry dates and the available
domains for your certificates. By default, clients and servers are considered running on the
same host (localhost).


## Using Certificates

### Setting Up Cardano-SL Backend

Once the CA certificate, the server certificate and private key are in your possession, they
can be fed to _cardano-sl_ as Command-Line Interface options:

```
$ cardano-node --help
...
Available options:
...
--tlscert FILEPATH       Path to file with TLS certificate
--tlskey  FILEPATH       Path to file with TLS key
--tlsca   FILEPATH       Path to file with TLS certificate authority
...
```

For example:

```
$ cardano-node \
  --tlscert ./tls/server/server.crt \
  --tlskey  ./tls/server/server.key \
  --tlsca   ./tls/ca/ca.crt 
```

All those options are actually optional. When missing, the node looks for default development
certificates and key in `<repo-root>/scripts/tls-files/`. 

#### Disable TLS (Not Recommended)

If needed, you can disable TLS by providing the `--no-tls` flag to the wallet or by running a
wallet in debug mode with `--wallet-debug` turned on.


### Contacting Cardano-SL Backend

When sending request to a node that has TLS enabled, clients need to provide their public
certificate to the backend and successfuly perform a TLS handshake with it. This can be achieve
in various ways depending on the client you use and the library available in your programming
language. Make sure to provide the client certificate and key generatd in previous steps and
signed by the same authority provided to the server.

Additionally, to try things out on Unix systems, you may use `curl` as follows:

```
$ curl -v --ca-cert ./tls/client/ca.crt --cert ./tls/client/client.crt --key ./tls/client/client.key <url>
```

Alternatively with `curl`, you may use the `.pem` file as a shortcut to provide the key and
certificate at once:

```
$ curl -v --ca-cert ./tls/client/ca.crt --cert ./tls/client/client.pem <url>
```

Verify that the connection is correctly setup and that TLS is enabled by looking at the output:

```
* SSL connection using TLS1.2 / ECDHE_RSA_AES_256_GCM_SHA384
* 	 server certificate verification SKIPPED
* 	 server certificate status verification SKIPPED
* 	 common name: Cardano Settlement Layer Server Node (matched)
* 	 server certificate expiration date OK
* 	 server certificate activation date OK
* 	 certificate public key: RSA
* 	 certificate version: #3
* 	 subject: DC=io,DC=iohk,O=Input Output HK,CN=Cardano Settlement Layer Server Node
* 	 start date: Fri, 16 Mar 2018 14:16:06 GMT
* 	 expire date: Mon, 15 Mar 2021 14:16:06 GMT
* 	 issuer: DC=io,DC=iohk,O=Input Output HK,OU=Daedalus Self-Signed Root CA,CN=Daedalus Self-Signed Root CA
* 	 compression: NULL
```
