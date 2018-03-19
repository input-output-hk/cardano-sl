# Authentication

To defend against impersonation attacks, as well as to maintain an encrypted communication
between the wallet's frontend (a.k.a. _[daedalus](https://github.com/input-output-hk/daedalus)_)
and the wallet's backend (a.k.a _[cardano-sl](https://github.com/input-output-hk/cardano-sl)_) both parties rely on TLS certificates and mututal
verification of their identity. 


## Table of Contents

  * [Motivation and Design](#motivation-and-design)

  * [Prerequisites](#prerequisites)

  * [Generating Certificates](#generating-certificates)
    + [Default (localhost)](#quick-and-easy)
    + [Personalized (custom domain)](#step-by-step)

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
usurp daedalus or cardano-sl's identities**.


## Prerequisites

- Install and configure [OpenSSL](https://www.openssl.org/)


## Generating Certificates

> **NOTE**: If you're setting up backend nodes and a frontend wallet on a same machine, 
> you can ignore the _personalized_ installation and fully rely on the _default_ case.

The following steps are described as if executed from a _Unix_ system (Linux or Mac OS) however,
scripts used are also available on _Windows_ and steps could be easily transposed to this
system.

### Default (localhost, 3 years)

#### Downloading Script

 _[daedalus](https://github.com/input-output-hk/daedalus)_ provides a script
 `installers/build-certificates-unix.sh` (or a windows equivalent
 `installers/build-certificates-win64.bat`) to generate default certificates for both
 _cardano-sl_ and _daedalus_, as well as a CA public certificate used to sign the formers. 

Make sure you have an Internet access and download the installer script and the necessary
configuration files as such:

```
$ wget -q https://raw.githubusercontent.com/input-output-hk/daedalus/master/installers/build-certificates-unix.sh
$ wget -q https://raw.githubusercontent.com/input-output-hk/daedalus/master/installers/{ca,server,client}.conf
```

#### Running Script

Run the script to generate the certificates as follows:

```
$ bash ./build-certificates-unix.sh 
```

Once done (`Oll Korrect`), a folder `tls` should have been created in your current directory
containing, at least, the following files:

```
--| tls
-----| ca
--------| ca.crt
-----| client
--------| client.crt
--------| client.key
-----| server
--------| server.crt
--------| server.key
```

Those are the certificates and private keys that _daedalus_ and _cardano-sl_ will require to
talk to each other. See [Using Certificates](#using-certificates) for more details.


### Personalized 

Follow the steps described in [Default | Downloading Script](#downloading-script) to first pull
the build script and the required configuration files.

> **NOTE:** You can also simply use compatible certificates that have been signed by an
> existing authority. Make sure you have the public certificate of that authority in your 
> possession.

#### Custom Server Domains

The `build-certificates-*` script will generate server's certificates for `localhost`. This
seemingly assume several things:

  - The backend nodes and frontend are running on the same machine
  - You don't need to enforce the backend domain (multiple services may be running on `localhost`)

In any case, you might need to run the backend on a different domain and therefore, identify it
using something else than `localhost`. To do so, modify the `[ server_alt_names ]` section in
the `ca.conf` downloaded earlier:

```
[ server_alt_names ]
DNS.1                   = localhost.localdomain
DNS.2                   = localhost
DNS.3                   = 127.0.0.1
DNS.4                   = ::1
```

and replace DNS references with your actual domain. For instance:

```
[ server_alt_names ]
DNS.1                   = www.example.com
DNS.2                   = example.com
```

Then, follow the steps described in [Default | Running Script](#running-script) to generate the certificates.

#### Different Expiry Date

By default, certificates generated are valid for a period of 3 years. If you need a shorter (or
longer) period, you can tweak the CA configuration to provide a different duration (in days).
To do so, modify the `[ daedssrca ]` section in the `ca.conf` downloaded earlier:

```
[ daedssrca ]
...
default_days      = 1095
default_crl_days  = 1095
...
```

and replace the highlighted values above with whatever value you need. For instance, for a
validity period of only one year:

```
[ daedssrca ]
...
default_days      = 365
default_crl_days  = 365
...
```

Then, follow the steps described in [Default | Running Script](#running-script) to generate the certificates.


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
$ curl -kv --cert ./tls/client/client.crt --key ./tls/client/client.key <url>
```

The `k` flag ignores security checks on certificates (as they're done by the backend anyway),
and because we are using it to debug or test. Optionally, you may aggregate your client
certificate and key in a portable `.pem` format doing so:

```
$ cat ./tls/client/client.crt ./tls/client/client.key > ./tls/client/client.pem
```

Use it with `curl` the following way:

```
$ curl -kv --cert ./tls/client/client.pem <url>
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
