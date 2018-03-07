<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Introduction</a></li>
<li><a href="#sec-2">2. Abstract</a></li>
<li><a href="#sec-3">3. Motivation</a></li>
<li><a href="#sec-4">4. Current design</a>
<li><a href="#sec-5">5. Proposed design</a>
<ul>
<li><a href="#sec-5-1">5.1. Nodejs frontend change</a></li>
<li><a href="#sec-5-2">5.2. Haskell backend change</a></li>
<ul>
</li>
</ul>
</div>
</div>

# Introduction<a id="sec-1" name="sec-1"></a>

# Abstract<a id="sec-2" name="sec-2"></a>

The purpose of this document is to review security model in communication
 between wallet backend and wallet frontend (Daedalus).

# Motivation<a id="sec-3" name="sec-3"></a>

Communication between wallet frontend (Daedalus) and wallet backend in cardano
 is done via http server. There are various attacks possible to this kind of 
environmnet (TODO footnote or further list of possible attack vectors needed).

# Current design<a id="sec-4" name="sec-4"></a>

This summarises current approach in server-client communication.

-   wallet server has https server (based on TLS CA server cert) set up and listening on specific IP:PORT
    
-   client is using its on pair of CA certificate in order to verify server cert when sending request to server

Downsides:

-   any other user on the same machine can connect to the wallet backend. If there's no spending password they can also spend

# Proposed design<a id="sec-5" name="sec-5"></a>

From [devops-103](https://iohk.myjetbrains.com/youtrack/issue/DEVOPS-103):

-   to defend against the most obvious of impersonation attacks, we need the wallet's frontend and backend to mutually authenticate to each other

-   the previous scheme, partly based on TLS CA server cert (provided by the backend) and the authentication password (provided by the frontend, CSM-100) was deemed too be unnecessarily hostile to the user (who had to remember Yet Another Password)

-   previous point means we have to make do with a non-interactive mutual authentication scheme

-   PKI provides us with such a scheme – the CA might be used to sign both the backend key/certificate pair (which we already do in DEVOPS-33/CSM-116) and the frontend key/certificate pair


The proposal is to implement this scheme:

-   Instead of three artifacts (CA cert, backend key, backend cert), the installer will generate five (+frontend key, +frontend cert) - already implemented

-   The frontend will have to provide the cert to the backend

-   The backend will switch from ignoring the client certificate to requiring a valid one to be provided

## Nodejs frontend change<a id="sec-5-1" name="sec-5-1"></a>

Current implementation (29 Jan 2018) is using nodejs `https` library which exposes `ca` placeholder for client side TLS authentication [daedalus request](https://github.com/input-output-hk/daedalus/blob/eaec35f44bbfe4335445d635a53fbbcb3b2e686c/app/api/etc/lib/request.js#L4) :

```js
export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  ca: string,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};
```

Details how `https` can be used can be found here [nodejs https](https://nodejs.org/api/https.html#https_https_request_options_callback) .

Our proposal is to extend it and add aditional `cert` field which should enable us to verify this certificate later on server side:

```js
export type RequestOptions = {
  hostname: string,
  method: string,
  path: string,
  port: number,
  ca: string,
  cert: string,
  headers?: {
    'Content-Type': string,
    'Content-Length': number,
  },
};

```

`cert` should be produced by the installers and our installers already provide them inside:

```shell
$ cd daedalus/installers
$ sh build-certificates-unix.sh
$ tree tls/
tls/
├── ca
│   ├── 01.pem
│   ├── 02.pem
│   ├── 03.pem
│   ├── ca.crt
│   ├── ca.csr
│   └── db
│       ├── ca.crt.srl
│       ├── ca.crt.srl.old
│       ├── ca.db
│       ├── ca.db.attr
│       ├── ca.db.attr.old
│       └── ca.db.old
├── client
│   ├── client.crt
│   ├── client.csr
│   └── client.key
└── server
    ├── server.crt
    ├── server.csr
    └── server.key
```

`cert` should be `tls/client/client.crt`.


## Haskell backend change<a id="sec-5-2" name="sec-5-2"></a>

There is no out of box solution for server side TLS authentication (last checked in Oct 2017) but implementation should be straightforward. Backend should check does client certificate matches his CA certificate. There is a package on hackage called `warp-tls` that provides mechanics for this to be implemented [tlsServerHooks](https://hackage.haskell.org/package/warp-tls-3.2.4/docs/Network-Wai-Handler-WarpTLS.html#v:tlsServerHooks) :

```haskell
tlsServerHooks :: TLSSettings -> ServerHooks
```

With the obove one should be able to implement server side TLS auth.

TODO: figure out details how to do the above.
