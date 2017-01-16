A Provably Secure Proof-of-Stake Blockchain Protocol
----------------------------------------------------

[![Build Status](https://travis-ci.org/input-output-hk/cardano-sl.svg)](https://travis-ci.org/input-output-hk/cardano-sl)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/input-output-hk/cardano-sl?branch=master&svg=true)](https://ci.appveyor.com/project/jagajaga/cardano-sl)
[![Release](https://img.shields.io/github/release/input-output-hk/cardano-sl.svg)](https://github.com/input-output-hk/cardano-sl/releases)

This repository
---------------

This repository contains a Haskell implementation of the _Provably Secure
Proof-of-Stake_ white paper[1], done in conjunction with members of
[IOHK](https://iohk.io), the University of Edinburgh, the University of
Athens, and the University of Connecticut.

In the paper that lends its name to this project¹, a "Proof-of-Stake" protocol
with rigorous security guarantees is described in detail, one upon which a
full-fledged cryptocurrency can be based.

This repository is the first implementation of a cryptocurrency that uses the
provably secure distributed consensus "proof-of-stake" model proposed in the
aforementioned paper.

The paper was as closely followed as possible by the implementation's authors.

Introduction
------------

This repository hosts the prototype implementation of "A Provably Secure
Proof-of-Stake Blockchain Protocol", the latest version of which can be found
in the [IOHK website](https://iohk.io/research/papers/a-provably-secure-proof-of-stake-blockchain-protocol/).

### Where to start

Aside from reading the paper[1], a good starting point is the source code
hosted herein. We recommend starting with the following source files:

- [Node entry point](src/node/Main.hs)
- [Core node logic](src/Pos/Launcher.hs), and the exported modules.
- [Listeners](src/Pos/Communication/)
- [Workers](src/Pos/Worker/)

Platform support
----------------

At the moment, the only supported platform is Linux. Mac OS X isn't
officially supported, but `pos-haskell-prototype` on Mac OS X has thus far
been ran and partially tested on Mac OS X.

Windows support is a milestone actively being worked on, and planned for the
near future.

Installation
------------

To build from source, clone this repository, navigate to the directory
into which the project was cloned and run `stack build`.

The [`stack` system](https://docs.haskellstack.org/en/stable/README/) is
required before `pos-haskell-prototype` can be built.

Benchmarking
------------

_Pending_

Contributing
------------

_Pending_

#### References

[1]: A. Kiayias, I. Konstantinou, A. Russell, B. David, R. Oliynykov, "[A Provably Secure Proof-of-Stake Blockchain Protocol](https://eprint.iacr.org/2016/889.pdf)"
