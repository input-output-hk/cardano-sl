A Provably Secure Proof-of-Stake Blockchain Protocol
---

This repository
---
In the paper that lends its name to this project¹, a "proof-of-stake" protocol
with rigorous security guarantees is described in detail, one upon which a
full-fledged cryptocurrency can be based.

This repository is the first implementation of a cryptocurrency that uses the
provably secure distributed consensus "proof-of-stake" model proposed in the
aforementioned paper, written using the Haskell functional programming language.
The paper was as closely followed as possible by the implementation's authors.

Platform support
---

At the moment, the only supported platform is Linux. Mac OS X isn't
officially supported, but it should be possible to deploy and run
'pos-haskell-prototype' on Mac OS X the same way as it is done on Linux.

Windows support is a milestone actively being worked on, and planned for the
near future.

Installation
---

To build from source, clone this repository, navigate to the directory
into which the project was cloned and run ``stack build`` there.
---

#### References

¹ — A. Kiayias, I. Konstantinou, A. Russell, B. David, R. Oliynykov, "[A Provably Secure Proof-of-Stake Blockchain Protocol](https://eprint.iacr.org/2016/889.pdf)"

=====================

[![Build Status](https://travis-ci.org/input-output-hk/pos-haskell-prototype.svg?branch=develop)](https://travis-ci.org/input-output-hk/pos-haskell-prototype)
