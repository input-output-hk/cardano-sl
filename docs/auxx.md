# The Cardano SL Auxx Tool and Language

Cardano SL is a distributed system, and one of the challenges when developing
or testing it is to interact with it. There can be multiple nodes running
locally, and we want to inspect and analyze their state, send commands to them
or on their behalf, etc. One of the tools we developed for this purpose is
`cardano-auxx` -- it's a command-line application that connects to the network
and executes user commands.

Auxx aims to cover a wide variety of use cases, so it supports many modes of
operation. First, it can either execute a single command supplied as an
argument (batch mode) or run as a REPL (interactive mode). Second, it can
either run without connecting to the network (light mode), with connection to
the network (medium mode), or with connection to the network and with
transaction processing (heavy mode).

The commands issued to auxx used to be simple, and the tool was extremely
ad-hoc: for each command there was a separate parser implemented, and there was
no way to pass the result of one command to another. As we used auxx more and more,
it grew apparent that we needed more flexibility -- in particular, writing
composite commands, where the output of one is used as input to another.

To allow command composition we developed a language that subsumed
previous ad-hoc operations with a principled execution model.

## Why a Language?

Let us start with motivation: why would we want to invent *yet another*
language? After all, we are happy using Haskell for Cardano SL development,
why not use GHCi for development?

The answer is twofold. First, the auxx tool is designed to be used *with*
Haskell, not instead of it.  The commands that are executed by auxx are
implemented in Haskell -- there is no standard lib for the auxx language, and
the design is such that adding new built-ins is extremely easy -- it is truly a
command language, not a programming language. It covers a completely
different use case -- issuing one-off commands, and being such a language, it
is also very different from Haskell.

Being designed for a completely different purpose from Haskell, the auxx
language differs in important ways -- and that's what makes it more convenient.

* The auxx language has no effect system. While Haskell tries to make effect handling
  explicit in the name of purity and referential transparency, the auxx tool
  executes commands in the same monad that our application runs, and it has *side-effects*.
  Yes -- we don't want clean maintainable code, we want to quickly
  issue commands, and no one wants to be bothered with binding variables in
  do-notation (Idris has bang-notation, which is a bit more convenient, but still
  introduces syntactic noise).

* The auxx language has vastly better story for *variadic functions*. Haskell with its
  currying forces a strict order of arguments, or you have to use awkward
  fundep-based tricks to get something resembling variadic functions. But in
  auxx, we implemented variadic functions, opitonal arguments, keyword arguments,
  positional arguments, type-indexed arguments -- you name it, just call the
  command *somehow*. And yet, while being flexible, the system is strict and
  well-defined, it does no guesswork.

* The auxx language has *implicit conversions*. That's right -- who needs strong types
  to write a few commands? The ability to skip calling a dozen of conversion
  functions easily beats the elegance of strongly typed code. Remember, we want
  to issue commands quickly and effortlessly, rather than write maintanable code.

* The auxx language has *domain-specific lexical rules*. While in Haskell we have to go
  through hoops to reuse existing literal syntax (with things like
  OverloadedStrings), in auxx we implemented literals that we actually need for
  commands. This includes filepaths (just start with `/` or `./`), block
  versions (they look like `1.0.3`), public keys, wallet addresses, stakeholder
  ids -- you name it.

Things that Haskell carefully avoids, the auxx language embraces. Language
design isn't about black and white, it's about tradeoffs, and Haskell has made
some tradeoffs that make it simply unsuitable to quickly issue a few commands.

What about other languages, like issuing commands from the shell? First, the
shell is rather inconvenient for the simple reason that it is to reliant on
strings -- in auxx we manipulate actual (dynamically) typed values, and this
saves a lot of headaches. Second, we avoid marshalling overhead and startup
overhead when running multiple commands.

The last option is to embed an existing language, such as Lua or Python,
designed for scripting. While a viable option, it would present way more work
to get these languages to interface nicely with our Haskell code, for no
apparent gain, and we'd also have no control over supported features
(especially domain-specific ones).

As no existing option would be as convenient as our own language, that's what
we implemented. And here I just want to praise Haskell for how easy it to
implement a language in it -- the entire parser and interpreter took only a few
days!

## Basic Usage

The binary is called `cardano-auxx` and we can use it to start a REPL:

```
$ stack exec -- cardano-auxx repl
Welcome to Cardano SL Auxx REPL
Mode: light
... the auxx plugin is ready
auxx>
```

Notice that no additional parameters were passed to it, so it runs in
lightweight mode, where most of the commands (anything to do with networking)
are not supported.

The command to list all other commands is unsurprisingly called `help`, and it is
aware of the current mode, so you will see what commands are unavailable:

```
auxx> help
Available commands:

-- construct a list
L elem: Value*

pk is unavailable because AuxxMode is not available

s is unavailable because AuxxMode is not available

addr is unavailable because AuxxMode is not available

addr-hd is unavailable because AuxxMode is not available

-- construct a transaction output
tx-out addr: Address
       value: Coin

hash-installer file: FilePath

...
```

For instance, let us use the `hash-installer` commands. From the help message we can
see that it has a single parameter named `file`, and this parameter has type `FilePath`.

We can run `hash-installer` specifying the parameter name explicitly (keyword argument) or omitting it (positional argument):

```
auxx> hash-installer file: ./update.example
Hash of installer './update.example' is c78823ad3dc7ef861a09019e73e66af9ea8832bcaf5073215721e1d78018f1b0

auxx> hash-installer ./update.example
Hash of installer './update.example' is c78823ad3dc7ef861a09019e73e66af9ea8832bcaf5073215721e1d78018f1b0
```

The parameter is mandatory, so we get an error if we forget to specify it:

```
auxx> hash-installer
Invalid arguments for 'hash-installer':
  Missing keys: file
```

We also get an error if we specify something that the command doesn't know how to process:

```
auxx> hash-installer index:42
Invalid arguments for 'hash-installer':
  Missing keys: file
  Irrelevant keys: index
```

Here we see that `index` is reported as irrelevant, and we *also* see that we
forgot to specify `file`. Argument processing is done in a way that we see all
errors reported at once.

We also get an error if we pass something of an incorrect type:

```
auxx> hash-installer 42
Invalid arguments for 'hash-installer':
  Following type errors occured:
    Couldn't match expected type FilePath with actual value ValueNumber 42.0!
```

This time we didn't specify the key, so `hash-installer` assumed that we
intended to pass a filepath to it. However, `42` is a number, not a filepath --
and this is what the error is telling us.

Finally, when we specify too many arguments without names, we get an error that tells
us about the amount of irrelevant positional arguments:

```
auxx> hash-installer ./update.example 1 1 1
Invalid arguments for 'hash-installer':
  Irrelevant positional arguments: 3
```

If you look at the help message, you will notice that some types are marked
with a star or a question mark. This syntax is straight from regular
expressions -- a star means 0+, a plus means 1+, and a question mark means 0-1. For example, here's
the help entry about the `L` function:

```
-- construct a list
L elem: Value*
```

This means that we can invoke it simply as `L` to get an empty list, or as `L
elem:1 elem:2 elem:3` to get a list with three elements `1`, `2`, and `3`.
Remember that it's alright to leave out parameter names, so we can just call `L
1 2 3`. And of course, nested lists are supported: `L 1 (L "a" "b" "c") 3`.

Notice that in the last example we passed the result of one `L` invocation as
an argument to another `L` invocation, and used parentheses for grouping. As
simple as it looks, this hadn't been supported by auxx before the language was
introduced.

At the moment there aren't many commands that can be run in the lightweight
mode, let's see how to run auxx with configuration:

```
$ scripts/launch/auxx.sh repl
/home/gumo/Projects/cardano-sl/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/bin/cardano-auxx  --peer 127.0.0.1:3000 --peer 127.0.0.1:3001 --peer 127.0.0.1:3002 --peer 127.0.0.1:3003   --json-log=/home/gumo/Projects/cardano-sl/scripts/../logs/2017-11-25_171923/node.json  --logs-prefix /home/gumo/Projects/cardano-sl/scripts/../logs/2017-11-25_171923 --log-config /home/gumo/Projects/cardano-sl/scripts/../logs/2017-11-25_171923/conf/auxx.log.yaml                   --system-start 100500 repl
Welcome to Cardano SL Auxx REPL
Mode: with-config
... the auxx plugin is ready
auxx>
```

The `scripts/launch/auxx.sh` script runs `cardano-auxx` with some default
parameters. Notice that we see `Mode: with-config` in the beginning (it was
`Mode: light` in previous examples).

This time if we run `help`, all commands will be available. However, not all of
them will use up-to-date context. To keep up with the blockchain, specify the mode explicitly:

```
$ scripts/launch/auxx.sh repl --mode with-node
/home/gumo/Projects/cardano-sl/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/bin/cardano-auxx  --peer 127.0.0.1:3000 --peer 127.0.0.1:3001 --peer 127.0.0.1:3002 --peer 127.0.0.1:3003   --json-log=/home/gumo/Projects/cardano-sl/scripts/../logs/2017-11-25_172834/node.json  --logs-prefix /home/gumo/Projects/cardano-sl/scripts/../logs/2017-11-25_172834 --log-config /home/gumo/Projects/cardano-sl/scripts/../logs/2017-11-25_172834/conf/auxx.log.yaml                   --system-start 100500 repl --mode with-node
Welcome to Cardano SL Auxx REPL
... the auxx plugin is ready
auxx>
```

## Keys, Addresses, and Transactions

In order to act on behalf of other nodes, `cardano-auxx` has access to their
secret keys, stored in the `secret.key` file. When we start auxx for the first
time and this file doesn't exist, the output of `listaddr` will be empty:

```
auxx> listaddr
Available addresses:
auxx>
```

We can add keys using the following commands:

```
add-key-pool i: Int*

add-key file: FilePath
        primary: Bool
```

While `add-key` can be used to add a key from a file, when we run a local
cluster using a demo script, we want to use `add-key-pool` instead. From the
auto-generated help message above we can see that it takes multiple `Int`
parameters, like this:

```
auxx> add-key-pool 0 1 2 3
auxx>
```

Now the `secret.key` file was extended with 4 pre-determined keys, the same as used by nodes
in the demo script. Let's see them:

```
Available addresses:
    #0:   addr:      Ae2tdPwUPEZD7r1mGc4BxGj5ikBop9DpdsKDzKpFujdYpd6bZBn9fZYEgsg
          pk:        ZcrTBz5bwqSA+6pGfg7D55uJfZt++5XiEY7h03ly3++41DDNIerLdILubwKrr1zinBEBJhJORXlla/S4yERogg==
          pk hash:   f83e9e0270abcc744b26570b069f66db5a83cdebe4bedb7cd435c443
          HD addr:   DdzFFzCqrhszS1SoxhSTtM8c3qNyvKZvguJ2GNeTjRY5AXyJFia81xS1cERj215orZ9Fd1BzedinMfSXBJerpHxWpu1tzpx53Q2gEJtL
    #1:   addr:      Ae2tdPwUPEZESKZGFtTEeR761598wbuCEERzqxeEtR69gCEzpTwSiaP7oxj
          pk:        LxQs4CWIxMp3demqmPBMxrI/v/KpscOVwxiXATy0sByGK3idaFpN72k3U/Nxfhk6HGUFQAfcMwEzy0za95BNJw==
          pk hash:   df4901e8f6abd96a8bd0579f24036a8b9b24c1df4bed85a1402db09b
          HD addr:   DdzFFzCqrhtBFQAHQMzQK2vdWLWC55APHkrzQryBgwPN9jNdFTHeSQ4cBVpmQuksLjpeLFCoGwHPxHFvQPNhtXDXFBnAquWumkTdYyDS
    #2:   addr:      Ae2tdPwUPEZMezLqssf4EMpudh7uNSoLKw6f1VN9F2o8rsAckXvx76PoZdc
          pk:        mNZwrlO1fVSn2SV0bqfvp5D22gSToWwDxBZLgbFFOp5HdgO/n8iO5JnofvKajPtyujUch7jQZxmMhvBzBMc5zA==
          pk hash:   c96ac1e118c0d38467e7ff7829e59d051c751c9c371687ed1c3a3cda
          HD addr:   DdzFFzCqrht1aeEzHjtMBKt1WixjyP5dH1N1rt7H7w5VfmtJqfVKFs1EnZ7Tbjd9n7R2m7aNcQDGDge73BEDCFAhB9CemqwCQRmyNQex
    #3:   addr:      Ae2tdPwUPEZKUpTf4jDurpvX7fdUrmZvFPM56TAwihnBbAuyvMZ47NeHHdY
          pk:        ulsqNypOZ39xAbW71PxPsyMbXfEc7RUb+NPHFYA6tdjv0IAAkEN8/yaA9CmdVLFUj1FGZxdpdbTiq6hdeLcJBw==
          pk hash:   81713ef44a9463104d3c7c48d547c20c869536af3da237d8fc12b5fa
          HD addr:   DdzFFzCqrht5oYxSwcfYag9fESzGjwC3HGA3ZmdfraXf3STPRBrrek9NUSF68HkbXkApwSTUJamGbLDF6vvaRZigdLFpLuX6BCFzvoEU

```

The `listaddr` command shows addresses (with default stake distribution),
public keys, public key hashes (stakeholder ids), and HD addresses.

We can access these individually using corresponding commands:

```
auxx> addr 0
Ae2tdPwUPEZD7r1mGc4BxGj5ikBop9DpdsKDzKpFujdYpd6bZBn9fZYEgsg

auxx> pk 0
ZcrTBz5bwqSA+6pGfg7D55uJfZt++5XiEY7h03ly3++41DDNIerLdILubwKrr1zinBEBJhJORXlla/S4yERogg==

auxx> s 0
f83e9e0270abcc744b26570b069f66db5a83cdebe4bedb7cd435c443

auxx> addr-hd 0
DdzFFzCqrhszS1SoxhSTtM8c3qNyvKZvguJ2GNeTjRY5AXyJFia81xS1cERj215orZ9Fd1BzedinMfSXBJerpHxWpu1tzpx53Q2gEJtL
```

Notice that the outputs of these commands differ in length and structure. Auxx
knows how to recognize addresses, public keys, and stakeholder ids, when you
use them as literals. For instance, the `s` command (to compute stakeholder id)
can be applied not only to an index in the `secret.key` file, but also to a
completely unrelated public key:

```
auxx> s vSZZRZVplbSxZ9IPieoGFUSDxXD4gslW9WMlu5EkZRqtLniOL/GUU72oYv91KFYLZV8Z7TsQ/T7XeZOpwMIoeA==
ac12af0ba909d44fc078281ee7f546b681a47323a3ef8c7fb28f4d6c
```

We didn't use quotation marks to write the input public key. We've seen `s`
used with either an int index (of a secret key) or an explicit public key.
These options are both reflected in auto-generated help message:

```
-- stakeholder id (hash) of the specified
-- public key
s pk: (PublicKey | Int)
```

The input type is shown as `PublicKey | Int`. Normally, `s` expects a
`PublicKey` (because the meaning of this function is to convert a public key to
a stakeholder id), but in case we pass `Int`, an implicit conversion is
applied first.

Let's take a closer look at the `addr` command:

```
-- address for the specified public key. a
-- stake distribution can be specified
-- manually (by default it uses the current
-- epoch to determine whether we want to
-- use bootstrap distr)
addr pk: (PublicKey | Int)
     distr: AddrStakeDistribution?
```

We can see that there's an optional `distr` parameter. When in bootstrap era,
the default distribution is `boot`, but we can specify it explicitly:

```
auxx> addr distr:boot 0
Ae2tdPwUPEZD7r1mGc4BxGj5ikBop9DpdsKDzKpFujdYpd6bZBn9fZYEgsg
```

We can also construct a different stake distribution using the `distr` command:

```
-- construct an address distribution (use
-- 'dp' for each part)
distr dp: AddrDistrPart+
```

And to construct an `AddrDistrPart` (we need 1 or more of these), we can use
the `dp` command:

```
-- construct an address distribution part
dp s: (StakeholderId | PublicKey | Int)
   p: CoinPortion
```

It's convenient to specify `CoinPortion` as percentage, and auxx supports
percentages for any numeric literal:

```
auxx> 3%
0.03

auxx> dp p:3% s:0
AddrDistrPart {adpStakeholderId = AbstractHash f83e9e0270abcc744b26570b069f66db5a83cdebe4bedb7cd435c443, adpCoinPortion = CoinPortion {getCoinPortion = 30000000000000}}
```

We used explicit parameter names to specify arguments in the order we want
(first `p:3%`, second `s:0`). If we don't specify parameter names, arguments
are treated positionally and must come in the order that the command expects
them:

```
auxx> dp 3% 0
Invalid arguments for 'dp':
  Following type errors occured:
    Couldn't match expected type StakeholderId | PublicKey | Int with actual value ValueNumber 3.0e-2!

auxx> dp 0 3%
AddrDistrPart {adpStakeholderId = AbstractHash f83e9e0270abcc744b26570b069f66db5a83cdebe4bedb7cd435c443, adpCoinPortion = CoinPortion {getCoinPortion = 30000000000000}}
```

Here's how we can construct a stake distribution:

```
auxx> distr (dp 0 25%) (dp 1 25%) (dp 2 50%)
Multi key distribution: {c96ac1e1: 500000000000000/1000000000000000 (approx. 0.5), df4901e8: 250000000000000/1000000000000000 (approx. 0.25), f83e9e02: 250000000000000/1000000000000000 (approx. 0.25)}
```

We used secret key indices as first arguments for `dp`, but we could've used
any public keys or stakeholder ids instead.

Now that we know how to construct an `AddrStakeDistribution`, let's make an address with it:

```
auxx> addr 3 (distr (dp 0 25%) (dp 1 25%) (dp 2 50%))
9k3qsjoVJ86AgpAZ2fcRhJU3CzDqnuxnf5vopXE844KTbPLrHMRHUWgTMZj2BFeoJmtsAHawxigbzgxM8HFXHZBYweqQZtHKUsfjJrGTrLKFfSK7qLf9FUGGKeMWATqdpT1JXKLBoMRULTzMKAVQfhA44XVPPbkz1xWuKZNAQXyPhKCtfow69CfTY18bpgic6woAgV3r2TpjcEXsTUHH5WbAfk7azYh5jD4
```

This time we used positional arguments, but we can use keyword arguments to
specify stake distribution first:

```
auxx> addr distr:(distr (dp 0 25%) (dp 1 25%) (dp 2 50%)) 3
9k3qsjoVJ86AgpAZ2fcRhJU3CzDqnuxnf5vopXE844KTbPLrHMRHUWgTMZj2BFeoJmtsAHawxigbzgxM8HFXHZBYweqQZtHKUsfjJrGTrLKFfSK7qLf9FUGGKeMWATqdpT1JXKLBoMRULTzMKAVQfhA44XVPPbkz1xWuKZNAQXyPhKCtfow69CfTY18bpgic6woAgV3r2TpjcEXsTUHH5WbAfk7azYh5jD4
```

Now, it may look a little silly to write `distr:(distr ...)` -- of course if we
construct an `AddrStakeDistribution` with `distr`, we intend to use it as
`distr:`! For this reason some commands in auxx may use type information to
annotate parameters automatically. For instance, when `addr` detects some input
of type `AddrStakeDistribution` (which `distr` produces), the `distr:`
parameter name will be implied.

This allows us to specify parameters to `addr` in arbitrary order without
explicit parameter names:

```
auxx> addr 3 (distr (dp 0 25%) (dp 1 25%) (dp 2 50%))
9k3qsjoVJ86AgpAZ2fcRhJU3CzDqnuxnf5vopXE844KTbPLrHMRHUWgTMZj2BFeoJmtsAHawxigbzgxM8HFXHZBYweqQZtHKUsfjJrGTrLKFfSK7qLf9FUGGKeMWATqdpT1JXKLBoMRULTzMKAVQfhA44XVPPbkz1xWuKZNAQXyPhKCtfow69CfTY18bpgic6woAgV3r2TpjcEXsTUHH5WbAfk7azYh5jD4

auxx> addr (distr (dp 0 25%) (dp 1 25%) (dp 2 50%)) 3
9k3qsjoVJ86AgpAZ2fcRhJU3CzDqnuxnf5vopXE844KTbPLrHMRHUWgTMZj2BFeoJmtsAHawxigbzgxM8HFXHZBYweqQZtHKUsfjJrGTrLKFfSK7qLf9FUGGKeMWATqdpT1JXKLBoMRULTzMKAVQfhA44XVPPbkz1xWuKZNAQXyPhKCtfow69CfTY18bpgic6woAgV3r2TpjcEXsTUHH5WbAfk7azYh5jD4
```

This works *not* because `distr` parameter name and `distr` command name
coincide, but because of (dynamic) type information. Also, the `addr` command
has explicit logic to support this (other commands may not -- but it would be
nice to make this work automatically for all commands in the future).

We can look at the balance at this address:

```
auxx> balance (addr (distr (dp 0 25%) (dp 1 25%) (dp 2 50%)) 3)
0
```

To send a transaction we need a secret key (from which we send) and 1+
transaction outputs (an address to which we send, and the amount of coins to
send):

```
-- send from #i to specified transaction
-- outputs (use 'tx-out' to build them)
send i: Int
     out: TxOut+

-- construct a transaction output
tx-out addr: Address
       value: Coin
```

Let's send some funds to the address we generated:

```
auxx> send 0 (tx-out value:5 addr:(addr (distr (dp 0 25%) (dp 1 25%) (dp 2 50%)) 3))
```

or, more concisely (relying on type information):

```
auxx> send 0 (tx-out 5 (addr (distr (dp 0 25%) (dp 1 25%) (dp 2 50%)) 3))
```

### Proposing an Update

We can propose an update using the `propose-update` command:

```
-- propose an update with one positive vote
-- for it using secret key #i
propose-update i: Int
               vote-all: Bool
               block-version: BlockVersion
               software-version: SoftwareVersion
               bvm: BlockVersionModifier?
               update: ProposeUpdateSystem*
```

The first `i` parameter specifies which node proposes the update (a secret key
index). Then the `vote-all` flag determines if we want an automatic positive vote
from all other nodes (otherwise we can vote using the `vote` function).

To construct `BlockVersion` we can use a literal:

```
auxx> 1.0.3
1.0.3
```

To construct `SoftwareVersion` we can use the `software` function:

```
auxx> software "cardano-sl" 3
cardano-sl:3
```

Thus, a minimal proposal looks like this:

```
auxx> propose-update 0 vote-all:true 1.0.3 (software "cardano-sl" 1)
Update proposal submitted along with votes, upId: f372755687c702b5676f2441f8bec55fa060aa461c59a0e067cf996b20468855
f372755687c702b5676f2441f8bec55fa060aa461c59a0e067cf996b20468855
```

It's okay to specify block version and software version out of order, in random
places -- their types are used to find them:

```
auxx> propose-update  (software "cardano-sl" 1) 0 vote-all:true 1.0.3
Update proposal submitted along with votes, upId: f372755687c702b5676f2441f8bec55fa060aa461c59a0e067cf996b20468855
f372755687c702b5676f2441f8bec55fa060aa461c59a0e067cf996b20468855
```

Granted, this proposal does nothing except bump some versions. In order to do something useful, we must
specify `BlockVersionModifier` and/or `ProposeUpdateSystem`. We can construct
these with `bvm` and `upd-bin` accordingly:

```
-- construct a BlockVersionModifier
bvm script-version: ScriptVersion?
    slot-duration: Second?
    max-block-size: Byte?
    max-header-size: Byte?
    max-tx-size: Byte?
    max-proposal-size: Byte?
    mpc-thd: CoinPortion?
    heavy-del-thd: CoinPortion?
    update-vote-thd: CoinPortion?
    update-proposal-thd: CoinPortion?
    unlock-stake-epoch: EpochIndex?

-- construct a part of the update proposal
-- for binary update
upd-bin system: SystemTag
        installer-path: FilePath?
        bin-diff-path: FilePath?
```

For instance:

```
auxx> propose-update  (software "cardano-sl" 1) 0 vote-all:true 1.0.3 (bvm unlock-stake-epoch: 4)
Update proposal submitted along with votes, upId: 7d382ace717d15c8221f94f12010821f29de3ece41dbbae98f4568b607490cdf
7d382ace717d15c8221f94f12010821f29de3ece41dbbae98f4568b607490cdf
```

### Extending Auxx

All auxx commands are built-ins, and it's important to know how to extend it
with more commands for your particular use case.

First, open up `auxx/src/Command/Proc.hs`, in there you will find
`createCommandProcs`. To add a command, just add another element to this list,
and that's it.

In order to construct such an element, we start with the name of the command:

```
    let name = "my-new-command" in
```

Then we specify dependencies (if any). For instance, if a command needs to
networking, it will have a dependency on `SendActions`:

```
    let name = "my-new-command" in
    needsSendActions name >>= \sendActions ->
```

Then we return a command specification:

```
    let name = "my-new-command" in
    needsSendActions name >>= \sendActions ->
    return CommandProc
    { cpName = name
    , cpArgumentPrepare = _
    , cpArgumentConsumer = _
    , cpExec = _
    , cpHelp = _
    },
```

From here it's just filling the blanks. In `cpHelp` write a short command
description. Don't worry about newline breaks, they will be removed/added and
the text will be left-aligned.

In `cpAurgemntPrepare` feel free to write `identity`. Alternatively, you can
preprocess arguments somehow (for instance, use type information).

In `cpArgumentConsumer` specify all arguments the command needs: `getArg` for
mandatory arguments, `getArgOpt` for optional ones (`Maybe`, denoted `?` in
help), `getArgMany` for zero or more arguments (`[]`, denoted `*` in help), and
`getArgSome` for one or more arguments (`NonEmpty`, denoted `+` in help).

Example:

```
    cpArgumentConsumer = do
        stagpTxsPerThread <- getArg tyInt "txsPerThread"
        stagpConc <- getArg tyInt "conc"
        stagpDelay <- getArg tyInt "delay"
        stagpMode <- getArg tySendMode "mode"
        stagpTpsSentFile <- getArg tyFilePath "file"
        return Tx.SendToAllGenesisParams{..}
```

Notice that we use `ApplicativeDo` to get `do`-notation, but the interface is
`Applicative`. We use the fact that it's only `Applicative` (not `Monad`) to
auto-generate help, much like `optparse-applicative` does.

To `getArg` we pass parameter type (i.e. `tyInt`) and parameter name (i.e.
`"file"`). Then we return some type (`SendToAllGenesisParams` in the last
example), which is specific to this particular command (you can use any
convenient type here).

Since any applicative operation is supported, it's easy to do defaulting with
`<$>` and `fromMaybe`:

```
    puBlockVersionModifier <- fromMaybe def <$> getArgOpt tyBlockVersionModifier "bvm"
```

We can also allow implicit conversions using `tyEither`. For instance, if we
want to accept `StakeholderId` but also implicitly convert `PublicKey` to it,
and also convert `Int` to `PublicKey` to `StakeholderId`, this is what we can
use:

```
    adpStakeholderId <- getArg (tyStakeholderId `tyEither` tyPublicKey `tyEither` tyInt) "s"
```

The end-result will be `StakeholderId`. To define implicit conversions for
other types, add a `ToLeft` instance in the bottom of this module.

Finally, in `cpExec` we write a lambda that takes what `cpArgumentConsumer` has
constructed and then implement the logic of the command:

```
    , cpExec = \addr' -> do
        addr <- toLeft addr'
        balance <- getBalance addr
        return $ ValueNumber (fromIntegral . unsafeGetCoin $ balance)
```

The result of `cpExec` must have type `Value` (from `auxx/src/Lang/Value.hs`),
and this value will be considered the result of this command (when you pass its
result to other commands). Feel free to return `ValueUnit` here if there's
nothing meaningful to return.

NB: when implementing a command in `cpExec`, use `printAction` rather than
`putText`, because it interacts nicely with Haskeline REPL prompt (doesn't
disrupt it).
