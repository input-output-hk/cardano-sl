# Generated using stack2nix 0.1.3.0.
#
# Only works with sufficiently recent nixpkgs, e.g. "NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/21a8239452adae3a4717772f4e490575586b2755.tar.gz".

{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
, ghc ? pkgs.haskell.compiler.ghc802
}:

with pkgs.haskell.lib;

let
  stackPackages = { callPackage, pkgs, stdenv }:
self: {
      acid-state = callPackage ({ array, base, bytestring, cereal, containers, directory, extensible-exceptions, fetchgit, filepath, mkDerivation, mtl, network, safecopy, stdenv, stm, template-haskell, th-expand-syns, unix }:
      mkDerivation {
          pname = "acid-state";
          version = "0.14.2";
          src = fetchgit {
            url = "https://github.com/serokell/acid-state.git";
            sha256 = "109liqzk66cxkarw8r8jxh27n6qzdcha2xlhsj56xzyqc2aqjz15";
            rev = "95fce1dbada62020a0b2d6aa2dd7e88eadd7214b";
          };
          libraryHaskellDepends = [
            array
            base
            bytestring
            cereal
            containers
            directory
            extensible-exceptions
            filepath
            mtl
            network
            safecopy
            stm
            template-haskell
            th-expand-syns
            unix
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://acid-state.seize.it/";
          description = "Add ACID guarantees to any serializable Haskell data structure";
          license = stdenv.lib.licenses.publicDomain;
        }) {};
      cardano-crypto = callPackage ({ base, bytestring, cryptonite, cryptonite-openssl, deepseq, fetchgit, hashable, memory, mkDerivation, stdenv }:
      mkDerivation {
          pname = "cardano-crypto";
          version = "1.0.0";
          src = fetchgit {
            url = "https://github.com/input-output-hk/cardano-crypto";
            sha256 = "10f89zm2sd015r6fbhlk1zp0720rzq2dvwazrmcxa3bd5s2l696v";
            rev = "1cde8e3a8d9093bbf571085920045c05edb3eaa4";
          };
          libraryHaskellDepends = [
            base
            bytestring
            cryptonite
            cryptonite-openssl
            deepseq
            hashable
            memory
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
          description = "Cryptography primitives for cardano";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-report-server = callPackage ({ aeson, aeson-pretty, base, bytestring, case-insensitive, directory, exceptions, fetchgit, filelock, filepath, formatting, http-types, lens, lifted-base, log-warper, mkDerivation, monad-control, mtl, network, optparse-applicative, parsec, random, stdenv, text, time, transformers, universum, vector, wai, wai-extra, warp }:
      mkDerivation {
          pname = "cardano-report-server";
          version = "0.2.1";
          src = fetchgit {
            url = "https://github.com/input-output-hk/cardano-report-server.git";
            sha256 = "02mf9nw5n0lcq9p6j33lsn0vbab4ai4z3j2099qlzcaqf3kq1987";
            rev = "c2af07ab7d627556ed3f6185b062e4cd1fb5ad26";
          };
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            aeson-pretty
            base
            bytestring
            case-insensitive
            directory
            exceptions
            filelock
            filepath
            formatting
            http-types
            lens
            lifted-base
            log-warper
            monad-control
            mtl
            network
            optparse-applicative
            parsec
            random
            text
            time
            transformers
            universum
            vector
            wai
            wai-extra
            warp
          ];
          executableHaskellDepends = [
            base
            directory
            filepath
            http-types
            log-warper
            monad-control
            mtl
            optparse-applicative
            parsec
            random
            universum
            wai-extra
            warp
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/input-output-hk/cardano-report-server";
          description = "Reporting server for CSL";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      cardano-sl = callPackage ({ MonadRandom, QuickCheck, acid-state, aeson, ansi-terminal, ansi-wl-pprint, async, base, base58-bytestring, base64-bytestring, binary, bytestring, cardano-crypto, cardano-report-server, cardano-sl-core, cardano-sl-db, cardano-sl-godtossing, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, cborg, cereal, conduit, containers, cpphs, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, dlist, dns, ed25519, ekg, ekg-core, ekg-statsd, ether, exceptions, file-embed, filelock, filepath, focus, formatting, generic-arbitrary, hashable, hspec, http-client, http-client-tls, http-conduit, http-types, iproute, kademlia, lens, list-t, log-warper, lrucache, memory, mkDerivation, mmorph, monad-control, monad-loops, mono-traversable, mtl, neat-interpolation, network-info, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, regex-tdfa, regex-tdfa-text, resourcet, rocksdb-haskell, safecopy, serokell-util, servant, servant-multipart, servant-server, stdenv, stm, stm-containers, string-qq, systemd, tagged, template-haskell, text, text-format, th-lift-instances, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers, vector, wai, wai-extra, warp, warp-tls, yaml }:
      mkDerivation {
          pname = "cardano-sl";
          version = "0.5.1";
          src = ./../node;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            acid-state
            aeson
            ansi-terminal
            ansi-wl-pprint
            async
            base
            base58-bytestring
            base64-bytestring
            binary
            bytestring
            cardano-crypto
            cardano-report-server
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            cereal
            conduit
            containers
            cpphs
            cryptonite
            cryptonite-openssl
            data-default
            deepseq
            deriving-compat
            digest
            directory
            dlist
            dns
            ed25519
            ekg
            ekg-core
            ekg-statsd
            ether
            exceptions
            file-embed
            filelock
            filepath
            focus
            formatting
            generic-arbitrary
            hashable
            http-client
            http-client-tls
            http-conduit
            http-types
            iproute
            kademlia
            lens
            list-t
            log-warper
            lrucache
            memory
            mmorph
            monad-control
            monad-loops
            MonadRandom
            mono-traversable
            mtl
            neat-interpolation
            network-info
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            plutus-prototype
            pvss
            QuickCheck
            quickcheck-instances
            random
            reflection
            resourcet
            rocksdb-haskell
            safecopy
            serokell-util
            servant
            servant-multipart
            servant-server
            stm
            stm-containers
            string-qq
            systemd
            tagged
            template-haskell
            text
            text-format
            th-lift-instances
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unix
            unordered-containers
            vector
            wai
            wai-extra
            warp
            warp-tls
            yaml
          ];
          executableHaskellDepends = [
            base
            binary
            bytestring
            cardano-sl-core
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-update
            containers
            data-default
            directory
            ether
            filepath
            formatting
            lens
            log-warper
            mtl
            neat-interpolation
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            serokell-util
            stm-containers
            time
            time-units
            universum
          ];
          testHaskellDepends = [
            base
            bytestring
            cardano-sl-core
            cardano-sl-db
            cardano-sl-godtossing
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            cborg
            cereal
            containers
            cryptonite
            data-default
            ether
            exceptions
            formatting
            generic-arbitrary
            hspec
            kademlia
            lens
            log-warper
            memory
            mmorph
            monad-control
            MonadRandom
            mtl
            node-sketch
            pvss
            QuickCheck
            quickcheck-instances
            random
            reflection
            regex-tdfa
            regex-tdfa-text
            safecopy
            serokell-util
            tagged
            text
            text-format
            time-units
            transformers-base
            universum
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL main implementation";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-core = callPackage ({ QuickCheck, aeson, autoexporter, base, base58-bytestring, binary, bytestring, cardano-crypto, cborg, cereal, concurrent-extra, containers, contravariant, cpphs, cryptonite, cryptonite-openssl, data-default, deepseq, deriving-compat, digest, directory, ed25519, ether, exceptions, file-embed, filepath, formatting, generic-arbitrary, hashable, lens, log-warper, lrucache, memory, mkDerivation, mmorph, mtl, node-sketch, parsec, plutus-prototype, pvss, quickcheck-instances, random, reflection, resourcet, safecopy, semigroups, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, th-utilities, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "cardano-sl-core";
          version = "0.5.1";
          src = ./../core;
          libraryHaskellDepends = [
            aeson
            autoexporter
            base
            base58-bytestring
            binary
            bytestring
            cardano-crypto
            cborg
            cereal
            concurrent-extra
            containers
            contravariant
            cryptonite
            cryptonite-openssl
            data-default
            deepseq
            deriving-compat
            digest
            directory
            ed25519
            ether
            exceptions
            file-embed
            filepath
            formatting
            generic-arbitrary
            hashable
            lens
            log-warper
            lrucache
            memory
            mmorph
            mtl
            node-sketch
            parsec
            plutus-prototype
            pvss
            QuickCheck
            quickcheck-instances
            random
            reflection
            resourcet
            safecopy
            semigroups
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            th-utilities
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unordered-containers
            vector
            yaml
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - core";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-db = callPackage ({ base, bytestring, cardano-sl-core, concurrent-extra, conduit, containers, cpphs, data-default, directory, ether, filepath, formatting, lens, log-warper, mkDerivation, mmorph, monad-control, mtl, node-sketch, resourcet, rocksdb-haskell, serokell-util, stdenv, text-format, transformers, transformers-base, transformers-lift, universum }:
      mkDerivation {
          pname = "cardano-sl-db";
          version = "0.5.1";
          src = ./../db;
          libraryHaskellDepends = [
            base
            bytestring
            cardano-sl-core
            concurrent-extra
            conduit
            containers
            data-default
            directory
            ether
            filepath
            formatting
            lens
            log-warper
            mmorph
            monad-control
            mtl
            node-sketch
            resourcet
            rocksdb-haskell
            serokell-util
            text-format
            transformers
            transformers-base
            transformers-lift
            universum
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - basic DB interfaces";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-explorer = callPackage ({ aeson, base, base16-bytestring, binary, bytestring, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-ssc, cardano-sl-update, containers, cpphs, data-default, either, engine-io, engine-io-wai, ether, exceptions, formatting, http-types, lens, lifted-base, log-warper, memory, mkDerivation, monad-control, monad-loops, mtl, network-transport-tcp, node-sketch, optparse-simple, purescript-bridge, serokell-util, servant, servant-multipart, servant-server, servant-swagger, servant-swagger-ui, socket-io, stdenv, stm, swagger2, tagged, text, text-format, time, time-units, transformers, universum, unordered-containers, wai, wai-cors, warp }:
      mkDerivation {
          pname = "cardano-sl-explorer";
          version = "0.2.0";
          src = ./../explorer;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            base
            base16-bytestring
            binary
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-ssc
            containers
            either
            engine-io
            engine-io-wai
            ether
            exceptions
            formatting
            http-types
            lens
            lifted-base
            log-warper
            memory
            monad-control
            monad-loops
            mtl
            node-sketch
            serokell-util
            servant
            servant-server
            socket-io
            stm
            tagged
            text
            text-format
            time
            time-units
            transformers
            universum
            unordered-containers
            wai
            wai-cors
            warp
          ];
          libraryToolDepends = [ cpphs ];
          executableHaskellDepends = [
            aeson
            base
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-update
            containers
            data-default
            ether
            formatting
            lens
            log-warper
            mtl
            network-transport-tcp
            node-sketch
            optparse-simple
            purescript-bridge
            serokell-util
            servant-multipart
            servant-server
            servant-swagger
            servant-swagger-ui
            swagger2
            text
            time
            time-units
            universum
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano explorer";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-godtossing = callPackage ({ QuickCheck, aeson, array, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, containers, cpphs, cryptonite, data-default, ether, file-embed, formatting, generic-arbitrary, hashable, lens, log-warper, mkDerivation, mmorph, mono-traversable, mtl, node-sketch, random, rocksdb-haskell, serokell-util, stdenv, stm, tagged, text, text-format, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-godtossing";
          version = "0.5.1";
          src = ./../godtossing;
          libraryHaskellDepends = [
            aeson
            array
            base
            bytestring
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            containers
            cryptonite
            data-default
            ether
            file-embed
            formatting
            generic-arbitrary
            hashable
            lens
            log-warper
            mmorph
            mono-traversable
            mtl
            node-sketch
            QuickCheck
            random
            rocksdb-haskell
            serokell-util
            stm
            tagged
            text
            text-format
            time-units
            transformers
            universum
            unordered-containers
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - GodTossing implementation of SSC";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-infra = callPackage ({ QuickCheck, aeson, base, base64-bytestring, binary, bytestring, cardano-report-server, cardano-sl-core, cardano-sl-db, containers, cpphs, data-default, directory, dns, either, ekg-core, ether, exceptions, filepath, formatting, generic-arbitrary, hashable, http-client, http-client-tls, iproute, kademlia, lens, list-t, log-warper, mkDerivation, mmorph, monad-control, mtl, network-info, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, reflection, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers, yaml }:
      mkDerivation {
          pname = "cardano-sl-infra";
          version = "0.5.1";
          src = ./../infra;
          libraryHaskellDepends = [
            aeson
            base
            base64-bytestring
            binary
            bytestring
            cardano-report-server
            cardano-sl-core
            cardano-sl-db
            containers
            data-default
            directory
            dns
            either
            ekg-core
            ether
            exceptions
            filepath
            formatting
            generic-arbitrary
            hashable
            http-client
            http-client-tls
            iproute
            kademlia
            lens
            list-t
            log-warper
            mmorph
            monad-control
            mtl
            network-info
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            QuickCheck
            reflection
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unix
            unordered-containers
            yaml
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - infrastructural";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-lrc = callPackage ({ QuickCheck, base, bytestring, cardano-sl-core, cardano-sl-db, conduit, cpphs, ether, formatting, generic-arbitrary, lens, log-warper, mkDerivation, node-sketch, reflection, stdenv, text-format, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-lrc";
          version = "0.5.1";
          src = ./../lrc;
          libraryHaskellDepends = [
            base
            bytestring
            cardano-sl-core
            cardano-sl-db
            conduit
            ether
            formatting
            generic-arbitrary
            lens
            log-warper
            node-sketch
            QuickCheck
            reflection
            text-format
            universum
            unordered-containers
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - Leaders and Richmen computation";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-lwallet = callPackage ({ QuickCheck, acid-state, ansi-wl-pprint, base, base58-bytestring, binary, bytestring, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-txp, cardano-sl-update, containers, cpphs, data-default, dlist, either, ether, formatting, lens, log-warper, mkDerivation, mmorph, monad-control, monad-loops, mtl, neat-interpolation, network-transport-tcp, node-sketch, optparse-applicative, parsec, random, resourcet, safecopy, serokell-util, stdenv, stm, stm-containers, tagged, text, time, time-units, transformers, transformers-base, transformers-lift, universum, unix, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-lwallet";
          version = "0.5.1";
          src = ./../lwallet;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            acid-state
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-txp
            cardano-sl-update
            containers
            data-default
            dlist
            either
            ether
            formatting
            lens
            log-warper
            mmorph
            monad-control
            monad-loops
            mtl
            neat-interpolation
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            QuickCheck
            random
            resourcet
            safecopy
            serokell-util
            stm
            stm-containers
            tagged
            text
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unix
            unordered-containers
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - Light wallet";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-ssc = callPackage ({ QuickCheck, aeson, base, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cpphs, cryptonite, data-default, ether, exceptions, formatting, lens, log-warper, memory, mkDerivation, mmorph, mtl, node-sketch, parsec, serokell-util, stdenv, stm, tagged, text-format, universum }:
      mkDerivation {
          pname = "cardano-sl-ssc";
          version = "0.5.1";
          src = ./../ssc;
          libraryHaskellDepends = [
            aeson
            base
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cryptonite
            data-default
            ether
            exceptions
            formatting
            lens
            log-warper
            memory
            mmorph
            mtl
            node-sketch
            parsec
            QuickCheck
            serokell-util
            stm
            tagged
            text-format
            universum
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - the SSC class";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-tools = callPackage ({ Chart, Chart-diagrams, Glob, MonadRandom, QuickCheck, aeson, ansi-wl-pprint, array, async, attoparsec, base, bytestring, cardano-report-server, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, cardano-sl-ssc, cardano-sl-txp, containers, cpphs, cryptonite, data-default, directory, ed25519, ether, fgl, filepath, foldl, formatting, graphviz, kademlia, lens, log-warper, mkDerivation, mtl, neat-interpolation, node-sketch, optparse-applicative, parsec, pipes, pipes-bytestring, pipes-interleave, pipes-safe, process, random, random-shuffle, serokell-util, stdenv, stm, system-filepath, tar, text, text-format, time, time-units, universum, unix-compat, unordered-containers, vector }:
      mkDerivation {
          pname = "cardano-sl-tools";
          version = "0.5.1";
          src = ./../tools;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            aeson
            ansi-wl-pprint
            array
            async
            attoparsec
            base
            bytestring
            cardano-report-server
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            cardano-sl-ssc
            cardano-sl-txp
            Chart
            Chart-diagrams
            containers
            cryptonite
            data-default
            directory
            ed25519
            ether
            fgl
            filepath
            foldl
            formatting
            Glob
            graphviz
            kademlia
            lens
            log-warper
            MonadRandom
            mtl
            neat-interpolation
            node-sketch
            optparse-applicative
            parsec
            pipes
            pipes-bytestring
            pipes-interleave
            pipes-safe
            process
            QuickCheck
            random
            random-shuffle
            serokell-util
            stm
            system-filepath
            tar
            text
            text-format
            time
            time-units
            universum
            unix-compat
            unordered-containers
            vector
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - Tools";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-txp = callPackage ({ QuickCheck, aeson, base, bytestring, cardano-sl-core, cardano-sl-db, cardano-sl-infra, conduit, containers, cpphs, data-default, ekg-core, ether, formatting, generic-arbitrary, hashable, lens, lifted-base, log-warper, memory, mkDerivation, monad-control, mtl, neat-interpolation, node-sketch, plutus-prototype, resourcet, rocksdb-haskell, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, time-units, transformers, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "cardano-sl-txp";
          version = "0.5.1";
          src = ./../txp;
          libraryHaskellDepends = [
            aeson
            base
            bytestring
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            conduit
            containers
            data-default
            ekg-core
            ether
            formatting
            generic-arbitrary
            hashable
            lens
            lifted-base
            log-warper
            memory
            monad-control
            mtl
            neat-interpolation
            node-sketch
            plutus-prototype
            QuickCheck
            resourcet
            rocksdb-haskell
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            time-units
            transformers
            universum
            unordered-containers
            vector
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - transaction processing";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-update = callPackage ({ QuickCheck, aeson, base, binary, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-lrc, concurrent-extra, conduit, containers, cpphs, data-default, ether, exceptions, formatting, generic-arbitrary, hashable, lens, log-warper, mkDerivation, mtl, node-sketch, parsec, resourcet, rocksdb-haskell, safecopy, serokell-util, stdenv, stm, tagged, template-haskell, text, text-format, th-lift-instances, time-units, transformers, universum, unordered-containers }:
      mkDerivation {
          pname = "cardano-sl-update";
          version = "0.5.1";
          src = ./../update;
          libraryHaskellDepends = [
            aeson
            base
            binary
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-lrc
            concurrent-extra
            conduit
            containers
            data-default
            ether
            exceptions
            formatting
            generic-arbitrary
            hashable
            lens
            log-warper
            mtl
            node-sketch
            parsec
            QuickCheck
            resourcet
            rocksdb-haskell
            safecopy
            serokell-util
            stm
            tagged
            template-haskell
            text
            text-format
            th-lift-instances
            time-units
            transformers
            universum
            unordered-containers
          ];
          libraryToolDepends = [ cpphs ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - update";
          license = stdenv.lib.licenses.mit;
        }) {};
      cardano-sl-wallet = callPackage ({ aeson, ansi-wl-pprint, base, base58-bytestring, binary, bytestring, cardano-report-server, cardano-sl, cardano-sl-core, cardano-sl-db, cardano-sl-infra, cardano-sl-ssc, cardano-sl-txp, cardano-sl-update, containers, cpphs, data-default, directory, dlist, ether, exceptions, filepath, formatting, lens, log-warper, mkDerivation, mtl, network-transport, network-transport-tcp, node-sketch, optparse-applicative, parsec, purescript-bridge, random, semver, serokell-util, servant, servant-multipart, servant-server, servant-swagger, servant-swagger-ui, stdenv, stm, stm-containers, string-qq, swagger2, text, text-format, time, time-units, transformers, universum, unix, unordered-containers, wai, wai-websockets, websockets }:
      mkDerivation {
          pname = "cardano-sl-wallet";
          version = "0.5.1";
          src = ./../wallet;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            cardano-report-server
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            containers
            data-default
            directory
            dlist
            ether
            exceptions
            filepath
            formatting
            lens
            log-warper
            mtl
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            random
            semver
            serokell-util
            servant
            servant-multipart
            servant-server
            stm
            stm-containers
            string-qq
            text-format
            time
            time-units
            transformers
            universum
            unix
            unordered-containers
            wai
            wai-websockets
            websockets
          ];
          libraryToolDepends = [ cpphs ];
          executableHaskellDepends = [
            aeson
            ansi-wl-pprint
            base
            base58-bytestring
            binary
            bytestring
            cardano-sl
            cardano-sl-core
            cardano-sl-db
            cardano-sl-infra
            cardano-sl-ssc
            cardano-sl-txp
            cardano-sl-update
            containers
            data-default
            directory
            dlist
            ether
            exceptions
            filepath
            formatting
            lens
            log-warper
            mtl
            network-transport
            network-transport-tcp
            node-sketch
            optparse-applicative
            parsec
            purescript-bridge
            random
            serokell-util
            servant
            servant-multipart
            servant-server
            servant-swagger
            servant-swagger-ui
            stm
            stm-containers
            string-qq
            swagger2
            text
            text-format
            time
            time-units
            transformers
            universum
            unordered-containers
            wai
            wai-websockets
            websockets
          ];
          executableToolDepends = [
            cpphs
          ];
          doHaddock = false;
          doCheck = true;
          description = "Cardano SL - wallet";
          license = stdenv.lib.licenses.mit;
        }) {};
      ed25519 = callPackage ({ base, bytestring, fetchgit, ghc-prim, mkDerivation, stdenv }:
      mkDerivation {
          pname = "ed25519";
          version = "0.0.5.0";
          src = fetchgit {
            url = "https://github.com/thoughtpolice/hs-ed25519";
            sha256 = "0fah4vkmqdkjsdh3s3x27yfaif2fbdg6049xvp54b5mh50yvxkfq";
            rev = "da4247b5b3420120e20451e6a252e2a2ca15b43c";
          };
          libraryHaskellDepends = [
            base
            bytestring
            ghc-prim
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://thoughtpolice.github.com/hs-ed25519";
          description = "Ed25519 cryptographic signatures";
          license = stdenv.lib.licenses.mit;
        }) {};
      engine-io = callPackage ({ aeson, async, attoparsec, base, base64-bytestring, bytestring, either, fetchgit, free, mkDerivation, monad-loops, mwc-random, stdenv, stm, stm-delay, text, transformers, unordered-containers, vector, websockets }:
      mkDerivation {
          pname = "engine-io";
          version = "1.2.15";
          src = fetchgit {
            url = "https://github.com/serokell/engine.io.git";
            sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
            rev = "a594e402fd450f11ad60d09ddbd93db500000632";
          };
          postUnpack = "sourceRoot+=/engine-io; echo source root reset to \$sourceRoot";
          libraryHaskellDepends = [
            aeson
            async
            attoparsec
            base
            base64-bytestring
            bytestring
            either
            free
            monad-loops
            mwc-random
            stm
            stm-delay
            text
            transformers
            unordered-containers
            vector
            websockets
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ocharles/engine.io";
          description = "A Haskell implementation of Engine.IO";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      engine-io-wai = callPackage ({ attoparsec, base, bytestring, either, engine-io, fetchgit, http-types, mkDerivation, mtl, stdenv, text, transformers, transformers-compat, unordered-containers, wai, wai-websockets, websockets }:
      mkDerivation {
          pname = "engine-io-wai";
          version = "1.0.6";
          src = fetchgit {
            url = "https://github.com/serokell/engine.io.git";
            sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
            rev = "a594e402fd450f11ad60d09ddbd93db500000632";
          };
          postUnpack = "sourceRoot+=/engine-io-wai; echo source root reset to \$sourceRoot";
          libraryHaskellDepends = [
            attoparsec
            base
            bytestring
            either
            engine-io
            http-types
            mtl
            text
            transformers
            transformers-compat
            unordered-containers
            wai
            wai-websockets
            websockets
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ocharles/engine.io";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      kademlia = callPackage ({ MonadRandom, base, binary, bytestring, containers, contravariant, cryptonite, data-default, extra, fetchgit, memory, mkDerivation, mtl, network, random, random-shuffle, stdenv, stm, time, transformers, transformers-compat }:
      mkDerivation {
          pname = "kademlia";
          version = "1.1.0.1";
          src = fetchgit {
            url = "https://github.com/serokell/kademlia.git";
            sha256 = "1k1wp9dwhzzqfivxc28vhxfqplnyh916crr7bhsiv829d6qifhw1";
            rev = "7120bb4d28e708acd52dfd61d3dca7914fac7d7f";
          };
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base
            bytestring
            containers
            contravariant
            cryptonite
            extra
            memory
            MonadRandom
            mtl
            network
            random
            random-shuffle
            stm
            time
            transformers
          ];
          executableHaskellDepends = [
            base
            binary
            bytestring
            containers
            data-default
            extra
            MonadRandom
            mtl
            network
            random
            random-shuffle
            transformers
            transformers-compat
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/serokell/kademlia";
          description = "An implementation of the Kademlia DHT Protocol";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      network-transport = callPackage ({ base, binary, bytestring, deepseq, fetchgit, hashable, mkDerivation, stdenv, transformers }:
      mkDerivation {
          pname = "network-transport";
          version = "0.5.2";
          src = fetchgit {
            url = "https://github.com/serokell/network-transport";
            sha256 = "0lqa26l2ikpq6a4s7qm9b2favx59w82i0wngakhfyax66fpixp8q";
            rev = "018a50b9042c2115c3ec9c9fd5ca5f28737dd29c";
          };
          libraryHaskellDepends = [
            base
            binary
            bytestring
            deepseq
            hashable
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-distributed.github.com";
          description = "Network abstraction layer";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      network-transport-tcp = callPackage ({ base, bytestring, containers, data-accessor, fetchgit, mkDerivation, network, network-transport, stdenv, uuid }:
      mkDerivation {
          pname = "network-transport-tcp";
          version = "0.5.1";
          src = fetchgit {
            url = "https://github.com/serokell/network-transport-tcp";
            sha256 = "1l4df0wgaixslah2c05wvq3srdbw1rmq246889wn3r8h43l9i0wl";
            rev = "24dd213cef81d383083feb7f5d9ce4b8a6be8168";
          };
          libraryHaskellDepends = [
            base
            bytestring
            containers
            data-accessor
            network
            network-transport
            uuid
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://haskell-distributed.github.com";
          description = "TCP instantiation of Network.Transport";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      node-sketch = callPackage ({ aeson, async, attoparsec, base, binary, bytestring, containers, cryptonite, data-default, deepseq, ekg, ekg-core, exceptions, fetchgit, formatting, hashable, kademlia, lens, lifted-base, log-warper, mkDerivation, mmorph, monad-control, mtl, mwc-random, network, network-transport, network-transport-tcp, random, resourcet, semigroups, serokell-util, statistics, stdenv, stm, tagged, text, text-format, time, time-units, transformers, transformers-base, transformers-lift, universum, unordered-containers, vector }:
      mkDerivation {
          pname = "node-sketch";
          version = "0.2.0.0";
          src = fetchgit {
            url = "https://github.com/serokell/time-warp-nt.git";
            sha256 = "0gwr1n6mp6n70mwgczf8pblq1l2085nmyll5wl19dg1ic0dnmbh2";
            rev = "40f4235cda65c746c6edc2a3455242d96d7175bf";
          };
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            aeson
            async
            attoparsec
            base
            binary
            bytestring
            containers
            cryptonite
            data-default
            deepseq
            ekg
            ekg-core
            exceptions
            formatting
            hashable
            kademlia
            lens
            lifted-base
            log-warper
            mmorph
            monad-control
            mtl
            mwc-random
            network
            network-transport
            network-transport-tcp
            random
            resourcet
            semigroups
            serokell-util
            statistics
            stm
            tagged
            text
            text-format
            time
            time-units
            transformers
            transformers-base
            transformers-lift
            universum
            unordered-containers
            vector
          ];
          executableHaskellDepends = [
            base
            binary
            bytestring
            containers
            network-transport-tcp
            random
            stm
            time-units
          ];
          doHaddock = false;
          doCheck = false;
          license = stdenv.lib.licenses.mit;
        }) {};
      plutus-prototype = callPackage ({ base, bifunctors, binary, bytestring, cardano-crypto, cryptonite, ed25519, either, fetchgit, filepath, lens, memory, mkDerivation, mtl, operational, parsec, stdenv, transformers }:
      mkDerivation {
          pname = "plutus-prototype";
          version = "0.1.0.0";
          src = fetchgit {
            url = "https://github.com/input-output-hk/plutus-prototype";
            sha256 = "1b0c9d8pr932fvaamyv53fa2jpfwm249imc8fxfybn71yz8p96ai";
            rev = "d4aa461fc69fc6957aab46b41a670c2144aefb77";
          };
          libraryHaskellDepends = [
            base
            bifunctors
            binary
            bytestring
            cardano-crypto
            cryptonite
            ed25519
            either
            filepath
            lens
            memory
            mtl
            operational
            parsec
            transformers
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "iohk.io";
          description = "Prototype of the Plutus language";
          license = stdenv.lib.licenses.mit;
        }) {};
      socket-io = callPackage ({ aeson, attoparsec, base, bytestring, engine-io, fetchgit, mkDerivation, mtl, stdenv, stm, text, transformers, unordered-containers, vector }:
      mkDerivation {
          pname = "socket-io";
          version = "1.3.7";
          src = fetchgit {
            url = "https://github.com/serokell/engine.io.git";
            sha256 = "0j2rxbw5g88ivmjzhmhnxk4cgkxdw97i2qlzw47gzyv56ciqfdny";
            rev = "a594e402fd450f11ad60d09ddbd93db500000632";
          };
          postUnpack = "sourceRoot+=/socket-io; echo source root reset to \$sourceRoot";
          libraryHaskellDepends = [
            aeson
            attoparsec
            base
            bytestring
            engine-io
            mtl
            stm
            text
            transformers
            unordered-containers
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/ocharles/engine.io";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      swagger2 = callPackage ({ Cabal, aeson, base, base-compat, bytestring, cabal-doctest, containers, fetchgit, generics-sop, hashable, http-media, insert-ordered-containers, lens, mkDerivation, mtl, network, scientific, stdenv, template-haskell, text, time, transformers, transformers-compat, unordered-containers, uuid-types, vector }:
      mkDerivation {
          pname = "swagger2";
          version = "2.1.4.1";
          src = fetchgit {
            url = "https://github.com/serokell/swagger2";
            sha256 = "1pavnbzpx5ybdwc7ridjz3mljmcwjlakrr7mpdkgl2dliyrrwj6d";
            rev = "6693ff91be2f0b015c2ab043292587c1cc9449cf";
          };
          setupHaskellDepends = [
            base
            Cabal
            cabal-doctest
          ];
          libraryHaskellDepends = [
            aeson
            base
            base-compat
            bytestring
            containers
            generics-sop
            hashable
            http-media
            insert-ordered-containers
            lens
            mtl
            network
            scientific
            template-haskell
            text
            time
            transformers
            transformers-compat
            unordered-containers
            uuid-types
            vector
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/GetShopTV/swagger2";
          description = "Swagger 2.0 data model";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      time-units = callPackage ({ base, fetchgit, mkDerivation, stdenv }:
      mkDerivation {
          pname = "time-units";
          version = "1.0.0";
          src = fetchgit {
            url = "https://github.com/serokell/time-units.git";
            sha256 = "0psdr1if0rgnn24698x3583m0603rwd3sd7yb9whj03hskmkwpgs";
            rev = "6c3747c1ac794f952de996dd7ba8a2f6d63bf132";
          };
          libraryHaskellDepends = [
            base
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "http://github.com/acw/time-units";
          description = "A basic library for defining units of time as types";
          license = stdenv.lib.licenses.bsd3;
        }) {};
    };
in
compiler.override {
  initialPackages = stackPackages;
  configurationCommon = { ... }: self: super: {};
}

