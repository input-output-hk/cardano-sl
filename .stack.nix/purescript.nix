{ system
, compiler
, flags ? {}
, pkgs
, hsPkgs
, pkgconfPkgs }:
  let
    _flags = {
      release = false;
    } // flags;
  in ({
    flags = _flags;
    package = {
      specVersion = "0";
      identifier = {
        name = "purescript";
        version = "0.12.0";
      };
      license = "BSD-3-Clause";
      copyright = "(c) 2013-17 Phil Freeman, (c) 2014-17 Gary Burgess";
      maintainer = "Phil Freeman <paf31@cantab.net>";
      author = "Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>, Hardy Jones <jones3.hardy@gmail.com>, Harry Garrood <harry@garrood.me>, Christoph Hegemann <christoph.hegemann1337@gmail.com>";
      homepage = "http://www.purescript.org/";
      url = "";
      synopsis = "PureScript Programming Language Compiler";
      description = "A small strongly, statically typed programming language with expressive types, inspired by Haskell and compiling to JavaScript.";
      buildType = "Simple";
    };
    components = {
      "purescript" = {
        depends  = [
          (hsPkgs.Glob)
          (hsPkgs.aeson)
          (hsPkgs.aeson-better-errors)
          (hsPkgs.ansi-terminal)
          (hsPkgs.base)
          (hsPkgs.base-compat)
          (hsPkgs.blaze-html)
          (hsPkgs.bower-json)
          (hsPkgs.boxes)
          (hsPkgs.bytestring)
          (hsPkgs.cheapskate)
          (hsPkgs.clock)
          (hsPkgs.containers)
          (hsPkgs.data-ordlist)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.dlist)
          (hsPkgs.edit-distance)
          (hsPkgs.file-embed)
          (hsPkgs.filepath)
          (hsPkgs.fsnotify)
          (hsPkgs.haskeline)
          (hsPkgs.language-javascript)
          (hsPkgs.lens)
          (hsPkgs.lifted-base)
          (hsPkgs.monad-control)
          (hsPkgs.monad-logger)
          (hsPkgs.mtl)
          (hsPkgs.parallel)
          (hsPkgs.parsec)
          (hsPkgs.pattern-arrows)
          (hsPkgs.process)
          (hsPkgs.protolude)
          (hsPkgs.regex-tdfa)
          (hsPkgs.safe)
          (hsPkgs.scientific)
          (hsPkgs.semigroups)
          (hsPkgs.sourcemap)
          (hsPkgs.split)
          (hsPkgs.stm)
          (hsPkgs.stringsearch)
          (hsPkgs.syb)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.transformers-base)
          (hsPkgs.transformers-compat)
          (hsPkgs.unordered-containers)
          (hsPkgs.utf8-string)
          (hsPkgs.vector)
        ];
      };
      exes = {
        "purs" = {
          depends  = [
            (hsPkgs.Glob)
            (hsPkgs.aeson)
            (hsPkgs.aeson-better-errors)
            (hsPkgs.ansi-terminal)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.base)
            (hsPkgs.base-compat)
            (hsPkgs.blaze-html)
            (hsPkgs.bower-json)
            (hsPkgs.boxes)
            (hsPkgs.bytestring)
            (hsPkgs.cheapskate)
            (hsPkgs.clock)
            (hsPkgs.containers)
            (hsPkgs.data-ordlist)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.dlist)
            (hsPkgs.edit-distance)
            (hsPkgs.file-embed)
            (hsPkgs.filepath)
            (hsPkgs.fsnotify)
            (hsPkgs.haskeline)
            (hsPkgs.http-types)
            (hsPkgs.language-javascript)
            (hsPkgs.lens)
            (hsPkgs.lifted-base)
            (hsPkgs.monad-control)
            (hsPkgs.monad-logger)
            (hsPkgs.mtl)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.parallel)
            (hsPkgs.parsec)
            (hsPkgs.pattern-arrows)
            (hsPkgs.process)
            (hsPkgs.protolude)
            (hsPkgs.purescript)
            (hsPkgs.regex-tdfa)
            (hsPkgs.safe)
            (hsPkgs.scientific)
            (hsPkgs.semigroups)
            (hsPkgs.sourcemap)
            (hsPkgs.split)
            (hsPkgs.stm)
            (hsPkgs.stringsearch)
            (hsPkgs.syb)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.transformers-base)
            (hsPkgs.transformers-compat)
            (hsPkgs.unordered-containers)
            (hsPkgs.utf8-string)
            (hsPkgs.vector)
            (hsPkgs.wai)
            (hsPkgs.wai-websockets)
            (hsPkgs.warp)
            (hsPkgs.websockets)
          ] ++ pkgs.lib.optional (!_flags.release) (hsPkgs.gitrev);
        };
      };
      tests = {
        "tests" = {
          depends  = [
            (hsPkgs.Glob)
            (hsPkgs.HUnit)
            (hsPkgs.aeson)
            (hsPkgs.aeson-better-errors)
            (hsPkgs.ansi-terminal)
            (hsPkgs.base)
            (hsPkgs.base-compat)
            (hsPkgs.blaze-html)
            (hsPkgs.bower-json)
            (hsPkgs.boxes)
            (hsPkgs.bytestring)
            (hsPkgs.cheapskate)
            (hsPkgs.clock)
            (hsPkgs.containers)
            (hsPkgs.data-ordlist)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.dlist)
            (hsPkgs.edit-distance)
            (hsPkgs.file-embed)
            (hsPkgs.filepath)
            (hsPkgs.fsnotify)
            (hsPkgs.haskeline)
            (hsPkgs.hspec)
            (hsPkgs.hspec-discover)
            (hsPkgs.language-javascript)
            (hsPkgs.lens)
            (hsPkgs.lifted-base)
            (hsPkgs.monad-control)
            (hsPkgs.monad-logger)
            (hsPkgs.mtl)
            (hsPkgs.parallel)
            (hsPkgs.parsec)
            (hsPkgs.pattern-arrows)
            (hsPkgs.process)
            (hsPkgs.protolude)
            (hsPkgs.purescript)
            (hsPkgs.regex-tdfa)
            (hsPkgs.safe)
            (hsPkgs.scientific)
            (hsPkgs.semigroups)
            (hsPkgs.sourcemap)
            (hsPkgs.split)
            (hsPkgs.stm)
            (hsPkgs.stringsearch)
            (hsPkgs.syb)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hspec)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.transformers-base)
            (hsPkgs.transformers-compat)
            (hsPkgs.unordered-containers)
            (hsPkgs.utf8-string)
            (hsPkgs.vector)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript.git";
      rev = "94327051aa311b369a58214a594eb00109912cd8";
      sha256 = "0ckkfhcy8gs2rm7vfd2cvjf7jg7lhcz3a1s6qc0cdn5wcv2a0blx";
    };
  }) // {
    cabal-generator = "hpack";
  }