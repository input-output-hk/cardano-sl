{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {
      test-parsing = false;
    } // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.14";
        identifier = {
          name = "graphviz";
          version = "2999.19.0.0";
        };
        license = "BSD-3-Clause";
        copyright = "Matthew Sackman, Ivan Lazar Miljenovic";
        maintainer = "Ivan.Miljenovic@gmail.com";
        author = "Matthew Sackman, Ivan Lazar Miljenovic";
        homepage = "http://projects.haskell.org/graphviz/";
        url = "";
        synopsis = "Bindings to Graphviz for graph visualisation.";
        description = "This library provides bindings for the Dot language used by the\nGraphviz (<http://graphviz.org/>) suite of programs for visualising\ngraphs, as well as functions to call those programs.\n\nMain features of the graphviz library include:\n\n* Almost complete coverage of all Graphviz attributes and syntax.\n\n* Support for specifying clusters.\n\n* The ability to use a custom node type.\n\n* Functions for running a Graphviz layout tool with all specified\noutput types.\n\n* The ability to not only generate but also parse Dot code with two\noptions: strict and liberal (in terms of ordering of statements).\n\n* Functions to convert FGL graphs and other graph-like data structures\nto Dot code - including support to group them into clusters - with a\nhigh degree of customisation by specifying which attributes to use\nand limited support for the inverse operation.\n\n* Round-trip support for passing an FGL graph through Graphviz to\naugment node and edge labels with positional information, etc.";
        buildType = "Simple";
      };
      components = {
        graphviz = {
          depends  = [
            hsPkgs.base
            hsPkgs.containers
            hsPkgs.process
            hsPkgs.directory
            hsPkgs.temporary
            hsPkgs.fgl
            hsPkgs.filepath
            hsPkgs.polyparse
            hsPkgs.bytestring
            hsPkgs.colour
            hsPkgs.transformers
            hsPkgs.text
            hsPkgs.wl-pprint-text
            hsPkgs.dlist
          ];
        };
        exes = {
          graphviz-testparsing = {
            depends  = [
              hsPkgs.base
              hsPkgs.graphviz
              hsPkgs.bytestring
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.text
            ];
          };
        };
        tests = {
          graphviz-testsuite = {
            depends  = [
              hsPkgs.base
              hsPkgs.graphviz
              hsPkgs.containers
              hsPkgs.fgl
              hsPkgs.fgl-arbitrary
              hsPkgs.filepath
              hsPkgs.text
              hsPkgs.QuickCheck
            ];
          };
        };
        benchmarks = {
          graphviz-printparse = {
            depends  = [
              hsPkgs.base
              hsPkgs.deepseq
              hsPkgs.text
              hsPkgs.graphviz
              hsPkgs.criterion
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/ivan-m/graphviz.git";
        rev = "b7539aa832f2a66a4bb4b6538cefe704ffd7d21a";
        sha256 = null;
      };
    }