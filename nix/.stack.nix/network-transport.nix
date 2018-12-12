{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.6";
      identifier = {
        name = "network-transport";
        version = "0.5.2";
      };
      license = "BSD-3-Clause";
      copyright = "Well-Typed LLP";
      maintainer = "Facundo Domínguez <facundo.dominguez@tweag.io>";
      author = "Duncan Coutts, Nicolas Wu, Edsko de Vries";
      homepage = "http://haskell-distributed.github.com";
      url = "";
      synopsis = "Network abstraction layer";
      description = "\"Network.Transport\" is a Network Abstraction Layer which provides\nthe following high-level concepts:\n\n* Nodes in the network are represented by 'EndPoint's. These are\nheavyweight stateful objects.\n\n* Each 'EndPoint' has an 'EndPointAddress'.\n\n* Connections can be established from one 'EndPoint' to another\nusing the 'EndPointAddress' of the remote end.\n\n* The 'EndPointAddress' can be serialised and sent over the\nnetwork, where as 'EndPoint's and connections cannot.\n\n* Connections between 'EndPoint's are unidirectional and lightweight.\n\n* Outgoing messages are sent via a 'Connection' object that\nrepresents the sending end of the connection.\n\n* Incoming messages for /all/ of the incoming connections on\nan 'EndPoint' are collected via a shared receive queue.\n\n* In addition to incoming messages, 'EndPoint's are notified of\nother 'Event's such as new connections or broken connections.\n\nThis design was heavily influenced by the design of the Common\nCommunication Interface\n(<http://www.olcf.ornl.gov/center-projects/common-communication-interface>).\nImportant design goals are:\n\n* Connections should be lightweight: it should be no problem to\ncreate thousands of connections between endpoints.\n\n* Error handling is explicit: every function declares as part of\nits type which errors it can return (no exceptions are thrown)\n\n* Error handling is \"abstract\": errors that originate from\nimplementation specific problems (such as \"no more sockets\" in\nthe TCP implementation) get mapped to generic errors\n(\"insufficient resources\") at the Transport level.\n\nThis package provides the generic interface only; you will\nprobably also want to install at least one transport\nimplementation (network-transport-*).";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.hashable)
          (hsPkgs.transformers)
          (hsPkgs.deepseq)
        ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "7.6") (hsPkgs.ghc-prim);
        libs = pkgs.lib.optional (system.isWindows) (pkgs."ws2_32");
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/network-transport";
      rev = "018a50b9042c2115c3ec9c9fd5ca5f28737dd29c";
      sha256 = "0lqa26l2ikpq6a4s7qm9b2favx59w82i0wngakhfyax66fpixp8q";
    };
  }