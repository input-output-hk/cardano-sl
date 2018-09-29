self:
  {
    lib_acid-state-exts = {
      src = ./acid-state-exts/src;
      dependencies = [
        "acid-state"
        "base"
        "directory"
        "exceptions"
        "extra"
        "filepath"
        "mtl"
        "safecopy"
        "time-units"
      ];
      extensions = [];
      packages = [];
    };
    lib_cardano-sl-binary = {
      src = ./binary/src;
      dependencies = [
        "aeson"
        "base"
        "binary"
        "bytestring"
        "canonical-json"
        "cborg"
        "cereal"
        "containers"
        "digest"
        "formatting"
        "hashable"
        "lens"
        "micro-recursion-schemes"
        "safe-exceptions"
        "safecopy"
        "serokell-util"
        "tagged"
        "template-haskell"
        "text"
        "th-utilities"
        "time-units"
        "universum"
        "unordered-containers"
        "vector"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-binary-test = {
      src = ./binary/test/.;
      dependencies = [
        "QuickCheck"
        "base"
        "bytestring"
        "cborg"
        "cereal"
        "containers"
        "cryptonite"
        "formatting"
        "half"
        "hedgehog"
        "hspec"
        "mtl"
        "pretty-show"
        "quickcheck-instances"
        "safecopy"
        "serokell-util"
        "text"
        "universum"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-util-test)
      ];
    };
    lib_cardano-sl-chain = {
      src = ./chain/src;
      dependencies = [
        "Cabal"
        "aeson"
        "aeson-options"
        "array"
        "base"
        "bytestring"
        "canonical-json"
        "cborg"
        "cereal"
        "conduit"
        "containers"
        "cryptonite"
        "data-default"
        "deepseq"
        "ekg-core"
        "ether"
        "exceptions"
        "extra"
        "filepath"
        "fmt"
        "formatting"
        "free"
        "hashable"
        "lens"
        "lrucache"
        "memory"
        "mmorph"
        "mono-traversable"
        "mtl"
        "neat-interpolation"
        "parsec"
        "plutus-prototype"
        "reflection"
        "safe-exceptions"
        "safecopy"
        "serokell-util"
        "template-haskell"
        "text"
        "time"
        "time-units"
        "transformers"
        "universum"
        "unordered-containers"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-chain-test = {
      src = ./chain/test/.;
      dependencies = [
        "QuickCheck"
        "base"
        "base16-bytestring"
        "bytestring"
        "cardano-crypto"
        "containers"
        "data-default"
        "formatting"
        "generic-arbitrary"
        "hedgehog"
        "pvss"
        "random"
        "reflection"
        "serokell-util"
        "time-units"
        "universum"
        "unordered-containers"
        "vector"
      ];
      extensions = [
        "NoImplicitPrelude"
        "OverloadedStrings"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-binary-test)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-crypto-test)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-util-test)
      ];
    };
    lib_cardano-sl-client = {
      src = ./client/src;
      dependencies = [
        "QuickCheck"
        "base"
        "containers"
        "data-default"
        "formatting"
        "lens"
        "mtl"
        "safe-exceptions"
        "serokell-util"
        "stm"
        "transformers"
        "universum"
        "unordered-containers"
        "vector"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-core = {
      src = ./core/src;
      dependencies = [
        "Cabal"
        "aeson"
        "aeson-options"
        "ansi-terminal"
        "base"
        "base58-bytestring"
        "bytestring"
        "canonical-json"
        "cardano-report-server"
        "cborg"
        "cereal"
        "containers"
        "cryptonite"
        "data-default"
        "deepseq"
        "deriving-compat"
        "ekg-core"
        "ether"
        "exceptions"
        "extra"
        "filepath"
        "fmt"
        "formatting"
        "hashable"
        "lens"
        "memory"
        "mmorph"
        "monad-control"
        "mtl"
        "parsec"
        "plutus-prototype"
        "random"
        "reflection"
        "resourcet"
        "safe-exceptions"
        "safecopy"
        "serokell-util"
        "servant"
        "stm"
        "template-haskell"
        "text"
        "th-lift-instances"
        "time"
        "time-units"
        "transformers"
        "transformers-base"
        "transformers-lift"
        "universum"
        "unliftio"
        "unliftio-core"
        "unordered-containers"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "InstanceSigs"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "RankNTypes"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "TypeFamilies"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-core-test = {
      src = ./core/test/.;
      dependencies = [
        "QuickCheck"
        "base"
        "bytestring"
        "cardano-crypto"
        "containers"
        "cryptonite"
        "generic-arbitrary"
        "hedgehog"
        "pvss"
        "quickcheck-instances"
        "random"
        "serokell-util"
        "text"
        "time-units"
        "universum"
        "unordered-containers"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-binary-test)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-crypto-test)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-util-test)
      ];
    };
    lib_cardano-sl-crypto = {
      src = ./crypto/.;
      dependencies = [
        "aeson"
        "base"
        "binary"
        "bytestring"
        "canonical-json"
        "cardano-crypto"
        "cborg"
        "cereal"
        "cryptonite"
        "cryptonite-openssl"
        "data-default"
        "formatting"
        "hashable"
        "lens"
        "memory"
        "mtl"
        "pvss"
        "reflection"
        "safe-exceptions"
        "safecopy"
        "scrypt"
        "serokell-util"
        "text"
        "universum"
        "unordered-containers"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-crypto-test = {
      src = ./crypto/test/.;
      dependencies = [
        "QuickCheck"
        "base"
        "bytestring"
        "cardano-crypto"
        "cryptonite"
        "generic-arbitrary"
        "hedgehog"
        "memory"
        "quickcheck-instances"
        "universum"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-binary-test)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-util-test)
      ];
    };
    lib_cardano-sl-db = {
      src = ./db/src;
      dependencies = [
        "aeson"
        "base"
        "binary"
        "bytestring"
        "concurrent-extra"
        "conduit"
        "containers"
        "cryptonite"
        "data-default"
        "directory"
        "ekg-core"
        "ether"
        "exceptions"
        "filepath"
        "formatting"
        "lens"
        "lrucache"
        "memory"
        "mmorph"
        "mtl"
        "resourcet"
        "rocksdb-haskell-ng"
        "safe-exceptions"
        "serokell-util"
        "stm"
        "tagged"
        "text"
        "time-units"
        "transformers"
        "universum"
        "unliftio"
        "unordered-containers"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-db-test = {
      src = ./db/test/.;
      dependencies = [
        "QuickCheck"
        "base"
        "generic-arbitrary"
        "universum"
        "unordered-containers"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-crypto-test)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-util-test)
      ];
    };
    exe_cardano-faucet = {
      src = ./faucet/server;
      dependencies = [
        "aeson"
        "base"
        "bytestring"
        "ekg"
        "ekg-core"
        "ekg-statsd"
        "exceptions"
        "lens"
        "log-warper"
        "mmorph"
        "mtl"
        "optparse-applicative"
        "safe-exceptions"
        "servant"
        "servant-client"
        "servant-server"
        "text"
        "universum"
        "wai"
        "wai-cors"
        "wai-extra"
        "warp"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-faucet)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-wallet)
        (self.lib_cardano-sl-wallet-new)
      ];
      main = "Main";
    };
    lib_cardano-sl-faucet = {
      src = ./faucet/src;
      dependencies = [
        "QuickCheck"
        "aeson"
        "aeson-pretty"
        "base"
        "base16-bytestring"
        "bytestring"
        "connection"
        "cryptonite"
        "data-default"
        "directory"
        "ekg-core"
        "ekg-statsd"
        "exceptions"
        "filepath"
        "formatting"
        "generic-arbitrary"
        "http-api-data"
        "http-client"
        "http-client-tls"
        "http-types"
        "lens"
        "log-warper"
        "memory"
        "mmorph"
        "mtl"
        "neat-interpolation"
        "random"
        "safe-exceptions"
        "serokell-util"
        "servant"
        "servant-client"
        "servant-client-core"
        "servant-server"
        "servant-swagger"
        "servant-swagger-ui"
        "stm"
        "swagger2"
        "tagged"
        "text"
        "time"
        "tls"
        "universum"
        "wai"
        "wai-app-static"
        "wreq"
      ];
      extensions = [
        "NoImplicitPrelude"
      ];
      packages = [
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-wallet)
        (self.lib_cardano-sl-wallet-new)
      ];
    };
    exe_cardano-sl-verification-bench-exe = {
      src = ./generator/app;
      dependencies = [
        "MonadRandom"
        "base"
        "bytestring"
        "containers"
        "cryptonite"
        "deepseq"
        "directory"
        "formatting"
        "optparse-applicative"
        "random"
        "text"
        "time-units"
        "universum"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-generator)
        (self.lib_cardano-sl-util)
      ];
      main = "VerificationBench";
    };
    lib_cardano-sl-generator = {
      src = ./generator/src;
      dependencies = [
        "MonadRandom"
        "QuickCheck"
        "base"
        "bytestring"
        "containers"
        "cryptonite"
        "data-default"
        "ether"
        "exceptions"
        "formatting"
        "lens"
        "monad-control"
        "random"
        "safe-exceptions"
        "serokell-util"
        "text"
        "time-units"
        "transformers-base"
        "universum"
        "unliftio"
        "unordered-containers"
        "vector"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-client)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-infra = {
      src = ./infra/src;
      dependencies = [
        "aeson"
        "async"
        "base"
        "base64-bytestring"
        "bytestring"
        "cardano-report-server"
        "clock"
        "conduit"
        "containers"
        "directory"
        "dns"
        "ekg-core"
        "ekg-statsd"
        "ekg-wai"
        "ether"
        "exceptions"
        "filepath"
        "formatting"
        "hashable"
        "http-client"
        "http-client-tls"
        "iproute"
        "kademlia"
        "lens"
        "lzma-conduit"
        "mtl"
        "network-info"
        "network-transport"
        "network-transport-tcp"
        "optparse-applicative"
        "parsec"
        "safe-exceptions"
        "serokell-util"
        "stm"
        "tagged"
        "tar"
        "text"
        "time"
        "time-units"
        "universum"
        "unliftio"
        "unordered-containers"
        "yaml"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-infra-test = {
      src = ./infra/test/.;
      dependencies = [
        "QuickCheck"
        "async"
        "base"
        "bytestring"
        "containers"
        "dns"
        "generic-arbitrary"
        "hedgehog"
        "hspec"
        "iproute"
        "kademlia"
        "universum"
      ];
      extensions = [
        "NoImplicitPrelude"
      ];
      packages = [
        (self.lib_cardano-sl-binary-test)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-crypto-test)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-util-test)
      ];
    };
    lib_cardano-sl = {
      src = ./lib/src;
      dependencies = [
        "QuickCheck"
        "aeson"
        "aeson-options"
        "ansi-terminal"
        "ansi-wl-pprint"
        "async"
        "base"
        "bytestring"
        "canonical-json"
        "cborg"
        "conduit"
        "constraints"
        "containers"
        "contravariant"
        "cpphs"
        "cryptonite"
        "data-default"
        "directory"
        "ekg-core"
        "ether"
        "exceptions"
        "filelock"
        "filepath"
        "formatting"
        "generic-arbitrary"
        "hspec"
        "http-client"
        "http-client-tls"
        "http-conduit"
        "http-types"
        "lens"
        "lifted-async"
        "memory"
        "mmorph"
        "monad-control"
        "mtl"
        "neat-interpolation"
        "network"
        "network-transport"
        "optparse-applicative"
        "parsec"
        "pvss"
        "random"
        "reflection"
        "safe-exceptions"
        "serokell-util"
        "servant"
        "servant-client"
        "servant-client-core"
        "servant-server"
        "servant-swagger"
        "stm"
        "streaming-commons"
        "tagged"
        "template-haskell"
        "text"
        "time"
        "time-units"
        "tls"
        "transformers"
        "universum"
        "unliftio"
        "unordered-containers"
        "wai"
        "warp"
        "warp-tls"
        "x509"
        "x509-store"
        "x509-validation"
        "yaml"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-binary-test)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-crypto-test)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-util)
      ];
    };
    exe_discovery = {
      src = ./networking/examples;
      dependencies = [
        "base"
        "binary"
        "bytestring"
        "containers"
        "contravariant"
        "network-transport"
        "network-transport-tcp"
        "random"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-util)
      ];
      main = "Discovery";
    };
    exe_ping-pong = {
      src = ./networking/examples;
      dependencies = [
        "async"
        "base"
        "binary"
        "bytestring"
        "contravariant"
        "network-transport"
        "network-transport-tcp"
        "random"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-util)
      ];
      main = "PingPong";
    };
    exe_bench-sender = {
      src = ./networking/bench/Sender;
      dependencies = [
        "MonadRandom"
        "async"
        "base"
        "contravariant"
        "lens"
        "mtl"
        "network-transport"
        "network-transport-tcp"
        "optparse-simple"
        "random"
        "safe-exceptions"
        "serokell-util"
        "time"
        "time-units"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-util)
      ];
      main = "Main";
    };
    exe_bench-receiver = {
      src = ./networking/bench/Receiver;
      dependencies = [
        "base"
        "contravariant"
        "network-transport-tcp"
        "optparse-simple"
        "random"
        "safe-exceptions"
        "serokell-util"
        "text"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-util)
      ];
      main = "Main";
    };
    exe_bench-log-reader = {
      src = ./networking/bench/LogReader;
      dependencies = [
        "attoparsec"
        "base"
        "conduit"
        "conduit-extra"
        "containers"
        "formatting"
        "lens"
        "mtl"
        "optparse-simple"
        "resourcet"
        "safe-exceptions"
        "text"
        "unliftio-core"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-util)
      ];
      main = "Main";
    };
    lib_cardano-sl-networking = {
      src = ./networking/src;
      dependencies = [
        "aeson"
        "aeson-options"
        "async"
        "attoparsec"
        "base"
        "binary"
        "bytestring"
        "containers"
        "ekg-core"
        "formatting"
        "hashable"
        "kademlia"
        "lens"
        "mtl"
        "network"
        "network-transport"
        "network-transport-tcp"
        "random"
        "safe-exceptions"
        "scientific"
        "stm"
        "text"
        "these"
        "time"
        "time-units"
        "universum"
        "unordered-containers"
      ];
      extensions = [
        "DeriveDataTypeable"
        "DeriveGeneric"
        "GeneralizedNewtypeDeriving"
        "MonadFailDesugaring"
        "OverloadedStrings"
      ];
      packages = [
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-node-ipc = {
      src = ./node-ipc/src;
      dependencies = [
        "Cabal"
        "aeson"
        "base"
        "binary"
        "bytestring"
        "mtl"
        "universum"
      ];
      extensions = [
        "NoImplicitPrelude"
      ];
      packages = [
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-util)
      ];
    };
    exe_dbgen = {
      src = ./tools/src/dbgen;
      dependencies = [
        "base"
        "containers"
        "data-default"
        "network-transport-tcp"
        "optparse-generic"
        "stm"
        "text"
        "time"
        "time-units"
        "universum"
        "unordered-containers"
      ];
      extensions = [];
      packages = [
        (self.lib_acid-state-exts)
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-client)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-tools)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-wallet)
      ];
      main = "Main";
    };
    exe_cardano-genupdate = {
      src = ./tools/src/genupdate;
      dependencies = [
        "ansi-wl-pprint"
        "base"
        "bytestring"
        "cryptonite"
        "filepath"
        "formatting"
        "neat-interpolation"
        "optparse-applicative"
        "process"
        "tar"
        "text"
        "universum"
        "unix-compat"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-util)
      ];
      main = "Main";
    };
    exe_cardano-keygen = {
      src = ./tools/src/keygen;
      dependencies = [];
      extensions = [];
      packages = [];
      main = "Main";
    };
    exe_cardano-launcher = {
      src = ./tools/src/launcher;
      dependencies = [
        "aeson"
        "aeson-options"
        "ansi-wl-pprint"
        "async"
        "base"
        "bytestring"
        "cardano-report-server"
        "directory"
        "filepath"
        "formatting"
        "lens"
        "lifted-async"
        "neat-interpolation"
        "optparse-applicative"
        "process"
        "safe-exceptions"
        "serokell-util"
        "silently"
        "text"
        "time-units"
        "universum"
        "unordered-containers"
        "yaml"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-tools)
        (self.lib_cardano-sl-util)
      ];
      main = "Main";
    };
    exe_cardano-addr-convert = {
      src = ./tools/src/addr-convert;
      dependencies = [
        "ansi-wl-pprint"
        "base"
        "neat-interpolation"
        "optparse-applicative"
        "text"
        "universum"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-util)
      ];
      main = "Main";
    };
    exe_cardano-cli-docs = {
      src = ./tools/src/cli-docs;
      dependencies = [
        "base"
        "directory"
        "filepath"
        "neat-interpolation"
        "optparse-applicative"
        "process"
        "text"
        "universum"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl)
      ];
      main = "Main";
    };
    exe_cardano-post-mortem = {
      src = ./tools/src/post-mortem;
      dependencies = [];
      extensions = [];
      packages = [];
      main = "Main";
    };
    exe_cardano-blockchain-analyser = {
      src = ./tools/src/blockchain-analyser;
      dependencies = [];
      extensions = [];
      packages = [];
      main = "Main";
    };
    exe_cardano-x509-certificates = {
      src = ./tools/src/gencerts;
      dependencies = [
        "base"
        "filepath"
        "optparse-applicative"
        "universum"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-x509)
      ];
      main = "Main";
    };
    exe_genesis-hash = {
      src = ./tools/src/genesis-hash;
      dependencies = [
        "base"
        "bytestring"
        "canonical-json"
        "cryptonite"
        "universum"
      ];
      extensions = [];
      packages = [];
      main = "Main";
    };
    lib_cardano-sl-tools = {
      src = ./tools/src;
      dependencies = [
        "QuickCheck"
        "aeson"
        "ansi-terminal"
        "base"
        "bytestring"
        "containers"
        "data-default"
        "directory"
        "filepath"
        "network-transport-tcp"
        "optparse-applicative"
        "optparse-generic"
        "parsers"
        "stm"
        "string-conv"
        "text"
        "time"
        "time-units"
        "trifecta"
        "universum"
        "unordered-containers"
      ];
      extensions = [
        "NoImplicitPrelude"
      ];
      packages = [
        (self.lib_acid-state-exts)
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-client)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-wallet)
      ];
    };
    lib_cardano-sl-util = {
      src = ./util/src;
      dependencies = [
        "aeson"
        "auto-update"
        "base"
        "canonical-json"
        "cborg"
        "cereal"
        "concurrent-extra"
        "containers"
        "contravariant"
        "cryptonite"
        "deepseq"
        "directory"
        "ether"
        "exceptions"
        "file-embed"
        "filepath"
        "formatting"
        "hashable"
        "katip"
        "lens"
        "lrucache"
        "megaparsec"
        "mmorph"
        "monad-control"
        "mtl"
        "optparse-applicative"
        "parsec"
        "process"
        "reflection"
        "resourcet"
        "safe-exceptions"
        "serokell-util"
        "stm"
        "tagged"
        "template-haskell"
        "text"
        "time"
        "time-units"
        "transformers"
        "transformers-base"
        "transformers-lift"
        "universum"
        "unliftio-core"
        "unordered-containers"
        "yaml"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [];
    };
    lib_cardano-sl-util-test = {
      src = ./util/test/.;
      dependencies = [
        "QuickCheck"
        "aeson"
        "attoparsec"
        "base"
        "base16-bytestring"
        "bytestring"
        "cryptonite"
        "directory"
        "file-embed"
        "filepath"
        "formatting"
        "hedgehog"
        "hspec"
        "mtl"
        "pretty-show"
        "quickcheck-instances"
        "tagged"
        "template-haskell"
        "text"
        "time-units"
        "universum"
        "unordered-containers"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-utxo = {
      src = ./utxo/src;
      dependencies = [
        "QuickCheck"
        "base"
        "cardano-crypto"
        "constraints"
        "containers"
        "cryptonite"
        "data-default"
        "formatting"
        "lens"
        "log-warper"
        "mtl"
        "reflection"
        "safe-exceptions"
        "safecopy"
        "serokell-util"
        "universum"
        "unordered-containers"
        "vector"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "GADTs"
        "InstanceSigs"
        "LambdaCase"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "RankNTypes"
        "RecordWildCards"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TypeFamilies"
      ];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-client)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-crypto-test)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-util)
      ];
    };
    exe_cardano-node = {
      src = ./wallet-new/server;
      dependencies = [
        "base"
        "stm"
        "text"
        "universum"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-wallet)
        (self.lib_cardano-sl-wallet-new)
      ];
      main = "Main";
    };
    exe_cardano-generate-swagger-file = {
      src = ./wallet-new/generate-swagger-file;
      dependencies = [
        "aeson"
        "base"
        "bytestring"
        "optparse-applicative"
        "servant-server"
        "swagger2"
        "universum"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-wallet-new)
      ];
      main = "Main";
    };
    exe_wal-integr-test = {
      src = ./wallet-new/integration;
      dependencies = [
        "QuickCheck"
        "aeson"
        "aeson-diff"
        "aeson-pretty"
        "async"
        "base"
        "bytestring"
        "containers"
        "exceptions"
        "formatting"
        "hspec"
        "http-client"
        "http-types"
        "lens"
        "mtl"
        "optparse-applicative"
        "pretty-show"
        "servant"
        "servant-client"
        "servant-quickcheck"
        "servant-server"
        "text"
        "universum"
        "x509-store"
      ];
      extensions = [];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-wallet)
        (self.lib_cardano-sl-wallet-new)
      ];
      main = "Main";
    };
    lib_cardano-sl-wallet-new = {
      src = ./wallet-new/src;
      dependencies = [
        "QuickCheck"
        "acid-state"
        "aeson"
        "aeson-options"
        "aeson-pretty"
        "async"
        "base"
        "base58-bytestring"
        "beam-core"
        "beam-migrate"
        "beam-sqlite"
        "bifunctors"
        "bytestring"
        "cardano-crypto"
        "cereal"
        "conduit"
        "connection"
        "containers"
        "cryptonite"
        "data-default"
        "data-default-class"
        "directory"
        "exceptions"
        "filepath"
        "foldl"
        "formatting"
        "generics-sop"
        "http-api-data"
        "http-client"
        "http-client-tls"
        "http-types"
        "ixset-typed"
        "lens"
        "memory"
        "mtl"
        "mwc-random"
        "neat-interpolation"
        "network-transport"
        "optparse-applicative"
        "reflection"
        "resourcet"
        "retry"
        "safe-exceptions"
        "safecopy"
        "serokell-util"
        "servant"
        "servant-client"
        "servant-client-core"
        "servant-server"
        "servant-swagger"
        "servant-swagger-ui"
        "servant-swagger-ui-core"
        "servant-swagger-ui-redoc"
        "sqlite-simple"
        "sqlite-simple-errors"
        "stm"
        "swagger2"
        "text"
        "time"
        "time-units"
        "tls"
        "transformers"
        "universum"
        "unliftio"
        "unliftio-core"
        "unordered-containers"
        "vector"
        "wai"
        "wai-middleware-throttle"
        "warp"
        "x509"
        "x509-store"
      ];
      extensions = [
        "DataKinds"
        "DefaultSignatures"
        "FlexibleContexts"
        "FlexibleInstances"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "RecordWildCards"
        "ScopedTypeVariables"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "TypeFamilies"
        "TypeOperators"
        "UndecidableInstances"
      ];
      packages = [
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-binary)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-client)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-node-ipc)
        (self.lib_cardano-sl-util)
        (self.lib_cardano-sl-utxo)
        (self.lib_cardano-sl-wallet)
        (self.lib_cardano-sl-x509)
      ];
    };
    lib_cardano-sl-wallet = {
      src = ./wallet/src;
      dependencies = [
        "QuickCheck"
        "acid-state"
        "aeson"
        "async"
        "base"
        "base58-bytestring"
        "basement"
        "bytestring"
        "cardano-crypto"
        "containers"
        "cryptonite"
        "data-default"
        "directory"
        "dlist"
        "ekg-core"
        "ether"
        "exceptions"
        "filepath"
        "formatting"
        "hashable"
        "hspec"
        "lens"
        "memory"
        "monad-control"
        "mtl"
        "random"
        "reflection"
        "safe-exceptions"
        "safecopy"
        "semver"
        "serokell-util"
        "servant"
        "servant-generic"
        "servant-multipart"
        "servant-server"
        "servant-swagger"
        "servant-swagger-ui"
        "stm"
        "swagger2"
        "text"
        "time"
        "time-units"
        "transformers"
        "universum"
        "unliftio"
        "unordered-containers"
        "wai"
        "wai-websockets"
        "warp"
        "websockets"
      ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveGeneric"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "LambdaCase"
        "MonadFailDesugaring"
        "MultiParamTypeClasses"
        "MultiWayIf"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "RecordWildCards"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "TypeOperators"
        "UndecidableInstances"
        "ViewPatterns"
      ];
      packages = [
        (self.lib_acid-state-exts)
        (self.lib_cardano-sl)
        (self.lib_cardano-sl-chain)
        (self.lib_cardano-sl-chain-test)
        (self.lib_cardano-sl-client)
        (self.lib_cardano-sl-core)
        (self.lib_cardano-sl-core-test)
        (self.lib_cardano-sl-crypto)
        (self.lib_cardano-sl-db)
        (self.lib_cardano-sl-generator)
        (self.lib_cardano-sl-infra)
        (self.lib_cardano-sl-networking)
        (self.lib_cardano-sl-node-ipc)
        (self.lib_cardano-sl-util)
      ];
    };
    lib_cardano-sl-x509 = {
      src = ./x509/src;
      dependencies = [
        "aeson"
        "asn1-encoding"
        "asn1-types"
        "base"
        "base64-bytestring"
        "bytestring"
        "cryptonite"
        "data-default-class"
        "filepath"
        "hourglass"
        "ip"
        "optparse-applicative"
        "text"
        "universum"
        "unordered-containers"
        "x509"
        "x509-store"
        "x509-validation"
        "yaml"
      ];
      extensions = [
        "DeriveGeneric"
        "NoImplicitPrelude"
        "OverloadedStrings"
        "TupleSections"
        "TypeApplications"
      ];
      packages = [];
    };
  }