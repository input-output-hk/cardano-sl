{ ... }@args:

let
  cardanoPkgs = import ../.. args;
  frontend = cardanoPkgs.cardano-sl-explorer-frontend;

in
  # fixme: cardano-sl-explorer source is not filtered enough, so
  # generating files in frontend will cause a rebuild of explorer
  # backend. You will just have to wait a little wait to get a shell.

  frontend.overrideAttrs (oldAttrs: {

    shellHook = ''
      help() {
        echo "*** To regenerate purescript code, run \`regen'."
        echo
        echo "*** To build, run \`yarn build:prod'."
        echo "*** For dev, run \`yarn server:dev'."
        echo
        echo "*** To see this message again, run \`help'."
      }

      echo; echo; help

      export PATH=$(pwd)/node_modules/.bin:$PATH
    '';

  })
