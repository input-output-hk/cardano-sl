nix-shell ${haskellScriptDir}shell-with-stylish.nix --run "\
    git diff --diff-filter=AMR --name-only -r HEAD~10 |\
    grep '.hs$' |\
    xargs -I '{}' realpath --relative-to=. $(git rev-parse --show-toplevel)/'{}' |\
    xargs -L 1 stylish-haskell -i"
