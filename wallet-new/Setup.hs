#! /usr/bin/env nix-shell
#! nix-shell ./default.nix -i runghc
import Distribution.Simple
main = defaultMain
