## Internal Technical Documentation

Official documentation for Cardano SL can be found at [cardanodocs.com](https://cardanodocs.com/).

The purpose of this directory is to store our **internal** developers-oriented technical documentation.

## Delegation design specification

The design specification for delegation was written using
[LaTeX](https://en.wikipedia.org/wiki/LaTeX). To generate a PDF version of this
document, you can either [install a LaTeX distribution for your
system](https://www.latex-project.org/get/), or use
[Nix](https://nixos.org/nix/) as follows:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch" 
```

