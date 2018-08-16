# Use the build system

Build this article: a one-liner

``` shell
$ nix-shell --run make
```

## Further `make` targets

``` shell
$ make rules.pdf
$ make <name>.pdf
```

Will make `rules.pdf` (resp. `<name>.pdf`)specifically

``` shell
$ make rules
$ make <name>
```

Will run `latexmk` in continuous preview mode. From `latexmk`'s man
page:

> The second previewing option is the powerful -pvc option (mnemonic:
> "preview continuously").  In this case, latexmk runs continuously,
> regularly monitoring all the source files to see if any have
> changed.  Every time a change is detected, latexmk runs all the
> programs necessary to generate a new version of the document.  A
> good previewer (like gv) will then automatically update its display.
> Thus the user can simply edit a file and, when the changes are
> written to disk, latexmk completely automates the cycle of
> updating the .dvi (and possibly the .ps and .pdf) file, and
> refreshing the previewer's display.  It's not quite WYSIWYG, but
> usefully close.

# CDDL spec

The current binary format is specified in `current-spec.cddl`. You can use this
to generate (pseudo-)valid blocks:

```shell
nix-shell
cddl current-spec.cddl generate | diag2pretty.rb > test.pretty
```

By default these are produced in [CBOR Diagnostic
notation](https://tools.ietf.org/html/rfc7049#section-6), so you can use
`diag2pretty.rb` or `diag2cbor.rb` to convert them to a pretty-printed or binary
encoded CBOR format respectively.

The CDDL tools may also be used to validate existing blocks:

```shell
cddl current-spec.cddl validate test.block
```
