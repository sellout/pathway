# Pathway system integration

[![Hackage Version](https://img.shields.io/hackage/v/pathway-system)](https://hackage.haskell.org/package/pathway-system)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:pathway-system.svg)](https://repology.org/project/haskell:pathway-system/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:pathway-system.svg)](https://repology.org/project/haskell:pathway-system/versions)

This provides filesystem integration for [Pathway](https://hackage.haskell.org/package/pathway).

## usage

- [comparisons to similar projects](#comparisons)

For each underlying module, there are three modules here:

1. <underlying>`.Thin` – minimal wrapper to convert to Pathway types;
2. <underlying>`.Overlay` – `.Thin` with types aligned as closely as possible with the underlying package (this often means converting failures into `IO` or `Maybe`); and
3. <underlying>`.Pathway` – `.Overlay` combined with the underlying module, so that this is a drop-in replacement for the underlying module.

If you are migrating to Pathway, the quickest route is to add `.Pathway` to the modules that are provided here, ignoring the `.Thin` and `.Overlay` modules.

The `.Thin` modules are useful when providing other wrappers (see [pathway-system]() for an example of this).

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20pathway).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

Other projects similar to this one, and how they differ.

### [directory](https://hackage.haskell.org/package/directory) and [path-io](https://hackage.haskell.org/package/path-io)

These packages are both similar to this one. They all roughly provide the same API, but taking the type of path provided by the library.

One way in which Pathway differs is that it requires absolute paths for almost all operations. This is a philosophical point, in which it’s much harder to apply operations in the wrong place if you must explicitly build the absolute path before passing it to the filesystem.

Some operations are exceptions, returning a relative path if that path is relative to some argument of the operation[^1]. It’s also allowable for an operation to take a relative path if, say, it’s going to search the filesystem for absolute paths containing that suffix.

[^1]: This is because it’s much easier (total) to prepend the argument to the relative result if you want to make the results absolute than is to strip the argument from the absolute result if you want to make it relative (partial).
