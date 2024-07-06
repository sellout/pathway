# Pathway system integration

[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fpathway)](https://garnix.io)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:pathway-system.svg)](https://repology.org/project/haskell:pathway-system/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:pathway-system.svg)](https://repology.org/project/haskell:pathway-system/versions)

This provides filesystem integration for [Pathway](https://hackage.haskell.org/package/pathway).

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20pathway-system).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## comparisons

Other projects similar to this one, and how they differ.

### [directory](https://hackage.haskell.org/package/directory) and [path-io](https://hackage.haskell.org/package/path-io)

These packages are both very similar to this one. They all roughly provide the same API, but taking the type of path provided by the library.

One way in which Pathway differs is that it requires absolute paths for almost all operations. This is a philosophical point, in which it is much harder to apply operations in the wrong place if you must explicitly build the absolute path before passing it to the filesystem.

There are exceptions in that some operations return a relative path, if that path is relative to some argument of the operation[^1]. It is also allowable for an operation to take a relative path if, say, it is going to search the filesystem for absolute paths containing that suffix.

[^1]: This is because it is much easier (total) to prepend the argument to the relative result if you want to make the results absolute than is to strip the argument from the absolute result if you want to make it relative (partial).
