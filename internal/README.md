# Pathway internals

[![Hackage Version](https://img.shields.io/hackage/v/pathway-internal)](https://hackage.haskell.org/package/pathway-internal)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:pathway-internal.svg)](https://repology.org/project/haskell:pathway-internal/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:pathway-internal.svg)](https://repology.org/project/haskell:pathway-internal/versions)

Implementation details for [Pathway](https://hackage.haskell.org/package/pathway) that generally shouldn’t need to be depended on directly.

It’s extracted into its own package to allow [Pathway QuickCheck integration](https://hackage.haskell.org/package/pathway-quickcheck) to not depend on Pathway itself (avoiding a dependency cycle on Pathway’s tests).

## usage

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20pathway).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)
