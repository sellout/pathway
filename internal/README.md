# Pathway internals

[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fpathway)](https://garnix.io)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:pathway-internal.svg)](https://repology.org/project/haskell:pathway-internal/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:pathway-internal.svg)](https://repology.org/project/haskell:pathway-internal/versions)

Implementation details for [Pathway](https://hackage.haskell.org/package/pathway) that generally shouldn’t need to be depended on directly.

It’s extracted into its own package to allow [Pathway QuickCheck integration](https://hackage.haskell.org/package/pathway-quickcheck) to not depend on Pathway itself (avoiding a dependency cycle on Pathway’s tests).

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20pathway-internal).

You should review the [license report](docs/license-report.md) for details about dependency licenses.
