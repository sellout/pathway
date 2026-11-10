# Pathway

[![GitHub CI](https://github.com/sellout/pathway/actions/workflows/build.yml/badge.svg)](https://github.com/sellout/pathway/actions/workflows/build.yml)
[![Nix CI](https://nix-ci.com/badge/gh:sellout:pathway)](https://nix-ci.com/gh:sellout:pathway)
[![Project Manager](https://img.shields.io/badge/%20-Project%20Manager-%235277C3?logo=nixos&labelColor=%23cccccc)](https://sellout.github.io/project-manager/)

Type-safe system-independent file path library

## usage

See the documentation for the individual packages:

- [pathway](./pathway/README.md) – core path operations
- [pathway-quickcheck](./quickcheck/README.md) - utilities for testing Pathway usage using [QuickCheck](https://hackage.haskell.org/package/QuickCheck) (particularly useful for [doctest](https://hackage.haskell.org/package/doctest) usage)
- [pathway-path](./path/README.md) - integration with the [path](https://hackage.haskell.org/package/path) library
- [pathway-system](./system/README.md) - integration with the local filesystem (extremely similar to the [directory](https://hackage.haskell.org/package/directory) package, but with changes to take advantage of the type safety provided by Pathway).

### conditional features

- `OsPath` support (and the `pathway-compat-file-io` package) appears if you are using GHC >= 9.6.1, and
- various operations in `pathway-compat-*` packages appear at the point the appear in the corresponding `*` package.

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

See [the Pathway package documentation](https://hackage.haskell.org/package/pathway#comparisons) for comparisons with a large number of other libraries.
