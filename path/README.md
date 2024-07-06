# Pathway ↔ `path` integration

[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fpathway)](https://garnix.io)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:pathway-path.svg)](https://repology.org/project/haskell:pathway-path/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:pathway-path.svg)](https://repology.org/project/haskell:pathway-path/versions)

[Pathway](https://hackage.haskell.org/package/pathway) is not a drop-in replacement for `path`, but it is very similar in a lot of ways. If anything, Pathway carries the baton from `path`.

This package makes it easier to use Pathway and `path` together.

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20pathway-path).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## comparison

Here we talk about how Pathway compares to `path` (trying to minimize bias).

### reparenting (“../”)

`path` doesn’t support this in paths, but Pathway does.

### partiality

`path` has two ways of handling failures. One is making them idempotent. E.g. `parent "/" == "/"`. In Pathway, the corresponding operation `ascend [posix|/|]` returns `Nothing`. This is the case with any partial operation in Pathway – there is only one failure case (which usually means walking off the top level of a path somehow), so `Nothing` sufficies.

Pathway also partitions functions to minimize `Maybe` as much as possible. E.g., `routeRelative` returns in `Maybe`, but `routeAbsolute` doesn’t. Similarly, `(</?>)` returns in `Maybe`, but `(</>)` doesn’t.

There are richer failures possible in parsing that the libraries also handle differently.

### `stripProperPrefix`

This is a function in `path` that given two paths, produces a new path that points to the second argument relative to the first – _if_ the first argument is a prefix of the second.

Pathway has more general operations: `routeAbsolute` and `routeRelative` that will produce a result in many more cases. `path` can’t be as general, because it doesn’t support reparenting, so there’s no way to write a path that is relative to the first argument _unless_ it is a proper prefix of the second.

`route*` will produce a result in all cases except when the first argument has more reparenting than the second. See the documentation of `routeRelative` for an explanation.

### file extensions

Pathway has no notion of file extension. It is just part of the filename. `path` has a number of operations around adding/updating/etc. the file extension.

### systems / formats

Since `path` uses a wrapper over `FilePath`, the distinction between systems is scattered throughout the code, and conditionalized so that the `Path` module represents the current system, while `Path.Posix` and `Path.Windows` are separate modules for when you need them explicitly.

Pathway, however, pushes system-specific notions to the boundaries. All `Path`s are the same. It is only parsing and printing that cares about the system. There is a `Filesystem.Format.local` that represents the current system.

### drives

Pathway doesn’t care about drives outside of parsing/printing. Paths are all assumed to be part of one filesystem. Supporting broader features like hostnames, drives, URL schemas, would be handled by a consumer of this library.

`path` does support drives. With POSIX, drives are just “/”.

### `QuasiQuoter`s

`path` offers separate `QuasiQuoter`s for `absdir`, `absfile`, `reldir`, and `relfile` for each of `Path`, `Path.Posix`, and `Path.Windows`.

The `Path` ones don’t really make sense, because they will behave differently based on the system that they are built on, but there are literal paths in the quotes that _don’t_ change based on the system.

So, that’s 12 `QuasiQuoter`s, the most obvious four to reach for don’t work.

Pathway only has two `QuasiQuoter`s – one per format (`posix` and `windows`). The `QuasiQuoter`s parse strictly (e.g., with `posix`, no trailing slash indicates that it should be parsed as a file) so it disallows the ambiguities you may need to support if you were to parse a user-supplied path. There is no `local` `QuasiQuoter`, since it would have the same issues as the ones in `path:Path`.
