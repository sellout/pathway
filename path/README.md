# Pathway ↔ `path` integration

[![Packaging status](https://repology.org/badge/tiny-repos/haskell:pathway-path.svg)](https://repology.org/project/haskell:pathway-path/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:pathway-path.svg)](https://repology.org/project/haskell:pathway-path/versions)

[Pathway](https://hackage.haskell.org/package/pathway) isn’t a drop-in replacement for `path`, but it’s very similar in a lot of ways. If anything, Pathway carries the baton from `path`.

This package makes it easier to use Pathway and `path` together.

## usage

## versioning

This project largely follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP), but is more strict in some ways.

The version always has four components, `A.B.C.D`. The first three correspond to those required by PVP, while the fourth matches the “patch” component from [Semantic Versioning](https://semver.org/).

Here is a breakdown of some of the constraints:

### sensitivity to additions to the API

PVP recommends that clients follow [these import guidelines](https://wiki.haskell.org/Import_modules_properly) in order that they may be considered insensitive to additions to the API. However, this isn’t sufficient. We expect clients to follow these additional recommendations for API insensitivity

If you don’t follow these recommendations (in addition to the ones made by PVP), you should ensure your dependencies don’t allow a range of `C` values. That is, your dependencies should look like

```cabal
yaya >=1.2.3 && <1.2.4
```

rather than

```cabal
yaya >=1.2.3 && <1.3
```

#### use package-qualified imports everywhere

If your imports are [package-qualified](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html?highlight=packageimports#extension-PackageImports), then a dependency adding new modules can’t cause a conflict with modules you already import.

#### avoid orphans

Because of the transitivity of instances, orphans make you sensitive to your dependencies’ instances. If you have an orphan instance, you are sensitive to the APIs of the packages that define the class and the types of the instance.

One way to minimize this sensitivity is to have a separate package (or packages) dedicated to any orphans you have. Those packages can be sensitive to their dependencies’ APIs, while the primary package remains insensitive, relying on the tighter ranges of the orphan packages to constrain the solver.

### transitively breaking changes (increments `A`)

#### removing a type class instance

Type class instances are imported transitively, and thus changing them can impact packages that only have your package as a transitive dependency.

#### widening a dependency range with new major versions

This is a consequence of instances being transitively imported. A new major version of a dependency can remove instances, and that can break downstream clients that unwittingly depended on those instances.

A library _may_ declare that it always bumps the `A` component when it removes an instance (as this policy dictates). In that case, only `A` widenings need to induce `A` bumps. `B` widenings can be `D` bumps like other widenings, Alternatively, one may compare the APIs when widening a dependency range, and if no instances have been removed, make it a `D` bump.

### breaking changes (increments `B`)

#### restricting an existing dependency’s version range in any way

Consumers have to contend not only with our version bounds, but also with those of other libraries. It’s possible that some dependency overlapped in a very narrow way, and even just restricting a particular patch version of a dependency could make it impossible to find a dependency solution.

#### restricting the license in any way

Making a license more restrictive may prevent clients from being able to continue using the package.

#### adding a dependency

A new dependency may make it impossible to find a solution in the face of other packages dependency ranges.

### non-breaking changes (increments `C`)

#### adding a module

This is also what PVP recommends. However, unlike in PVP, this is because we recommend that package-qualified imports be used on all imports.

### other changes (increments `D`)

#### widening a dependency range for non-major versions

This is fairly uncommon, in the face of `^>=`-style ranges, but it can happen in a few situations.

#### deprecation

**NB**: This case is _weaker_ than PVP, which indicates that packages should bump their major version when adding `deprecation` pragmas.

We disagree with this because packages shouldn’t be _publishing_ with `-Werror`. The intent of deprecation is to indicate that some API _will_ change. To make that signal a major change itself defeats the purpose. You want people to start seeing that warning as soon as possible. The major change occurs when you actually remove the old API.

Yes, in development, `-Werror` is often (and should be) used. However, that just helps developers be aware of deprecations more immediately. They can always add `-Wwarn=deprecation` in some scope if they need to avoid updating it for the time being.

## licensing

This package is licensed under [The GNU AGPL 3.0 only](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20pathway).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## comparison

Here we talk about how Pathway compares to `path` (trying to minimize bias).

### reparenting (“../”)

`path` doesn’t support this in paths, but Pathway does.

### partiality

`path` has two ways of handling failures. One is making them idempotent. For example, `parent "/" == "/"`. In Pathway, the corresponding operation `ascend [posix|/|]` returns `Nothing`. This is the case with any partial operation in Pathway – there is only one failure case (which usually means walking off the top level of a path somehow), so `Nothing` suffices.

Pathway also partitions functions to minimize `Maybe` as much as possible. For example, `routeRelative` returns in `Maybe`, but `routeAbsolute` doesn’t. Similarly, `(</?>)` returns in `Maybe`, but `(</>)` doesn’t.

There are richer failures possible in parsing that the libraries also handle differently.

### `stripProperPrefix`

This is a function in `path` that given two paths, produces a new path that points to the second argument relative to the first – _if_ the first argument is a prefix of the second.

Pathway has more general operations: `routeAbsolute` and `routeRelative` that will produce a result in many more cases. `path` can’t be as general, because it doesn’t support reparenting, so there’s no way to write a path that’s relative to the first argument _unless_ it’s a proper prefix of the second.

`route*` will produce a result in all cases except when the first argument has more reparenting than the second. See the documentation of `routeRelative` for an explanation.

### file extensions

Pathway has no notion of file extension. It’s just part of the filename. `path` has a number of operations around adding/updating/etc. the file extension.

### systems / formats

Since `path` uses a wrapper over `FilePath`, the distinction between systems is scattered throughout the code, and conditionalized so that the `Path` module represents the current system, while `Path.Posix` and `Path.Windows` are separate modules for when you need them explicitly.

Pathway, however, pushes system-specific notions to the boundaries. All `Path`s are the same. It’s only parsing and printing that cares about the system. There is a `Filesystem.Format.local` that represents the current system.

### drives

Pathway doesn’t care about drives outside of parsing/printing. Paths are all assumed to be part of one filesystem. Supporting broader features like hostnames, drives, URL schemas, would be handled by a consumer of this library.

`path` does support drives. With POSIX, drives are just “/”.

### `QuasiQuoter`s

`path` offers separate `QuasiQuoter`s for `absdir`, `absfile`, `reldir`, and `relfile` for each of `Path`, `Path.Posix`, and `Path.Windows`.

The `Path` ones don’t really make sense, because they will behave differently based on the system that they’re built on, but there are literal paths in the quotes that _don’t_ change based on the system.

So, that’s 12 `QuasiQuoter`s, the most obvious four to reach for don’t work.

Pathway only has two `QuasiQuoter`s – one per format (`posix` and `windows`). The `QuasiQuoter`s parse strictly (for example, with `posix`, no trailing slash indicates that it should be parsed as a file) so it disallows the ambiguities you may need to support if you were to parse a user-supplied path. There is no `local` `QuasiQuoter`, since it would have the same issues as the ones in `path:Path`.
