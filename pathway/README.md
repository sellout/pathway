# Pathway

[![Packaging status](https://repology.org/badge/tiny-repos/haskell:pathway.svg)](https://repology.org/project/haskell:pathway/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:pathway.svg)](https://repology.org/project/haskell:pathway/versions)

Well-typed path manipulation for Haskell

A path library for Haskell, with various type-level guarantees.

## usage

```haskell
let emacsDir = [posix|/usr/share/emacs/|]
 in Path.toText (Format.Windows "C") emacsDir == "C:\\\\usr\\share\\emacs\\"
```

This library offers two primary path types

- `Path relativity typ representation`
- `AmbiguousPath relativity representation`

the parameters can be

- `relativity`
  - `Abs` - an absolute path
  - `Rel False` – a relative path (without `../`)
  - `Rel True` – a “reparented” path (relative, with perhaps some number of `../`)
  - `Any` – either relative or absolute (it is known, but not represented at the type level)
- `type`
  - `Dir` – a directory
  - `File` – a file
  - `Path` – either a directory or file (it is known, but not represented at the type level)
- `representation`
  - an arbitrary type, it is the representation of path components, often some textual type

### `AmbiguousPath`

`AmbiguousPath` doesn’t take a `type` parameter, because it truly doesn’t know whether the type is a file or directory. This can occur when parsing an arbitrary path without a trailing separator. All parsers treat a path with a trailing separator as a directory, but there are three ways to parse paths without a trailing separator: `Parse.directory` will parse everything as a directary, regardless of trailing separator; `Parse.strict` will parse paths without a trailing separator as a file; and `lax` will parse paths without a trailing separator as `AmbiguousPath`.

An `AmbiguousPath` can be resolved to a `Path` in a few ways:

- you can “tell” it in a pure fashion whether it’s a file or directory;
- you can get some parent of it, resulting in a directory; or
- you can query it in a `DirectoryTrie`, which will return a non-ambiguous `Path` if it was found in the trie.

Also, using the `pathway-filesystem` package, you can `disambiguate` it, asking the filesystem what actually exists (if anything).

### `Any`(`Ambiguous`)`Path`

`AnyPath` and `AnyAmbiguousPath` are synonyms over `Path 'Any 'Path` and `AmbiguousPath Any`, respectively.

These types still distinguish between all of the possible cases, but it’s not exposed at the type level. This is useful at the boundary of the system. E.g., the three parsers have approximately the following types:

- `Parser.directory :: rep -> Path Any Dir rep`
- `Parser.strict :: rep -> Path Any Path rep`
- `Parser.lax :: rep -> Either (AmbiguousPath Any rep) (Path Any Dir rep)`

The last case is a bit complicated, but needs to exist that way because 1. some paths can’t be ambiguous (e.g., “/”, “.”, and “..” are all necessarily directories), and then for the ones that do have a trailing “/”, we also preserve that information.

### operations

#### concatenation

There are various ways to concatenate paths.

- `Semigroup`/`Monoid` – `<>` only concatenates `Path Rel Dir rep`
- `</>` concatenates _most_ paths:
  - `Path relativity Dir rep </> Path (Rel False) typ rep -> Path relativity typ rep`
  - `Path (Rel reparented) Dir rep </> Path (Rel reparented') typ rep -> Path (Rel True) typ rep`
  - `Path relativity Dir rep </> AmbiguousPath (Rel False) rep -> AmbiguousPath relativity rep`
  - `Path (Rel reparented) Dir rep </> AmbiguousPath (Rel reparented') rep -> AmbiguousPath (Rel True) rep`
- `</?>` attempts to concatenate a reparented path onto an absolute path (which can fail when there are more `..` in the reparented path then there are components in the absolute path)
  - `Path Abs Dir rep </?> Path (Rel True) typ rep -> Maybe (Path Abs typ rep)`
  - `Path Abs Dir rep </?> AmbiguousPath (Rel True) rep -> Maybe (AmbiguousPath Abs rep)`
- And then there are paths that are just impossible to concatenate:
  - `Path relativity File rep >< Path relativity' typ rep`
  - `Path relativity typ rep >< Path Abs typ rep`
  - `AmbiguousPath relativity rep >< Path relativity' typ rep`

#### routing

“Routing” one path to another results in a relative path that tells how to get to the second path starting from the first. There are various ways to accomplish this, according to the type. Routing is restricted to types with “compatible” relativity. That is – you can route `Abs` to `Abs`, or `Rel a` to `Rel b`. The `Rel a` to `Rel True` case is partial, because if the destination has more `..` than the origin, we don’t have enough context to build the route.

## internals

The `directories` are stored in reverse order, to make the common operations of cons, tail, fold, etc. a bit more intuitive.

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

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20pathway).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## comparisons

Other projects similar to this one, and how they differ.

### [FilePath](https://hackage.haskell.org/package/filepath)

The `filepath` package is included with GHC (and partially exposed via `base` and `Prelude`). It exposes two different path types in `System.FilePath` and `System.OsPath`. Neither one distinguishes between various path types and the behavior for applying operations to the wrong kinds of paths is often invisible.

### [Path](https://hackage.haskell.org/package/path)

This package inspired Pathway, but they differ in a number of ways.

See [the integration package documentation](https://hackage.haskell.org/package/pathway-path) for a detailed comparison.

### [`posix-paths`](https://hackage.haskell.org/package/posix-paths)

This provides a lot of `filepath` operations for `RawFilePath` from the `unix` package (described below). As such, it is POSIX-specifc, and has the other characteristics of that package.

### [`rawfilepath`](https://hackage.haskell.org/package/rawfilepath)

This library isn’t similar to Pathway (but it’s mentioned because it sounds like it is). It provides a number of `IO` operations (e.g., `readFile`, `doesPathExist`, etc.) using the `RawFilePath` type from the `unix` package (described below), so it is also POSIX-specific.

Pathway will eventually support `IO` operations like this, but they will be in a separate package from `pathway` proper. For now, Pathway expects you to convert your paths to another representation before calling existing `IO` operations in other packages.

### [StrongPath](https://hackage.haskell.org/package/strong-path)

This is a wrapper around Path that adds some features. E.g., it allows you to reparent (`../`). It also adds a few other features to the type level that I’m uncertain of.

- You can specify a file type in the type, but it’s simply a phantom type, it doesn’t guarantee anything about the file contents.
- It tracks what a relative path is relative _to_, which I don’t understand, because then why not just prefix the path it’s relative to?
- And it tracks the “standard” (e.g., POSIX or Windows), but Pathway explicitly removes this from the structure of the path in its representation, only using this distinction for parsing and printing.

### [`unix`](https://hackage.haskell.org/package/unix)

Provides `System.Posix.ByteString.RawFilePath` and `System.Posix.PosixString.PosixPath`. Both are type synonyms over some text representation, and are intended to contain POSIX-specific paths.

### in other languages

#### [Paths (for Dhall)](https://github.com/sellout/dhall-path)

A sister library to this one. Being Dhall, it is restricted in ways that Haskell isn’t (e.g., lacking much text manipulation), and that inspired the structure of this library.

#### Pathy ([Purescript](https://github.com/purescript-contrib/purescript-pathy) & [Scala](https://github.com/precog/scala-pathy))

This library was inspired by the [Haskell](https://hackage.haskell.org/package/path) `path` library and the [PureScript](https://github.com/purescript-contrib/purescript-pathy) & [Scala](https://github.com/precog/scala-pathy) `pathy` libraries. However, it differs from them in several ways.

For one, it supports `../` (unlike Haskell’s `path`). Avoiding re-parenting does nothing to eliminate failure cases, it just shifts them to different places. Avoiding escaping some scope can be handled via a “chroot”-style approach, using `encapsulate`.

This library has no special handling of extensions.
