# How to Contribute

**NOTE**: Most guidelines & justifications are in the [Haskell contributing documentation](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html), this is just the project-specific parts, which should be considered an addendum.

## experimenting in GHCi

You'll need to `import` and `:set` a few things, depending on what you want to experiment with (you might want to look into existing modules to work out what you need exactly). For instance, issuing the following in GHCi,

```
import Data.Path.TH
import qualified Data.Path.Format as Format
```

you'll be able to evaluate the following expression:

```
[windows|\etc\foo\bar\|] :: Data.Path.Path _ _ Text
```

## module patterns

In the `pathway-compat-*` packages, there are a number of module “groups” that look like

```mermaid
graph BT
  pathway(.Pathway) --> caught(.Caught) --> thin(.Thin)
  dropin(.DropIn) --> overlay(.Overlay) --> thin
  pathway --> upstream()
  dropin --> upstream
```

All of the modules (except the upstream one) use Pathway `Path` in place of `FilePath` or `OsPath`.

- `.Thin` is the simplest wrapper to do that, which means some operations end up returning more complex results (due to path parsing issues, etc.)
- `.Overlay` throws as necessary to bring the types back in line with the upstream module (modulo the `Path` types)
- `.DropIn` overlays `.Overlay` on the upstream module to provide a drop-in replacement for the upstream module
- `.Caught` goes the opposite direction of `.Overlay` – reifying raised `IOException`s into variants we can handle, filtered by the cases known to be triggered by each operation.
- `.Pathway` is like `.DropIn`, but it overlays `.Caught` on the upstream module

`.DropIn` is useful for minimizing code changes to adopt Pathway, while `.Pathway` is the safer way to do things (but if you are going to use `.Pathway` from the `pathway-compat-*` packages, then you are probably better off using `pathway-system` instead.`
