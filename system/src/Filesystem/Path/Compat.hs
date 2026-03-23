{-# LANGUAGE Trustworthy #-}

-- | Operations that don’t fit with the philosophy of this library, but that
-- help with interoperability with other code.
module Filesystem.Path.Compat
  ( withCurrentDirectory,
  )
where

import safe "base" Data.Either (Either)
import safe "base" System.IO (IO)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import safe "pathway-compat-directory" System.Directory.Caught qualified as Dir
import "variant" Data.Variant (V)
import safe "this" Filesystem.Path.Internal (PathComponent)

-- | Instead of working with the dynamically-scoped “current” directory, you
--   should pass explicit absolute paths. However, for compatibility with other
--   code that relies on the “current” directory, this is made available so that
--   it can be scoped around such calls.
--
--  __NB__: Like the underlying `System.Directory.withCurrentDirectory`, this is
--          /not/ thread-safe.
withCurrentDirectory ::
  Path 'Abs 'Dir PathComponent ->
  IO a ->
  IO (Either (V (Dir.FullError ': Dir.SetCurrentDirectoryFailure)) a)
withCurrentDirectory = Dir.withCurrentDirectory
