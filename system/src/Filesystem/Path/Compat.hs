{-# LANGUAGE Trustworthy #-}

-- | Operations that don’t fit with the philosophy of this library, but that
-- help with interoperability with other code.
module Filesystem.Path.Compat
  ( withCurrentDirectory,
  )
where

import safe "base" Data.Either (Either)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.String (String)
import safe "base" System.IO (IO)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import safe "pathway-compat-directory" System.Directory.Caught qualified as F.Caught
import safe "pathway-compat-directory" System.Directory.Error
  ( SetCurrentDirectoryFailure,
  )
import safe "pathway-compat-directory" System.Directory.OsPath.Caught qualified as O.Caught
import safe "pathway-compat-directory" System.IO.Error (FullError)
import safe "pathway-compat-filepath" System.OsPath.Pathway (OsString)
import "variant" Data.Variant (V)

type Rep :: Kind.Type -> Kind.Constraint
class Rep rep where
  -- | Instead of working with the dynamically-scoped “current” directory, you
  --   should pass explicit absolute paths. However, for compatibility with other
  --   code that relies on the “current” directory, this is made available so that
  --   it can be scoped around such calls.
  --
  --  __NB__: Like the underlying `System.Directory.withCurrentDirectory`, this is
  --          /not/ thread-safe.
  withCurrentDirectory ::
    Path 'Abs 'Dir rep ->
    IO a ->
    IO (Either (V (FullError ': SetCurrentDirectoryFailure)) a)

instance Rep String where
  withCurrentDirectory = F.Caught.withCurrentDirectory

instance Rep OsString where
  withCurrentDirectory = O.Caught.withCurrentDirectory
