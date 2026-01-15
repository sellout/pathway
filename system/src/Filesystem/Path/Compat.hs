{-# LANGUAGE Trustworthy #-}
-- "System.Directory" has inconsistent Safe Haskell modes across versions. We
-- can’t conditionalize the Safe Haskell extension (because it forces Safe
-- Haskell-using consumers to conditionalize), so this silences the fact that
-- this module is inferred ‘Safe’ in some configurations.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2025 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Operations that don’t fit with the philosophy of this library, but that help
-- with interoperability with other code.
module Filesystem.Path.Compat
  ( withCurrentDirectory,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either)
import safe "base" System.IO (IO)
import "directory" System.Directory qualified as Dir
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import safe "transformers" Control.Monad.Trans.Class (lift)
import safe "transformers" Control.Monad.Trans.Except (ExceptT)
import safe "this" Filesystem.Path qualified as Path
import safe "this" Filesystem.Path.Internal (PathComponent, toPathRep)

-- | Instead of working with the “current” directory, you should pass explicit
--   absolute paths. However, for compatibility with other code that relies on
--   the “current” directory, this is made available so that it can be scoped
--   around such calls.
--
--  __NB__: Like the underlying `Dir.withCurrentDirectory`, this is _not_ thread-safe.
withCurrentDirectory ::
  Path 'Abs 'Dir PathComponent ->
  IO a ->
  ExceptT (Either Path.GetFailure Path.SetFailure) IO a
withCurrentDirectory newCurDir = lift . Dir.withCurrentDirectory (toPathRep newCurDir)
