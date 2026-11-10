{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module System.File.OsPath.Caught
  ( OpenFileFailure,
    openFile,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either)
import safe "base" System.IO (Handle, IO, IOMode)
import safe "exceptions" Control.Monad.Catch (tryJust)
import "filepath" System.OsPath.Types (OsString)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (File))
import safe "pathway-compat-base" System.IO.Caught
  ( OpenFileFailure,
    recoverOpenFileFailure,
  )
import "variant" Data.Variant (V)
import safe "this" System.File.OsPath.Thin qualified as Thin

-- | `System.File.OsPath.openFile`, on `Path`s, with the `IOException`s it’s
--   known to raise returned as variants instead.
--
-- @since 99999
openFile ::
  Path 'Abs 'File OsString -> IOMode -> IO (Either (V OpenFileFailure) Handle)
openFile filePath = tryJust recoverOpenFileFailure . Thin.openFile filePath
