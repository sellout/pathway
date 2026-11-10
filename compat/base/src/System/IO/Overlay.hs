{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module System.IO.Overlay
  ( openTempFile,
    openBinaryTempFile,
    openTempFileWithDefaultPermissions,
    openBinaryTempFileWithDefaultPermissions,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Exception (throwIO)
import "base" Control.Monad ((<=<))
import "base" Data.Either (either)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" System.IO (Handle, IO)
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import "this" System.IO.Thin qualified as Thin

-- | `System.IO.openTempFile`, on `Path`s, with failures thrown in `IO` (as the
--   original would) rather than returned.
--
-- @since 99999
openTempFile ::
  Path 'Abs 'Dir String -> String -> IO (Path 'Abs 'File String, Handle)
openTempFile tmpDir = either throwIO pure <=< Thin.openTempFile @Void tmpDir

-- | `System.IO.openBinaryTempFile`, on `Path`s, with failures thrown in `IO`
--   (as the original would) rather than returned.
--
-- @since 99999
openBinaryTempFile ::
  Path 'Abs 'Dir String ->
  String ->
  IO (Path 'Abs 'File String, Handle)
openBinaryTempFile tmpDir = either throwIO pure <=< Thin.openBinaryTempFile @Void tmpDir

-- | `System.IO.openTempFileWithDefaultPermissions`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
openTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir String -> String -> IO (Path 'Abs 'File String, Handle)
openTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openTempFileWithDefaultPermissions @Void tmpDir

-- | `System.IO.openBinaryTempFileWithDefaultPermissions`, on `Path`s, with
--   failures thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
openBinaryTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir String ->
  String ->
  IO (Path 'Abs 'File String, Handle)
openBinaryTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openBinaryTempFileWithDefaultPermissions @Void tmpDir
