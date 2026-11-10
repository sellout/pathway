{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
#if MIN_VERSION_file_io (0, 1, 3)
{-# LANGUAGE TypeApplications #-}
#endif

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
#if MIN_VERSION_file_io(0, 1, 3)
module System.File.OsPath.Overlay
  ( openFile,
    openBinaryFile,
    openExistingFile,
    withFile,
    withBinaryFile,
    withFile',
    withBinaryFile',
    readFile,
    readFile',
    writeFile,
    writeFile',
    appendFile,
    appendFile',
    openTempFile,
    openBinaryTempFile,
    openTempFileWithDefaultPermissions,
    openBinaryTempFileWithDefaultPermissions,
  )
where
#else
module System.File.OsPath.Overlay
  ( openFile,
    openBinaryFile,
    openExistingFile,
    withFile,
    withBinaryFile,
    withFile',
    withBinaryFile',
    readFile,
    readFile',
    writeFile,
    writeFile',
    appendFile,
    appendFile',
  )
where
#endif

import "this" System.File.OsPath.Thin
  ( appendFile,
    appendFile',
    openBinaryFile,
    openExistingFile,
    openFile,
    readFile,
    readFile',
    withBinaryFile,
    withBinaryFile',
    withFile,
    withFile',
    writeFile,
    writeFile',
  )
#if MIN_VERSION_file_io (0, 1, 3)
import "base" Control.Applicative (pure)
import "base" Control.Exception (throwIO)
import "base" Control.Monad ((<=<))
import "base" Data.Either (either)
import "base" Data.Void (Void)
import "base" System.IO (Handle, IO)
import "filepath" System.OsPath.Types (OsString)
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import "this" System.File.OsPath.Thin qualified as Thin

-- | `System.File.OsPath.openTempFile`, on `Path`s, with failures thrown in `IO`
--   (as the original would) rather than returned.
--
-- @since 99999
openTempFile ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openTempFile tmpDir = either throwIO pure <=< Thin.openTempFile @Void tmpDir

-- | `System.File.OsPath.openBinaryTempFile`, on `Path`s, with failures thrown
--   in `IO` (as the original would) rather than returned.
--
-- @since 99999
openBinaryTempFile ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openBinaryTempFile tmpDir =
  either throwIO pure <=< Thin.openBinaryTempFile @Void tmpDir

-- | `System.File.OsPath.openTempFileWithDefaultPermissions`, on `Path`s, with
--   failures thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
openTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openTempFileWithDefaultPermissions @Void tmpDir

-- | `System.File.OsPath.openBinaryTempFileWithDefaultPermissions`, on `Path`s,
--   with failures thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
openBinaryTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openBinaryTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openBinaryTempFileWithDefaultPermissions @Void tmpDir
#endif
