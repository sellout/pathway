{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

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

import "base" Control.Applicative (pure)
import "base" Control.Exception (throwIO)
import "base" Control.Monad ((<=<))
import "base" Data.Either (either)
import "base" Data.Void (Void)
import "base" System.IO (Handle, IO)
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import "filepath" System.OsPath.Types (OsString)
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
import "this" System.File.OsPath.Thin qualified as Thin

openTempFile ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openTempFile tmpDir = either throwIO pure <=< Thin.openTempFile @Void tmpDir

openBinaryTempFile ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openBinaryTempFile tmpDir =
  either throwIO pure <=< Thin.openBinaryTempFile @Void tmpDir

openTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openTempFileWithDefaultPermissions @Void tmpDir

openBinaryTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Path 'Abs 'File OsString, Handle)
openBinaryTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openBinaryTempFileWithDefaultPermissions @Void tmpDir
