{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module System.IO.Overlay
  ( withFile,
    openFile,
    readFile,
    readFile',
    writeFile,
    appendFile,
    withBinaryFile,
    openBinaryFile,
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
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" System.IO (Handle, IO)
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import "this" System.IO.Thin
  ( appendFile,
    openBinaryFile,
    openFile,
    readFile,
    readFile',
    withBinaryFile,
    withFile,
    writeFile,
  )
import "this" System.IO.Thin qualified as Thin

openTempFile ::
  Path 'Abs 'Dir String -> String -> IO (Path 'Abs 'File String, Handle)
openTempFile tmpDir = either throwIO pure <=< Thin.openTempFile @Void tmpDir

openBinaryTempFile ::
  Path 'Abs 'Dir String ->
  String ->
  IO (Path 'Abs 'File String, Handle)
openBinaryTempFile tmpDir = either throwIO pure <=< Thin.openBinaryTempFile @Void tmpDir

openTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir String -> String -> IO (Path 'Abs 'File String, Handle)
openTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openTempFileWithDefaultPermissions @Void tmpDir

openBinaryTempFileWithDefaultPermissions ::
  Path 'Abs 'Dir String ->
  String ->
  IO (Path 'Abs 'File String, Handle)
openBinaryTempFileWithDefaultPermissions tmpDir =
  either throwIO pure <=< Thin.openBinaryTempFileWithDefaultPermissions @Void tmpDir
