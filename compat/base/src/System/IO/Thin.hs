{-# LANGUAGE Safe #-}

module System.IO.Thin
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
import "base" Control.Category ((.))
import "base" Data.Bitraversable (bitraverse)
import "base" Data.Either (Either)
import "base" Data.Functor (fmap)
import "base" Data.Ord (Ord)
import "base" Data.String (String)
import "base" System.IO (FilePath, Handle, IO, IOMode)
import "base" System.IO qualified as IO
import "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import "this" Common (InternalFailure, absFileFromPathRep, toPathRep)

withFile :: Path 'Abs 'File String -> IOMode -> (Handle -> IO r) -> IO r
withFile = IO.withFile . toPathRep

openFile :: Path 'Abs 'File String -> IOMode -> IO Handle
openFile = IO.openFile . toPathRep

readFile :: Path 'Abs 'File String -> IO String
readFile = IO.readFile . toPathRep

readFile' :: Path 'Abs 'File String -> IO String
readFile' = IO.readFile' . toPathRep

writeFile :: Path 'Abs 'File String -> String -> IO ()
writeFile = IO.writeFile . toPathRep

appendFile :: Path 'Abs 'File String -> String -> IO ()
appendFile = IO.appendFile . toPathRep

withBinaryFile :: Path 'Abs 'File String -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = IO.withBinaryFile . toPathRep

openBinaryFile :: Path 'Abs 'File String -> IOMode -> IO Handle
openBinaryFile = IO.openBinaryFile . toPathRep

openTempFile ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  String ->
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'File String, Handle))
openTempFile tmpDir =
  fmap (bitraverse absFileFromPathRep pure) . IO.openTempFile (toPathRep tmpDir)

openBinaryTempFile ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  String ->
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'File String, Handle))
openBinaryTempFile tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . IO.openBinaryTempFile (toPathRep tmpDir)

openTempFileWithDefaultPermissions ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  String ->
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'File String, Handle))
openTempFileWithDefaultPermissions tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . IO.openTempFileWithDefaultPermissions (toPathRep tmpDir)

openBinaryTempFileWithDefaultPermissions ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  String ->
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'File String, Handle))
openBinaryTempFileWithDefaultPermissions tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . IO.openBinaryTempFileWithDefaultPermissions (toPathRep tmpDir)
