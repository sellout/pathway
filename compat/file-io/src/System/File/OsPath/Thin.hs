{-# LANGUAGE Trustworthy #-}

module System.File.OsPath.Thin
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

    -- * utils
    fromPathRep,
    handleAnchoredPath,
    absFileFromPathRep,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bitraversable (bitraverse)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Ord (Ord)
import safe "base" System.IO (Handle, IO, IOMode)
import "bytestring" Data.ByteString qualified as Strict (ByteString)
import "bytestring" Data.ByteString.Lazy qualified as Lazy (ByteString)
import "file-io" System.File.OsPath qualified as FIO
import "filepath" System.OsPath.Types (OsPath, OsString)
import "megaparsec" Text.Megaparsec qualified as MP
import safe "pathway" Data.Path
  ( Anchored (AbsDir, AbsFile, RelDir, RelFile, ReparentedDir, ReparentedFile),
    Path,
    Relativity (Abs, Rel),
    Type (Dir, File),
    anchor,
    forgetType,
    unanchor,
  )
import "pathway" Data.Path.Parser qualified as Parser
import safe "pathway-compat-base" Common
  ( InternalFailure (IncorrectResultType, ParseFailure),
  )
import safe "pathway-compat-filepath" Common.OsPath (localFormat, toPathRep)

openFile :: Path 'Abs 'File OsString -> IOMode -> IO Handle
openFile = FIO.openFile . toPathRep

openBinaryFile :: Path 'Abs 'File OsString -> IOMode -> IO Handle
openBinaryFile = FIO.openBinaryFile . toPathRep

openExistingFile :: Path 'Abs 'File OsString -> IOMode -> IO Handle
openExistingFile = FIO.openExistingFile . toPathRep

withFile :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withFile = FIO.withFile . toPathRep

withBinaryFile :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = FIO.withBinaryFile . toPathRep

withFile' :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withFile' = FIO.withFile' . toPathRep

withBinaryFile' :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' = FIO.withBinaryFile' . toPathRep

readFile :: Path 'Abs 'File OsString -> IO Lazy.ByteString
readFile = FIO.readFile . toPathRep

readFile' :: Path 'Abs 'File OsString -> IO Strict.ByteString
readFile' = FIO.readFile' . toPathRep

writeFile :: Path 'Abs 'File OsString -> Lazy.ByteString -> IO ()
writeFile = FIO.writeFile . toPathRep

writeFile' :: Path 'Abs 'File OsString -> Strict.ByteString -> IO ()
writeFile' = FIO.writeFile' . toPathRep

appendFile :: Path 'Abs 'File OsString -> Lazy.ByteString -> IO ()
appendFile = FIO.appendFile . toPathRep

appendFile' :: Path 'Abs 'File OsString -> Strict.ByteString -> IO ()
appendFile' = FIO.appendFile' . toPathRep

fromPathRep ::
  (Ord e) =>
  OsPath ->
  Either (MP.ParseErrorBundle OsPath e) (Anchored OsString)
fromPathRep =
  fmap (anchor . forgetType) . MP.parse (Parser.directory localFormat) ""

handleAnchoredPath ::
  (Ord e) =>
  (Anchored OsString -> Either (InternalFailure OsPath e) a) ->
  OsPath ->
  Either (InternalFailure OsPath e) a
handleAnchoredPath handler = either (Left . ParseFailure) handler . fromPathRep

absFileFromPathRep ::
  (Ord e) =>
  OsPath ->
  Either (InternalFailure OsPath e) (Path 'Abs 'File OsString)
absFileFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs File rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> pure path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

openTempFile ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openTempFile tmpDir =
  fmap (bitraverse absFileFromPathRep pure) . FIO.openTempFile (toPathRep tmpDir)

openBinaryTempFile ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openBinaryTempFile tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . FIO.openBinaryTempFile (toPathRep tmpDir)

openTempFileWithDefaultPermissions ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openTempFileWithDefaultPermissions tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . FIO.openTempFileWithDefaultPermissions (toPathRep tmpDir)

openBinaryTempFileWithDefaultPermissions ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openBinaryTempFileWithDefaultPermissions tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . FIO.openBinaryTempFileWithDefaultPermissions (toPathRep tmpDir)
