{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
#if MIN_VERSION_file_io(0, 1, 3)
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
#else
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

    -- * utils
    fromPathRep,
    handleAnchoredPath,
    absFileFromPathRep,
  )
where
#endif

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
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
#if MIN_VERSION_file_io(0, 1, 3)
import safe "base" Data.Bitraversable (bitraverse)
#endif

-- | `System.File.OsPath.openFile`, on `Path`s.
--
-- @since 99999
openFile :: Path 'Abs 'File OsString -> IOMode -> IO Handle
openFile = FIO.openFile . toPathRep

-- | `System.File.OsPath.openBinaryFile`, on `Path`s.
--
-- @since 99999
openBinaryFile :: Path 'Abs 'File OsString -> IOMode -> IO Handle
openBinaryFile = FIO.openBinaryFile . toPathRep

-- | `System.File.OsPath.openExistingFile`, on `Path`s.
--
-- @since 99999
openExistingFile :: Path 'Abs 'File OsString -> IOMode -> IO Handle
openExistingFile = FIO.openExistingFile . toPathRep

-- | `System.File.OsPath.withFile`, on `Path`s.
--
-- @since 99999
withFile :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withFile = FIO.withFile . toPathRep

-- | `System.File.OsPath.withBinaryFile`, on `Path`s.
--
-- @since 99999
withBinaryFile :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = FIO.withBinaryFile . toPathRep

-- | `System.File.OsPath.withFile'`, on `Path`s.
--
-- @since 99999
withFile' :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withFile' = FIO.withFile' . toPathRep

-- | `System.File.OsPath.withBinaryFile'`, on `Path`s.
--
-- @since 99999
withBinaryFile' :: Path 'Abs 'File OsString -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' = FIO.withBinaryFile' . toPathRep

-- | `System.File.OsPath.readFile`, on `Path`s.
--
-- @since 99999
readFile :: Path 'Abs 'File OsString -> IO Lazy.ByteString
readFile = FIO.readFile . toPathRep

-- | `System.File.OsPath.readFile'`, on `Path`s.
--
-- @since 99999
readFile' :: Path 'Abs 'File OsString -> IO Strict.ByteString
readFile' = FIO.readFile' . toPathRep

-- | `System.File.OsPath.writeFile`, on `Path`s.
--
-- @since 99999
writeFile :: Path 'Abs 'File OsString -> Lazy.ByteString -> IO ()
writeFile = FIO.writeFile . toPathRep

-- | `System.File.OsPath.writeFile'`, on `Path`s.
--
-- @since 99999
writeFile' :: Path 'Abs 'File OsString -> Strict.ByteString -> IO ()
writeFile' = FIO.writeFile' . toPathRep

-- | `System.File.OsPath.appendFile`, on `Path`s.
--
-- @since 99999
appendFile :: Path 'Abs 'File OsString -> Lazy.ByteString -> IO ()
appendFile = FIO.appendFile . toPathRep

-- | `System.File.OsPath.appendFile'`, on `Path`s.
--
-- @since 99999
appendFile' :: Path 'Abs 'File OsString -> Strict.ByteString -> IO ()
appendFile' = FIO.appendFile' . toPathRep

-- | Parse a path from the underlying library, in the local format.
--
-- @since 99999
fromPathRep ::
  (Ord e) =>
  OsPath ->
  Either (MP.ParseErrorBundle OsPath e) (Anchored OsString)
fromPathRep =
  fmap (anchor . forgetType) . MP.parse (Parser.directory localFormat) ""

-- | Parse a path from the underlying library, and pass it to a handler that
--   checks it has the expected type.
--
-- @since 99999
handleAnchoredPath ::
  (Ord e) =>
  (Anchored OsString -> Either (InternalFailure OsPath e) a) ->
  OsPath ->
  Either (InternalFailure OsPath e) a
handleAnchoredPath handler = either (Left . ParseFailure) handler . fromPathRep

-- | Parse an absolute file path from the underlying library, failing with
--   `IncorrectResultType` if it’s some other type of path.
--
-- @since 99999
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

#if MIN_VERSION_file_io(0, 1, 3)
-- | `System.File.OsPath.openTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
openTempFile ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openTempFile tmpDir =
  fmap (bitraverse absFileFromPathRep pure) . FIO.openTempFile (toPathRep tmpDir)

-- | `System.File.OsPath.openBinaryTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
openBinaryTempFile ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openBinaryTempFile tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . FIO.openBinaryTempFile (toPathRep tmpDir)

-- | `System.File.OsPath.openTempFileWithDefaultPermissions`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
openTempFileWithDefaultPermissions ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openTempFileWithDefaultPermissions tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . FIO.openTempFileWithDefaultPermissions (toPathRep tmpDir)

-- | `System.File.OsPath.openBinaryTempFileWithDefaultPermissions`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
openBinaryTempFileWithDefaultPermissions ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  OsString ->
  IO (Either (InternalFailure OsPath e) (Path 'Abs 'File OsString, Handle))
openBinaryTempFileWithDefaultPermissions tmpDir =
  fmap (bitraverse absFileFromPathRep pure)
    . FIO.openBinaryTempFileWithDefaultPermissions (toPathRep tmpDir)
#endif
