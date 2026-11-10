{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
#if MIN_VERSION_directory(1, 3, 10)
module System.Directory.OsPath.Overlay
  ( createDirectory,
    createDirectoryIfMissing,
    removeDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    renameDirectory,
    listDirectory,
    getDirectoryContents,
    getCurrentDirectory,
    setCurrentDirectory,
    withCurrentDirectory,
    getHomeDirectory,
    getXdgDirectory,
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,
    getExecSearchPath,
    removeFile,
    renameFile,
    renamePath,
    copyFile,
    copyFileWithMetadata,
    getFileSize,
    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    createFileLink,
    createDirectoryLink,
    removeDirectoryLink,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    setPermissions,
    copyPermissions,
    getAccessTime,
    getModificationTime,
    setAccessTime,
    setModificationTime,
  )
where
#else
module System.Directory.OsPath.Overlay
  ( createDirectory,
    createDirectoryIfMissing,
    removeDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    renameDirectory,
    listDirectory,
    getDirectoryContents,
    getCurrentDirectory,
    setCurrentDirectory,
    withCurrentDirectory,
    getHomeDirectory,
    getXdgDirectory,
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,
    removeFile,
    renameFile,
    renamePath,
    copyFile,
    copyFileWithMetadata,
    getFileSize,
    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    createFileLink,
    createDirectoryLink,
    removeDirectoryLink,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    getPermissions,
    setPermissions,
    copyPermissions,
    getAccessTime,
    getModificationTime,
    setAccessTime,
    setModificationTime,
  )
where
#endif

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Category (Category ((.)))
import "base" Control.Exception (throwIO)
import "base" Control.Monad ((<=<), (=<<))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (either)
import "base" Data.Foldable (foldrM)
import "base" Data.Maybe (Maybe)
import "base" Data.Traversable (Traversable (traverse))
import "base" Data.Void (Void)
import "base" System.IO (IO)
import "pathway" Data.Path
  ( Path,
    Relativity (Abs, Rel),
    Type (Dir, File),
    forgetType,
    weaken,
  )
import "pathway" Data.Path.Relativity qualified as Rel
import "pathway" Data.Path.Type qualified as Type
import "pathway-compat-filepath" System.OsPath.Pathway (OsString)
import "these" Data.These (these)
import "this" System.Directory.Common (Operations)
import "this" System.Directory.OsPath.Thin
  ( XdgDirectory,
    XdgDirectoryList,
    copyFile,
    copyFileWithMetadata,
    copyPermissions,
    createDirectory,
    createDirectoryIfMissing,
    createDirectoryLink,
    createFileLink,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getAccessTime,
    getFileSize,
    getModificationTime,
    getPermissions,
    pathIsSymbolicLink,
    removeDirectory,
    removeDirectoryLink,
    removeDirectoryRecursive,
    removeFile,
    removePathForcibly,
    renameDirectory,
    renameFile,
    renamePath,
    setAccessTime,
    setCurrentDirectory,
    setModificationTime,
    setPermissions,
    withCurrentDirectory,
  )
import "this" System.Directory.OsPath.Thin qualified as Thin

-- | `System.Directory.OsPath.listDirectory`, on `Path`s, with failures thrown
--   in `IO` (as the original would) rather than returned.
--
-- @since 99999
listDirectory ::
  Path 'Abs 'Dir OsString -> IO [Path ('Rel 'False) 'Type.Any OsString]
listDirectory =
  foldrM
    ( \p acc ->
        either
          throwIO
          ( pure
              . these
                ((: acc) . forgetType)
                ((: acc) . forgetType)
                (\d f -> forgetType d : forgetType f : acc)
          )
          p
    )
    []
    <=< Thin.listDirectory @Void

-- | `System.Directory.OsPath.getDirectoryContents`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
getDirectoryContents ::
  Path 'Abs 'Dir OsString -> IO [Path ('Rel 'True) 'Type.Any OsString]
getDirectoryContents =
  foldrM
    ( \p acc ->
        either
          throwIO
          ( pure
              . these
                ((: acc) . forgetType)
                ((: acc) . weaken . forgetType)
                (\d f -> forgetType d : weaken (forgetType f) : acc)
          )
          p
    )
    []
    <=< Thin.getDirectoryContents @Void

-- | `System.Directory.OsPath.getCurrentDirectory`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
getCurrentDirectory :: IO (Path 'Abs 'Dir OsString)
getCurrentDirectory = either throwIO pure =<< Thin.getCurrentDirectory @Void

-- | `System.Directory.OsPath.getHomeDirectory`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
getHomeDirectory :: IO (Path 'Abs 'Dir OsString)
getHomeDirectory = either throwIO pure =<< Thin.getHomeDirectory @Void

-- | Prefer `XDG.BaseDirectory.*Home`.`
--
-- @since 99999
getXdgDirectory ::
  XdgDirectory -> Path ('Rel 'False) 'Dir OsString -> IO (Path 'Abs 'Dir OsString)
getXdgDirectory xdgDir =
  either throwIO pure <=< Thin.getXdgDirectory @Void xdgDir

-- | `System.Directory.OsPath.getXdgDirectoryList`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
getXdgDirectoryList :: XdgDirectoryList -> IO [Path 'Abs 'Dir OsString]
getXdgDirectoryList =
  traverse (either throwIO pure) <=< Thin.getXdgDirectoryList @Void

-- | Prefer `XDG.BaseDirectory.*Home`.
--
-- @since 99999
getAppUserDataDirectory ::
  Path ('Rel 'False) 'Dir OsString -> IO (Path 'Abs 'Dir OsString)
getAppUserDataDirectory = either throwIO pure <=< Thin.getAppUserDataDirectory @Void

-- | Prefer `XDG.UserDirectory.Common.documents`.
--
-- @since 99999
getUserDocumentsDirectory :: IO (Path 'Abs 'Dir OsString)
getUserDocumentsDirectory =
  either throwIO pure =<< Thin.getUserDocumentsDirectory @Void

-- | Prefer `XDG.BaseDirectory.runtimeDir`.
--
-- @since 99999
getTemporaryDirectory :: IO (Path 'Abs 'Dir OsString)
getTemporaryDirectory = either throwIO pure =<< Thin.getTemporaryDirectory @Void

#if MIN_VERSION_directory(1, 3, 10)
-- | `System.Directory.OsPath.getExecSearchPath`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
getExecSearchPath :: IO [Path 'Abs 'Dir OsString]
getExecSearchPath = traverse (either throwIO pure) =<< Thin.getExecSearchPath @Void
#endif

-- | `System.Directory.OsPath.canonicalizePath`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
canonicalizePath ::
  (Operations OsString typ) =>
  Path 'Abs typ OsString -> IO (Path 'Abs typ OsString)
canonicalizePath = either throwIO pure <=< Thin.canonicalizePath @_ @_ @Void

-- | Don’t use this. It relies on the “current directory”.
--
-- @since 99999
makeAbsolute ::
  (Operations OsString typ) =>
  Path ('Rel 'True) typ OsString -> IO (Path 'Abs typ OsString)
makeAbsolute = either throwIO pure <=< Thin.makeAbsolute @_ @_ @Void

-- | `System.Directory.OsPath.makeRelativeToCurrentDirectory`, on `Path`s, with
--   failures thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
makeRelativeToCurrentDirectory ::
  (Operations OsString typ) =>
  Path 'Abs typ OsString -> IO (Path ('Rel 'True) typ OsString)
makeRelativeToCurrentDirectory =
  either throwIO pure <=< Thin.makeRelativeToCurrentDirectory @_ @_ @Void

-- | This allows a relative file, because 1. the intent is to resolve the
--   absolute path, and 2. the paths searched are platform-specific, so
--   difficult to extract portably (especially when trying to match a search
--   function).
--
-- @since 99999
findExecutable ::
  Path ('Rel 'True) 'File OsString -> IO (Maybe (Path 'Abs 'File OsString))
findExecutable = traverse (either throwIO pure) <=< Thin.findExecutable @Void

-- | `System.Directory.OsPath.findExecutables`, on `Path`s, with failures thrown
--   in `IO` (as the original would) rather than returned.
--
-- @since 99999
findExecutables :: Path ('Rel 'True) 'File OsString -> IO [Path 'Abs 'File OsString]
findExecutables = traverse (either throwIO pure) <=< Thin.findExecutables @Void

-- | `System.Directory.OsPath.findExecutablesInDirectories`, on `Path`s, with
--   failures thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
findExecutablesInDirectories ::
  [Path 'Abs 'Dir OsString] ->
  Path ('Rel 'True) 'File OsString ->
  -- | This doesn‘t attempt to partition the failures. We can’t expect the
  --   result list to have a 1:1 mapping with the input list, because some files
  --   may not exist or have had some other failure in IO, so this preserves the
  --   ordering so that some inference can be made for which failure correspond
  --   to which search directory.
  IO [Path 'Abs 'File OsString]
findExecutablesInDirectories dirs =
  traverse (either throwIO pure)
    <=< Thin.findExecutablesInDirectories @Void dirs

-- |
--
--  __TODO__: Why is this better than @`listToMaybe` `<$>` `findFiles` dirs@? In
--            `Pathway`, it probably isn’t because we probably want to return a
--            /parsable/ path if one exists, and this one could return an
--            unparsable path first.
--
-- @since 99999
findFile ::
  [Path 'Abs 'Dir OsString] ->
  Path ('Rel 'True) 'File OsString ->
  IO (Maybe (Path 'Abs 'File OsString))
findFile dirs = traverse (either throwIO pure) <=< Thin.findFile @Void dirs

-- | `System.Directory.OsPath.findFiles`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
findFiles ::
  [Path 'Abs 'Dir OsString] ->
  Path ('Rel 'True) 'File OsString ->
  IO [Path 'Abs 'File OsString]
findFiles dirs = traverse (either throwIO pure) <=< Thin.findFiles @Void dirs

-- | `System.Directory.OsPath.findFileWith`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
findFileWith ::
  (Path 'Abs 'File OsString -> IO Bool) ->
  [Path 'Abs 'Dir OsString] ->
  Path ('Rel 'True) 'File OsString ->
  IO (Maybe (Path 'Abs 'File OsString))
findFileWith pred dirs =
  traverse (either throwIO pure) <=< Thin.findFileWith @Void pred dirs

-- | `System.Directory.OsPath.findFilesWith`, on `Path`s, with failures thrown
--   in `IO` (as the original would) rather than returned.
--
-- @since 99999
findFilesWith ::
  (Path 'Abs 'File OsString -> IO Bool) ->
  [Path 'Abs 'Dir OsString] ->
  Path ('Rel 'True) 'File OsString ->
  IO [Path 'Abs 'File OsString]
findFilesWith pred dirs =
  traverse (either throwIO pure) <=< Thin.findFilesWith @Void pred dirs

-- | `System.Directory.OsPath.getSymbolicLinkTarget`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
getSymbolicLinkTarget ::
  (Operations OsString typ) =>
  Path 'Abs typ OsString -> IO (Path 'Rel.Any typ OsString)
getSymbolicLinkTarget =
  either throwIO pure <=< Thin.getSymbolicLinkTarget @_ @_ @Void
