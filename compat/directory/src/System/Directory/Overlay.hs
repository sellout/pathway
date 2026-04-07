{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module System.Directory.Overlay
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
    -- getExecSearchPath,
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

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Category (Category ((.)))
import "base" Control.Exception (throwIO)
import "base" Control.Monad ((<=<), (=<<))
import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (either)
import "base" Data.Function (($))
import "base" Data.Maybe (Maybe)
import "base" Data.String (String)
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
import "this" System.Directory.Common (Operations)
import "this" System.Directory.Thin
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
import "this" System.Directory.Thin qualified as Thin

listDirectory ::
  Path 'Abs 'Dir String -> IO [Path ('Rel 'False) 'Type.Any String]
listDirectory =
  traverse (either throwIO $ pure . either forgetType forgetType)
    <=< Thin.listDirectory @Void

getDirectoryContents ::
  Path 'Abs 'Dir String -> IO [Path ('Rel 'True) 'Type.Any String]
getDirectoryContents =
  traverse (either throwIO $ pure . either forgetType (weaken . forgetType))
    <=< Thin.getDirectoryContents @Void

getCurrentDirectory :: IO (Path 'Abs 'Dir String)
getCurrentDirectory = either throwIO pure =<< Thin.getCurrentDirectory @Void

getHomeDirectory :: IO (Path 'Abs 'Dir String)
getHomeDirectory = either throwIO pure =<< Thin.getHomeDirectory @Void

-- | Prefer `XDG.BaseDirectory.*Home`.`
getXdgDirectory ::
  XdgDirectory -> Path ('Rel 'False) 'Dir String -> IO (Path 'Abs 'Dir String)
getXdgDirectory xdgDir =
  either throwIO pure <=< Thin.getXdgDirectory @Void xdgDir

getXdgDirectoryList :: XdgDirectoryList -> IO [Path 'Abs 'Dir String]
getXdgDirectoryList =
  traverse (either throwIO pure) <=< Thin.getXdgDirectoryList @Void

-- | Prefer `XDG.BaseDirectory.*Home`.
getAppUserDataDirectory ::
  Path ('Rel 'False) 'Dir String -> IO (Path 'Abs 'Dir String)
getAppUserDataDirectory = either throwIO pure <=< Thin.getAppUserDataDirectory @Void

-- | Prefer `XDG.UserDirectory.Common.documents`.
getUserDocumentsDirectory :: IO (Path 'Abs 'Dir String)
getUserDocumentsDirectory =
  either throwIO pure =<< Thin.getUserDocumentsDirectory @Void

-- | Prefer `XDG.BaseDirectory.runtimeDir`.
getTemporaryDirectory :: IO (Path 'Abs 'Dir String)
getTemporaryDirectory = either throwIO pure =<< Thin.getTemporaryDirectory @Void

#if MIN_VERSION_directory(1, 3, 10)
getExecSearchPath :: IO [Path 'Abs 'Dir String]
getExecSearchPath = traverse (either throwIO pure) =<< Thin.getExecSearchPath @Void
#endif

canonicalizePath ::
  (Operations String typ) => Path 'Abs typ String -> IO (Path 'Abs typ String)
canonicalizePath = either throwIO pure <=< Thin.canonicalizePath @_ @_ @Void

-- | Don’t use this. It relies on the “current directory”.
makeAbsolute ::
  (Operations String typ) =>
  Path ('Rel 'True) typ String -> IO (Path 'Abs typ String)
makeAbsolute = either throwIO pure <=< Thin.makeAbsolute @_ @_ @Void

makeRelativeToCurrentDirectory ::
  (Operations String typ) =>
  Path 'Abs typ String -> IO (Path ('Rel 'True) typ String)
makeRelativeToCurrentDirectory =
  either throwIO pure <=< Thin.makeRelativeToCurrentDirectory @_ @_ @Void

-- | This allows a relative file, because 1. the intent is to resolve the
--   absolute path, and 2. the paths searched are platform-specific, so
--   difficult to extract portably (especially when trying to match a search
--   function).
findExecutable ::
  Path ('Rel 'True) 'File String -> IO (Maybe (Path 'Abs 'File String))
findExecutable = traverse (either throwIO pure) <=< Thin.findExecutable @Void

findExecutables :: Path ('Rel 'True) 'File String -> IO [Path 'Abs 'File String]
findExecutables = traverse (either throwIO pure) <=< Thin.findExecutables @Void

findExecutablesInDirectories ::
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  -- | This doesn‘t attempt to partition the failures. We can’t expect the
  --   result list to have a 1:1 mapping with the input list, because some files
  --   may not exist or have had some other failure in IO, so this preserves the
  --   ordering so that some inference can be made for which failure correspond
  --   to which search directory.
  IO [Path 'Abs 'File String]
findExecutablesInDirectories dirs =
  traverse (either throwIO pure)
    <=< Thin.findExecutablesInDirectories @Void dirs

-- |
--
--  __TODO__: Why is this better than @`listToMaybe` `<$>` `findFiles` dirs@? In
--            `Pathway`, it probably isn’t because we probably want to return a
--            /parsable/ path if one exists, and this one could return an
--            unparsable path first.
findFile ::
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO (Maybe (Path 'Abs 'File String))
findFile dirs = traverse (either throwIO pure) <=< Thin.findFile @Void dirs

findFiles ::
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO [Path 'Abs 'File String]
findFiles dirs = traverse (either throwIO pure) <=< Thin.findFiles @Void dirs

findFileWith ::
  (Path 'Abs 'File String -> IO Bool) ->
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO (Maybe (Path 'Abs 'File String))
findFileWith pred dirs =
  traverse (either throwIO pure) <=< Thin.findFileWith @Void pred dirs

findFilesWith ::
  (Path 'Abs 'File String -> IO Bool) ->
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO [Path 'Abs 'File String]
findFilesWith pred dirs =
  traverse (either throwIO pure) <=< Thin.findFilesWith @Void pred dirs

getSymbolicLinkTarget ::
  (Operations String typ) => Path 'Abs typ String -> IO (Path 'Rel.Any typ String)
getSymbolicLinkTarget =
  either throwIO pure <=< Thin.getSymbolicLinkTarget @_ @_ @Void
