{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- __NB__: Because of the nested @`Show` (`MP.Token` rep)@ constraints.
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- "System.Directory" has inconsistent Safe Haskell modes across versions. We
-- can’t conditionalize the Safe Haskell extension (because it forces Safe
-- Haskell-using consumers to conditionalize), so this silences the fact that
-- this module is inferred ‘Safe’ in some configurations.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

#if MIN_VERSION_directory(1, 3, 10)
module System.Directory.Thin
  ( InternalFailure (..),
    Dir.Permissions,
    XdgDirectory,
    XdgDirectoryList,
    createDirectory,
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
module System.Directory.Thin
  ( InternalFailure (..),
    Dir.Permissions,
    XdgDirectory,
    XdgDirectoryList,
    createDirectory,
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

import safe "base" Control.Applicative (Applicative (pure))
import safe "base" Control.Category (Category ((.)))
import safe "base" Control.Exception (throwIO)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor (fmap), (<$>))
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Ord (Ord)
import safe "base" Data.String (String)
import safe "base" Data.Void (Void)
import safe "base" System.IO (FilePath, IO)
import "directory" System.Directory (XdgDirectory, XdgDirectoryList)
import "directory" System.Directory qualified as Dir
import safe "pathway" Data.Path
  ( Anchored (AbsDir, AbsFile, RelDir, RelFile, ReparentedDir, ReparentedFile),
    Path,
    Pathy,
    Relative,
    Relativity (Abs, Rel),
    Type (Dir, File),
    Typey,
    forgetRelativity,
    unanchor,
    weaken,
  )
import safe "pathway" Data.Path.Relativity qualified as Rel
import safe "pathway" Data.Path.Type qualified as Type
import safe "pathway-compat-base" Common
  ( InternalFailure (IncorrectResultType, ParseFailure),
    absFileFromPathRep,
    handleAnchoredPath,
    toPathRep,
  )
import safe "pathway-compat-filepath" Common.FilePath
  ( absDirFromPathRep,
    anyDirFromPathRep,
    handleAnchoredDir,
  )
import safe "time" Data.Time.Clock (UTCTime)
import safe "this" System.Directory.Common
  ( Operations,
    canonicalizePath,
    getSymbolicLinkTarget,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
  )
import safe "base" Prelude (Integer)

relPathFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either
    (InternalFailure FilePath e)
    ( Either
        (Path ('Rel 'False) 'Dir String)
        (Path ('Rel 'False) 'File String)
    )
relPathFromPathRep =
  let badType rel typ = Left . IncorrectResultType (Rel False) Type.Any rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> pure $ Left path
        RelFile path -> pure $ pure path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

repDirFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either (InternalFailure FilePath e) (Path ('Rel 'True) 'Dir String)
repDirFromPathRep =
  let badType rel typ = Left . IncorrectResultType (Rel True) Dir rel typ
   in handleAnchoredDir \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> pure $ weaken path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> pure path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

repFileFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either (InternalFailure FilePath e) (Path ('Rel 'True) 'File String)
repFileFromPathRep =
  let badType rel typ = Left . IncorrectResultType Rel.Any Type.Any rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> pure $ weaken path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> pure path

anyFileFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either (InternalFailure FilePath e) (Path 'Rel.Any 'File String)
anyFileFromPathRep =
  let badType rel typ = Left . IncorrectResultType Rel.Any Type.Any rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> pure $ forgetRelativity path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> pure $ forgetRelativity path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> pure $ forgetRelativity path

mixedPathFromPathRep ::
  (Ord e) =>
  FilePath ->
  Either
    (InternalFailure FilePath e)
    ( Either
        (Path ('Rel 'True) 'Dir String)
        (Path ('Rel 'False) 'File String)
    )
mixedPathFromPathRep =
  let badType rel typ = Left . IncorrectResultType Rel.Any Type.Any rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> pure . Left $ weaken path
        RelFile path -> pure $ pure path
        ReparentedDir path -> pure $ Left path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

-- * Above here needs to be moved

createDirectory :: Path 'Abs 'Dir String -> IO ()
createDirectory = Dir.createDirectory . toPathRep

-- | Can perhaps unify this and the following definition depending on how we
--   handle errors.
createDirectoryIfMissing :: Bool -> Path 'Abs 'Dir String -> IO ()
createDirectoryIfMissing cp = Dir.createDirectoryIfMissing cp . toPathRep

removeDirectory :: Path 'Abs 'Dir String -> IO ()
removeDirectory = Dir.removeDirectory . toPathRep

removeDirectoryRecursive :: Path 'Abs 'Dir String -> IO ()
removeDirectoryRecursive = Dir.removeDirectoryRecursive . toPathRep

removePathForcibly :: (Typey typ) => Path 'Abs typ String -> IO ()
removePathForcibly = Dir.removePathForcibly . toPathRep

renameDirectory :: Path 'Abs 'Dir String -> Path 'Abs 'Dir String -> IO ()
renameDirectory source = Dir.renameDirectory (toPathRep source) . toPathRep

listDirectory ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  -- |
  --
  --  __TODO__: This should be @`Unambiguous` ('`Rel` '`False`) `String`@ once #18
  --            lands.
  IO
    [ Either
        (InternalFailure FilePath e)
        ( Either
            (Path ('Rel 'False) 'Dir String)
            (Path ('Rel 'False) 'File String)
        )
    ]
listDirectory = fmap (relPathFromPathRep <$>) . Dir.listDirectory . toPathRep

getDirectoryContents ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  -- |
  --
  --  __TODO__: Should the parent directory (`../`) be singled out, to allow
  --            @`Rel` `False`@ for the directories?
  IO
    [ Either
        (InternalFailure FilePath e)
        ( Either
            (Path ('Rel 'True) 'Dir String)
            (Path ('Rel 'False) 'File String)
        )
    ]
getDirectoryContents =
  fmap (mixedPathFromPathRep <$>) . Dir.getDirectoryContents . toPathRep

getCurrentDirectory ::
  (Ord e) =>
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'Dir String))
getCurrentDirectory = absDirFromPathRep <$> Dir.getCurrentDirectory

-- | Don’t use this. Ideally, you should avoid relying on a dynamically scoped
--   “current directory” (as the pathway-system library encourages), but if you
--   are using code that expects the current directory to be set, then use
--   `withCurrentDirectory`.
setCurrentDirectory :: Path 'Abs 'Dir String -> IO ()
setCurrentDirectory = Dir.setCurrentDirectory . toPathRep

withCurrentDirectory :: Path 'Abs 'Dir String -> IO a -> IO a
withCurrentDirectory = Dir.withCurrentDirectory . toPathRep

getHomeDirectory ::
  (Ord e) => IO (Either (InternalFailure FilePath e) (Path 'Abs 'Dir String))
getHomeDirectory = absDirFromPathRep <$> Dir.getHomeDirectory

overAbsDir ::
  (Ord e, Pathy rel typ) =>
  (FilePath -> IO FilePath) ->
  Path rel typ String ->
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'Dir String))
overAbsDir op = fmap absDirFromPathRep . op . toPathRep

-- | Prefer `XDG.BaseDirectory.*Home`.`
getXdgDirectory ::
  (Ord e) =>
  XdgDirectory ->
  Path ('Rel 'False) 'Dir String ->
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'Dir String))
getXdgDirectory = overAbsDir . Dir.getXdgDirectory

getXdgDirectoryList ::
  (Ord e) =>
  XdgDirectoryList ->
  IO [Either (InternalFailure FilePath e) (Path 'Abs 'Dir String)]
getXdgDirectoryList = fmap (absDirFromPathRep <$>) . Dir.getXdgDirectoryList

-- | Prefer `XDG.BaseDirectory.*Home`.
getAppUserDataDirectory ::
  (Ord e) =>
  Path ('Rel 'False) 'Dir String ->
  IO (Either (InternalFailure FilePath e) (Path 'Abs 'Dir String))
getAppUserDataDirectory = overAbsDir Dir.getAppUserDataDirectory

-- | Prefer `XDG.UserDirectory.Common.documents`.
getUserDocumentsDirectory ::
  (Ord e) => IO (Either (InternalFailure FilePath e) (Path 'Abs 'Dir String))
getUserDocumentsDirectory = absDirFromPathRep <$> Dir.getUserDocumentsDirectory

-- | Prefer `XDG.BaseDirectory.runtimeDir`.
getTemporaryDirectory ::
  (Ord e) => IO (Either (InternalFailure FilePath e) (Path 'Abs 'Dir String))
getTemporaryDirectory = absDirFromPathRep <$> Dir.getTemporaryDirectory

#if MIN_VERSION_directory(1, 3, 10)
getExecSearchPath ::
  (Ord e) =>
  IO [Either (InternalFailure FilePath e) (Path 'Abs 'Dir String)]
getExecSearchPath = fmap absDirFromPathRep <$> Dir.getExecSearchPath
#endif

removeFile :: Path 'Abs 'File String -> IO ()
removeFile = Dir.removeFile . toPathRep

renameFile :: Path 'Abs 'File String -> Path 'Abs 'File String -> IO ()
renameFile old = Dir.renameFile (toPathRep old) . toPathRep

renamePath ::
  (Typey typ) => Path 'Abs typ String -> Path 'Abs typ String -> IO ()
renamePath source = Dir.renamePath (toPathRep source) . toPathRep

copyFile' ::
  (FilePath -> FilePath -> IO ()) ->
  Path 'Abs 'File String ->
  Path 'Abs 'File String ->
  IO ()
copyFile' op source = op (toPathRep source) . toPathRep

copyFile ::
  Path 'Abs 'File String -> Path 'Abs 'File String -> IO ()
copyFile = copyFile' Dir.copyFile

copyFileWithMetadata ::
  Path 'Abs 'File String -> Path 'Abs 'File String -> IO ()
copyFileWithMetadata = copyFile' Dir.copyFileWithMetadata

getFileSize :: Path 'Abs 'File String -> IO Integer
getFileSize = Dir.getFileSize . toPathRep

instance Operations String 'Dir where
  canonicalizePath = fmap absDirFromPathRep . Dir.canonicalizePath . toPathRep
  makeAbsolute = fmap absDirFromPathRep . Dir.makeAbsolute . toPathRep
  makeRelativeToCurrentDirectory =
    fmap repDirFromPathRep . Dir.makeRelativeToCurrentDirectory . toPathRep
  getSymbolicLinkTarget =
    fmap anyDirFromPathRep . Dir.getSymbolicLinkTarget . toPathRep

instance Operations String 'File where
  canonicalizePath = fmap absFileFromPathRep . Dir.canonicalizePath . toPathRep
  makeAbsolute = fmap absFileFromPathRep . Dir.makeAbsolute . toPathRep
  makeRelativeToCurrentDirectory =
    fmap repFileFromPathRep . Dir.makeRelativeToCurrentDirectory . toPathRep

  getSymbolicLinkTarget =
    fmap anyFileFromPathRep . Dir.getSymbolicLinkTarget . toPathRep

doesPathExist :: (Typey typ) => Path 'Abs typ String -> IO Bool
doesPathExist = Dir.doesPathExist . toPathRep

doesFileExist :: Path 'Abs 'File String -> IO Bool
doesFileExist = Dir.doesFileExist . toPathRep

doesDirectoryExist :: Path 'Abs 'Dir String -> IO Bool
doesDirectoryExist = Dir.doesDirectoryExist . toPathRep

findExecutables' ::
  (Ord e, Functor f) =>
  (String -> IO (f FilePath)) ->
  Path ('Rel 'True) 'File String ->
  IO (f (Either (InternalFailure FilePath e) (Path 'Abs 'File String)))
findExecutables' op = fmap (absFileFromPathRep <$>) . op . toPathRep

-- | This allows a relative file, because 1. the intent is to resolve the
--   absolute path, and 2. the paths searched are platform-specific, so
--   difficult to extract portably (especially when trying to match a search
--   function).
findExecutable ::
  (Ord e) =>
  Path ('Rel 'True) 'File String ->
  IO (Maybe (Either (InternalFailure FilePath e) (Path 'Abs 'File String)))
findExecutable = findExecutables' Dir.findExecutable

findExecutables ::
  (Ord e) =>
  Path ('Rel 'True) 'File String ->
  IO [Either (InternalFailure FilePath e) (Path 'Abs 'File String)]
findExecutables = findExecutables' Dir.findExecutables

findFiles' ::
  (Ord e, Functor f) =>
  ([FilePath] -> String -> IO (f FilePath)) ->
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO (f (Either (InternalFailure FilePath e) (Path 'Abs 'File String)))
findFiles' op dirs =
  fmap (absFileFromPathRep <$>) . op (toPathRep <$> dirs) . toPathRep

findExecutablesInDirectories ::
  (Ord e) =>
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  -- | This doesn‘t attempt to partition the failures. We can’t expect the
  --   result list to have a 1:1 mapping with the input list, because some files
  --   may not exist or have had some other failure in IO, so this preserves the
  --   ordering so that some inference can be made for which failure correspond
  --   to which search directory.
  IO [Either (InternalFailure FilePath e) (Path 'Abs 'File String)]
findExecutablesInDirectories = findFiles' Dir.findExecutablesInDirectories

-- |
--
--  __TODO__: Why is this better than @`listToMaybe` `<$>` `findFiles` dirs@? In
--            `Pathway`, it probably isn’t because we probably want to return a
--            /parsable/ path if one exists, and this one could return an
--            unparsable path first.
findFile ::
  (Ord e) =>
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO (Maybe (Either (InternalFailure FilePath e) (Path 'Abs 'File String)))
findFile = findFiles' Dir.findFile

findFiles ::
  (Ord e) =>
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  -- | This doesn‘t attempt to partition the failures. We can’t expect the
  --   result list to have a 1:1 mapping with the input list, because some files
  --   may not exist or have had some other failure in IO, so this preserves the
  --   ordering so that some inference can be made for which failure correspond
  --   to which search directory.
  IO [Either (InternalFailure FilePath e) (Path 'Abs 'File String)]
findFiles = findFiles' Dir.findFiles

-- |
--
--  __TODO__: Provide a `MonadIO` version of this, which would also allow
--            handling failures from the predicate.
--
--  __TODO__: Why have this instead of using something like @`filterM` pred
--            `<=<` `findFiles` dirs@, which would also handle the `MonadIO`
--            aspect nicely and eliminate the need for handling path-parsing
--            failures in the predicate?
findFilesWith' ::
  forall f e.
  (Ord e, Functor f) =>
  ((FilePath -> IO Bool) -> [FilePath] -> String -> IO (f FilePath)) ->
  (Path 'Abs 'File String -> IO Bool) ->
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO (f (Either (InternalFailure FilePath e) (Path 'Abs 'File String)))
findFilesWith' op pred dirs =
  fmap (absFileFromPathRep <$>)
    . op (either throwIO pred . absFileFromPathRep @Void) (toPathRep <$> dirs)
    . toPathRep

findFileWith ::
  forall e.
  (Ord e) =>
  (Path 'Abs 'File String -> IO Bool) ->
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO (Maybe (Either (InternalFailure FilePath e) (Path 'Abs 'File String)))
findFileWith = findFilesWith' Dir.findFileWith

findFilesWith ::
  (Ord e) =>
  (Path 'Abs 'File String -> IO Bool) ->
  [Path 'Abs 'Dir String] ->
  Path ('Rel 'True) 'File String ->
  IO [Either (InternalFailure FilePath e) (Path 'Abs 'File String)]
findFilesWith = findFilesWith' Dir.findFilesWith

createFileLink :: (Relative rel) => Path rel 'File String -> Path 'Abs 'File String -> IO ()
createFileLink old = Dir.createFileLink (toPathRep old) . toPathRep

createDirectoryLink :: (Relative rel) => Path rel 'Dir String -> Path 'Abs 'Dir String -> IO ()
createDirectoryLink old = Dir.createDirectoryLink (toPathRep old) . toPathRep

removeDirectoryLink :: Path 'Abs 'Dir String -> IO ()
removeDirectoryLink = Dir.removeDirectoryLink . toPathRep

pathIsSymbolicLink :: (Typey typ) => Path 'Abs typ String -> IO Bool
pathIsSymbolicLink = Dir.pathIsSymbolicLink . toPathRep

getPermissions ::
  (Typey typ) => Path 'Abs typ String -> IO Dir.Permissions
getPermissions = Dir.getPermissions . toPathRep

setPermissions ::
  (Typey typ) => Path 'Abs typ String -> Dir.Permissions -> IO ()
setPermissions = Dir.setPermissions . toPathRep

copyPermissions ::
  (Typey typ, Typey typ') =>
  Path 'Abs typ String ->
  Path 'Abs typ' String ->
  IO ()
copyPermissions from = Dir.copyPermissions (toPathRep from) . toPathRep

getAccessTime :: (Typey typ) => Path 'Abs typ String -> IO UTCTime
getAccessTime = Dir.getAccessTime . toPathRep

getModificationTime :: (Typey typ) => Path 'Abs typ String -> IO UTCTime
getModificationTime = Dir.getModificationTime . toPathRep

setAccessTime :: (Typey typ) => Path 'Abs typ String -> UTCTime -> IO ()
setAccessTime = Dir.setAccessTime . toPathRep

setModificationTime ::
  (Typey typ) => Path 'Abs typ String -> UTCTime -> IO ()
setModificationTime = Dir.setModificationTime . toPathRep
