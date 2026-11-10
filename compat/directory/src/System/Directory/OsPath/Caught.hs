{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- WAIT: "NoRecursion" doesn’t currently provide a way to ignore names like
--       `$dPopVariant_aei8`, but it should.
{-# OPTIONS_GHC -fplugin-opt NoRecursion:allow-recursion:true #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--  __TODO__: If we reimplement the directory package rather than wrap it, we
--            can capture failures at a lower level, and be more precise about
--            which ones can occur in each operation, rather than relying on
--            docs.
--
-- @since 99999
module System.Directory.OsPath.Caught
  ( Dir.Permissions,
    createDirectory,
    createDirectoryIfMissing,
    createDirectoryWithParentsIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    findFiles,
    findFilesWith,
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

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category (Category ((.)), (.))
import safe "base" Control.Exception (tryJust)
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Ord (Ord)
import safe "base" Data.Void (Void)
import safe "base" System.IO (IO)
import safe "base" System.IO.Error qualified as IO
import "directory" System.Directory (XdgDirectory, XdgDirectoryList)
import "filepath" System.OsPath.Types (OsPath, OsString)
import safe "pathway" Data.Path
  ( Path,
    Relative,
    Relativity (Abs, Rel),
    Type (Dir, File),
    Typey,
  )
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "pathway-compat-base" System.IO.Error.Pathway
  ( AlreadyExistsError (AlreadyExistsError),
    FullError (FullError),
    InvalidArgument,
  )
import safe "these" Data.These (These)
import safe "time" Data.Time.Clock (UTCTime)
import "variant" Data.Variant (V, liftVariant, toVariant)
import safe "this" System.Directory.Common (Operations)
import safe "this" System.Directory.Error
  ( CreateLinkFailure,
    CreationFailure,
    CurrentDirectoryFailure,
    DirRemovalFailure,
    GetDirectoryFailure,
    GetUserDirectoriesFailure,
    GetUserDirectoryFailure,
    InternalFailure,
    ListFailure,
    MakeFailure,
    MaybeCreationFailure,
    MaybeParentCreationFailure,
    MetadataFailure,
    RemovalFailure,
    RenameFailure,
    SetCurrentDirectoryFailure,
    recoverCreateLinkFailure,
    recoverCreationFailure,
    recoverDirRemovalFailure,
    recoverGetDirectoryFailure,
    recoverGetUserDirectoriesFailure,
    recoverGetUserDirectoryFailure,
    recoverListFailure,
    recoverMakeFailure,
    recoverMaybeCreationFailure,
    recoverMaybeParentCreationFailure,
    recoverMetadataFailure,
    recoverRemovalFailure,
    recoverRenameFailure,
    recoverSetCurrentDirectoryFailure,
    tryWithIF,
  )
import safe "this" System.Directory.OsPath.Thin
  ( doesDirectoryExist,
    doesFileExist,
    findExecutable,
    findFiles,
    findFilesWith,
  )
import safe "this" System.Directory.OsPath.Thin qualified as Dir
import safe "base" Prelude (Integer)

-- | `System.Directory.OsPath.createDirectory`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
createDirectory :: Path 'Abs 'Dir OsString -> IO (Either (V CreationFailure) ())
createDirectory = tryJust recoverCreationFailure . Dir.createDirectory

-- | Can perhaps unify this and the following definition depending on how we
--   handle errors.
--
-- @since 99999
createDirectoryIfMissing ::
  Path 'Abs 'Dir OsString -> IO (Either (V MaybeCreationFailure) ())
createDirectoryIfMissing =
  tryJust recoverMaybeCreationFailure . Dir.createDirectoryIfMissing False

-- | `System.Directory.OsPath.createDirectoryIfMissing` @True@, on `Path`s, with
--   the `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
createDirectoryWithParentsIfMissing ::
  Path 'Abs 'Dir OsString ->
  IO (Either (V MaybeParentCreationFailure) ())
createDirectoryWithParentsIfMissing =
  tryJust recoverMaybeParentCreationFailure . Dir.createDirectoryIfMissing True

-- | `System.Directory.OsPath.removeDirectory`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
removeDirectory :: Path 'Abs 'Dir OsString -> IO (Either (V DirRemovalFailure) ())
removeDirectory = tryJust recoverDirRemovalFailure . Dir.removeDirectory

-- | `System.Directory.OsPath.removeDirectoryRecursive`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
removeDirectoryRecursive :: Path 'Abs 'Dir OsString -> IO (Either (V DirRemovalFailure) ())
removeDirectoryRecursive =
  tryJust recoverDirRemovalFailure . Dir.removeDirectoryRecursive

-- | `System.Directory.OsPath.removePathForcibly`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
removePathForcibly :: (Typey typ) => Path 'Abs typ OsString -> IO (Either (V DirRemovalFailure) ())
removePathForcibly = tryJust recoverDirRemovalFailure . Dir.removePathForcibly

-- | `System.Directory.OsPath.renameDirectory`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
renameDirectory ::
  Path 'Abs 'Dir OsString ->
  Path 'Abs 'Dir OsString ->
  IO (Either (V (AlreadyExistsError ': RenameFailure)) ())
renameDirectory source =
  tryJust
    ( \ioe ->
        if IO.isAlreadyExistsError ioe
          then pure . toVariant $ AlreadyExistsError ioe
          else liftVariant <$> recoverRenameFailure ioe
    )
    . Dir.renameDirectory source

-- | `System.Directory.OsPath.listDirectory`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
listDirectory ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  -- |
  --
  --  __TODO__: This should be @`Unambiguous` ('`Rel` '`False`) `OsString`@ once #18
  --            lands.
  IO
    ( Either
        (V ListFailure)
        [ Either
            (InternalFailure OsPath e)
            ( These
                (Path ('Rel 'False) 'Dir OsString)
                (Path ('Rel 'False) 'File OsString)
            )
        ]
    )
listDirectory = tryJust recoverListFailure . Dir.listDirectory

-- | `System.Directory.OsPath.getDirectoryContents`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getDirectoryContents ::
  (Ord e) =>
  Path 'Abs 'Dir OsString ->
  -- |
  --
  --  __TODO__: Should the parent directory (`../`) be singled out, to allow
  --            @`Rel` `False`@ for the directories?
  IO
    ( Either
        (V ListFailure)
        [ Either
            (InternalFailure OsPath e)
            ( These
                (Path ('Rel 'True) 'Dir OsString)
                (Path ('Rel 'False) 'File OsString)
            )
        ]
    )
getDirectoryContents = tryJust recoverListFailure . Dir.getDirectoryContents

-- | `System.Directory.OsPath.getCurrentDirectory`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getCurrentDirectory ::
  IO
    ( Either
        (V (FullError ': InternalFailure OsPath Void ': CurrentDirectoryFailure))
        (Path 'Abs 'Dir OsString)
    )
getCurrentDirectory = first toVariant <$> Dir.getCurrentDirectory @Void

-- | Don’t use this. Ideally, you should avoid relying on a dynamically scoped(
--   “current directory” (as the pathway-system library encourages), but if you
--   are using code that expects the current directory to be set, then use
--   `withCurrentDirectory`.
--
-- @since 99999
setCurrentDirectory ::
  Path 'Abs 'Dir OsString -> IO (Either (V SetCurrentDirectoryFailure) ())
setCurrentDirectory =
  tryJust recoverSetCurrentDirectoryFailure . Dir.setCurrentDirectory

-- | `System.Directory.OsPath.withCurrentDirectory`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
withCurrentDirectory ::
  Path 'Abs 'Dir OsString ->
  IO a ->
  IO (Either (V (FullError ': SetCurrentDirectoryFailure)) a)
withCurrentDirectory path =
  tryJust
    ( \ioe ->
        if IO.isFullError ioe
          then pure . toVariant $ FullError ioe
          else liftVariant <$> recoverSetCurrentDirectoryFailure ioe
    )
    . Dir.withCurrentDirectory path

-- | `System.Directory.OsPath.getHomeDirectory`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getHomeDirectory ::
  IO (Either (V (GetUserDirectoryFailure OsString)) (Path 'Abs 'Dir OsString))
getHomeDirectory = tryWithIF recoverGetUserDirectoryFailure $ Dir.getHomeDirectory @Void

-- | Prefer `XDG.BaseDirectory.*Home`.`
--
-- @since 99999
getXdgDirectory ::
  XdgDirectory ->
  Path ('Rel 'False) 'Dir OsString ->
  IO (Either (V (GetUserDirectoryFailure OsString)) (Path 'Abs 'Dir OsString))
getXdgDirectory dir =
  tryWithIF recoverGetUserDirectoryFailure . Dir.getXdgDirectory @Void dir

-- | `System.Directory.OsPath.getXdgDirectoryList`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getXdgDirectoryList ::
  (Ord e) =>
  XdgDirectoryList ->
  IO
    ( Either
        (V GetUserDirectoriesFailure)
        [Either (InternalFailure OsPath e) (Path 'Abs 'Dir OsString)]
    )
getXdgDirectoryList =
  tryJust recoverGetUserDirectoriesFailure . Dir.getXdgDirectoryList

-- | Prefer `XDG.BaseDirectory.*Home`.
--
-- @since 99999
getAppUserDataDirectory ::
  Path ('Rel 'False) 'Dir OsString ->
  IO (Either (V (GetUserDirectoryFailure OsString)) (Path 'Abs 'Dir OsString))
getAppUserDataDirectory =
  tryWithIF recoverGetUserDirectoryFailure . Dir.getAppUserDataDirectory @Void

-- | Prefer `XDG.UserDirectory.Common.documents`.
--
-- @since 99999
getUserDocumentsDirectory ::
  IO (Either (V (GetUserDirectoryFailure OsString)) (Path 'Abs 'Dir OsString))
getUserDocumentsDirectory =
  tryWithIF recoverGetUserDirectoryFailure $ Dir.getUserDocumentsDirectory @Void

-- | Prefer `XDG.BaseDirectory.runtimeDir`.
--
-- @since 99999
getTemporaryDirectory ::
  IO (Either (V (GetDirectoryFailure OsString)) (Path 'Abs 'Dir OsString))
getTemporaryDirectory =
  tryWithIF recoverGetDirectoryFailure $ Dir.getTemporaryDirectory @Void

-- | `System.Directory.OsPath.removeFile`, on `Path`s, with the `IOException`s
--   it’s known to raise returned as variants instead.
--
-- @since 99999
removeFile :: Path 'Abs 'File OsString -> IO (Either (V RemovalFailure) ())
removeFile = tryJust recoverRemovalFailure . Dir.removeFile

-- | `System.Directory.OsPath.renameFile`, on `Path`s, with the `IOException`s
--   it’s known to raise returned as variants instead.
--
-- @since 99999
renameFile ::
  Path 'Abs 'File OsString ->
  Path 'Abs 'File OsString ->
  IO (Either (V RenameFailure) ())
renameFile old = tryJust recoverRenameFailure . Dir.renameFile old

-- | `System.Directory.OsPath.renamePath`, on `Path`s, with the `IOException`s
--   it’s known to raise returned as variants instead.
--
-- @since 99999
renamePath ::
  (Typey typ) =>
  Path 'Abs typ OsString ->
  Path 'Abs typ OsString ->
  IO (Either (V RenameFailure) ())
renamePath source = tryJust recoverRenameFailure . Dir.renamePath source

-- |
--
--  __TODO__: It seems likely this would fail with similar cases as
--            `createDirectory`, but the docs don’t claim it.
--
-- @since 99999
copyFile ::
  Path 'Abs 'File OsString -> Path 'Abs 'File OsString -> IO ()
copyFile = Dir.copyFile

-- |
--
--  __TODO__: It seems likely this would fail with similar cases as
--            `createDirectory`, but the docs don’t claim it.
--
-- @since 99999
copyFileWithMetadata ::
  Path 'Abs 'File OsString -> Path 'Abs 'File OsString -> IO ()
copyFileWithMetadata = Dir.copyFileWithMetadata

-- |
--
--  __TODO__: It seems likely that this would fail with at least
--            `MetadataFailure`, but the docs don’t claim any.
--
-- @since 99999
getFileSize :: Path 'Abs 'File OsString -> IO Integer
getFileSize = Dir.getFileSize

-- | `System.Directory.OsPath.canonicalizePath`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
canonicalizePath ::
  (Operations OsString typ) =>
  Path 'Abs typ OsString ->
  IO (Either (V (MakeFailure OsString)) (Path 'Abs typ OsString))
canonicalizePath =
  tryWithIF recoverMakeFailure . Dir.canonicalizePath @_ @_ @Void

-- | Don’t use this. It relies on the “current directory”.
--
-- @since 99999
makeAbsolute ::
  (Operations OsString typ) =>
  Path ('Rel 'True) typ OsString ->
  IO (Either (V (MakeFailure OsString)) (Path 'Abs typ OsString))
makeAbsolute = tryWithIF recoverMakeFailure . Dir.makeAbsolute @_ @_ @Void

-- | `System.Directory.OsPath.makeRelativeToCurrentDirectory`, on `Path`s, with
--   the `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
makeRelativeToCurrentDirectory ::
  (Operations OsString typ) =>
  Path 'Abs typ OsString ->
  IO (Either (V (MakeFailure OsString)) (Path ('Rel 'True) typ OsString))
makeRelativeToCurrentDirectory =
  tryWithIF recoverMakeFailure . Dir.makeRelativeToCurrentDirectory @_ @_ @Void

-- | `System.Directory.OsPath.getSymbolicLinkTarget`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getSymbolicLinkTarget ::
  (Operations OsString typ) =>
  Path 'Abs typ OsString ->
  IO (Either (V (GetDirectoryFailure OsString)) (Path 'Rel.Any typ OsString))
getSymbolicLinkTarget =
  tryWithIF recoverGetDirectoryFailure . Dir.getSymbolicLinkTarget @_ @_ @Void

-- | `System.Directory.OsPath.createFileLink`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
createFileLink ::
  (Relative rel) =>
  Path rel 'File OsString ->
  Path 'Abs 'File OsString ->
  IO (Either (V CreateLinkFailure) ())
createFileLink old = tryJust recoverCreateLinkFailure . Dir.createFileLink old

-- | `System.Directory.OsPath.createDirectoryLink`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
createDirectoryLink ::
  (Relative rel) =>
  Path rel 'Dir OsString ->
  Path 'Abs 'Dir OsString ->
  IO (Either (V CreateLinkFailure) ())
createDirectoryLink old =
  tryJust recoverCreateLinkFailure . Dir.createDirectoryLink old

-- |
--
--  __TODO__: On POSIX, the failure could be restricted to `RemovalFailure`.
--
-- @since 99999
removeDirectoryLink ::
  Path 'Abs 'Dir OsString -> IO (Either (V DirRemovalFailure) ())
removeDirectoryLink = tryJust recoverDirRemovalFailure . Dir.removeDirectoryLink

-- | `System.Directory.OsPath.pathIsSymbolicLink`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
pathIsSymbolicLink ::
  (Typey typ) =>
  Path 'Abs typ OsString ->
  IO (Either (V MetadataFailure) Bool)
pathIsSymbolicLink = tryJust recoverMetadataFailure . Dir.pathIsSymbolicLink

-- | `System.Directory.OsPath.getPermissions`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getPermissions ::
  (Typey typ) =>
  Path 'Abs typ OsString -> IO (Either (V MetadataFailure) Dir.Permissions)
getPermissions = tryJust recoverMetadataFailure . Dir.getPermissions

-- | `System.Directory.OsPath.setPermissions`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
setPermissions ::
  (Typey typ) =>
  Path 'Abs typ OsString ->
  Dir.Permissions ->
  IO (Either (V MetadataFailure) ())
setPermissions path = tryJust recoverMetadataFailure . Dir.setPermissions path

-- |
--
--  __TODO__: It seems likely that this would fail with at least
--            `MetadataFailure`, but the docs don’t claim any.
--
-- @since 99999
copyPermissions ::
  (Typey typ, Typey typ') =>
  Path 'Abs typ OsString ->
  Path 'Abs typ' OsString ->
  IO ()
copyPermissions from = Dir.copyPermissions from

-- | `System.Directory.OsPath.getAccessTime`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getAccessTime ::
  (Typey typ) =>
  Path 'Abs typ OsString -> IO (Either (V MetadataFailure) UTCTime)
getAccessTime = tryJust recoverMetadataFailure . Dir.getAccessTime

-- | `System.Directory.OsPath.getModificationTime`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
getModificationTime ::
  (Typey typ) =>
  Path 'Abs typ OsString -> IO (Either (V MetadataFailure) UTCTime)
getModificationTime = tryJust recoverMetadataFailure . Dir.getModificationTime

-- | `System.Directory.OsPath.setAccessTime`, on `Path`s, with the
--   `IOException`s it’s known to raise returned as variants instead.
--
-- @since 99999
setAccessTime ::
  (Typey typ) =>
  Path 'Abs typ OsString -> UTCTime -> IO (Either (V MetadataFailure) ())
setAccessTime path = tryJust recoverMetadataFailure . Dir.setAccessTime path

-- |
--
--  __TODO__: On POSIX, the failure could be restricted to `MetadataFailure`.
--
-- @since 99999
setModificationTime ::
  (Typey typ) =>
  Path 'Abs typ OsString ->
  UTCTime ->
  IO (Either (V (InvalidArgument ': MetadataFailure)) ())
setModificationTime path =
  tryJust (\ioe -> liftVariant <$> recoverMetadataFailure ioe)
    . Dir.setModificationTime path
