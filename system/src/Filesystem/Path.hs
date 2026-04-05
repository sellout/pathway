-- #if MIN_VERSION_filepath(1, 4, 100)
-- #if MIN_VERSION_directory(1, 3, 8)
{-# LANGUAGE Trustworthy #-}

-- | There are various ways to use filesystem operations with Pathway. This is
--   the most opinionated. If you want something that is more closely aligned
--   with existing filesystem libraries, you may want to look at the
--   [pathway-compat-directory]() package as well as [pathway-compat-base]() and
--   [pathway-compat-filepath]().
--
--   Some differences from the [directory]() package:
-- - Operations mostly require absolute paths, the “current directory” is not
--   implicit (operations may _return_ relative paths, but they will be relative
--   to an argument). The reason for returning relative paths, is because it’s a
--   total operation (once we separate reparented paths at the type level) to
--   concat the relative path to the path passed in (creating the absolute path
--   that would be returned), but a partial operation to convert a returned
--   absolute path to the same relative path.
-- - There is no `Dir.makeAbsolute`. to do the same thing,
--   @(`</?>` myPath) `<$>` `getCurrentDirectory`@ or similar will work. This
--   just makes all paths explicit, even if they do end up relative to the
--  “current” path. __TODO__: Might be worth removing the idea of a “current”
--   directory altogether?
-- - Similarly, there’s no `Dir.makeRelativeToCurrentDirectory`.
-- - This includes some exception handlers for dealing with filesystem-specific
--   meanings of different `IOError`s.
--
--   One reason for enforcing the absoluteness of paths, is that paths are often
--   reported to users, and often without enough context. This tries to ensure
--   that there is at least a full path available (unless the developer makes an
--   effort to remove it.
module Filesystem.Path
  ( ParseFailure,
    Rep (..),
    Operations (..),
    Dir.Permissions,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Control.Monad.IO.Class (MonadIO, liftIO)
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Ord (Ord)
import safe "base" Data.String (String)
import safe "base" Data.Void (Void)
import safe "base" System.IO (Handle, IO, IOMode)
import safe "exceptions" Control.Monad.Catch (MonadMask, bracket)
import "megaparsec" Text.Megaparsec qualified as MP
import safe "pathway" Data.Path (Path, Relative, Relativity (Abs, Rel), Type (Dir, File), Typey)
import safe "pathway" Data.Path.Format qualified as Format
import safe "pathway" Data.Path.Parser qualified as Parser
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "pathway" Data.Path.Type qualified as Type (Type (Any))
import safe "pathway-compat-base" System.IO.Pathway qualified as F.IO
import safe "pathway-compat-directory" System.Directory.Caught qualified as F.Caught
import safe "pathway-compat-directory" System.Directory.Common qualified as Dir
import safe "pathway-compat-directory" System.Directory.Error
  ( CreateLinkFailure,
    CreationFailure,
    CurrentDirectoryFailure,
    DirRemovalFailure,
    GetDirectoryFailure,
    GetUserDirectoryFailure,
    InternalFailure,
    ListFailure,
    MakeFailure,
    MaybeCreationFailure,
    MaybeParentCreationFailure,
    MetadataFailure,
    RenameFailure,
  )
import safe "pathway-compat-directory" System.Directory.OsPath.Caught qualified as O.Caught
import safe "pathway-compat-directory" System.Directory.OsPath.Thin qualified as O.Path
  ( doesDirectoryExist,
    doesFileExist,
    findExecutable,
    findFiles,
    findFilesWith,
  )
import safe "pathway-compat-directory" System.Directory.Thin qualified as F.Path
  ( doesDirectoryExist,
    doesFileExist,
    findExecutable,
    findFiles,
    findFilesWith,
  )
import safe "pathway-compat-directory" System.IO.Error
  ( AlreadyExistsError,
    FullError,
    InvalidArgument,
  )
import safe "pathway-compat-file-io" System.File.OsPath.Pathway qualified as O.IO
import safe "pathway-compat-filepath" Common.OsPath qualified as OsPath
import safe "pathway-compat-filepath" System.FilePath.Thin qualified as FP
import safe "pathway-compat-filepath" System.OsPath.Pathway (OsString)
import safe "pathway-compat-filepath" System.OsPath.Thin qualified as OP
import safe "time" Data.Time.Clock (UTCTime)
import "variant" Data.Variant (V, liftVariant)

type ParseFailure :: Kind.Type -> Kind.Type
type ParseFailure rep = MP.ParseErrorBundle rep Void

type Rep :: Kind.Type -> Kind.Constraint
class Rep rep where
  parseDirectory :: rep -> Either (ParseFailure rep) (Path 'Rel.Any 'Dir rep)
  parsePath :: rep -> Either (ParseFailure rep) (Path 'Rel.Any 'Type.Any rep)
  splitSearchPath ::
    rep -> [Either (InternalFailure rep Void) (Path 'Rel.Any 'Dir rep)]
  canonicalizePath ::
    (Dir.Operations rep typ) =>
    Path 'Abs typ rep -> IO (Either (V (MakeFailure rep)) (Path 'Abs typ rep))
  copyPermissions ::
    (Typey typ, Typey typ') => Path 'Abs typ rep -> Path 'Abs typ' rep -> IO ()
  createDirectory :: Path 'Abs 'Dir rep -> IO (Either (V CreationFailure) ())
  createDirectoryIfMissing ::
    Path 'Abs 'Dir rep -> IO (Either (V MaybeCreationFailure) ())
  createDirectoryWithParentsIfMissing ::
    Path 'Abs 'Dir rep -> IO (Either (V MaybeParentCreationFailure) ())
  getAccessTime ::
    (Typey typ) => Path 'Abs typ rep -> IO (Either (V MetadataFailure) UTCTime)
  getCurrentDirectory ::
    IO
      ( Either
          (V (FullError ': InternalFailure rep Void ': CurrentDirectoryFailure))
          (Path 'Abs 'Dir rep)
      )
  getHomeDirectory ::
    IO (Either (V (GetUserDirectoryFailure rep)) (Path 'Abs 'Dir rep))
  getModificationTime ::
    (Typey typ) => Path 'Abs typ rep -> IO (Either (V MetadataFailure) UTCTime)
  getPermissions ::
    (Typey typ) =>
    Path 'Abs typ rep -> IO (Either (V MetadataFailure) Dir.Permissions)
  getSymbolicLinkTarget ::
    (Dir.Operations rep typ) =>
    Path 'Abs typ rep ->
    IO (Either (V (GetDirectoryFailure rep)) (Path 'Rel.Any typ rep))
  listDirectory ::
    (Ord e) =>
    Path 'Abs 'Dir rep ->
    -- |
    --
    --  __TODO__: This should be @`Unambiguous` ('`Rel` '`False`) `OsString`@ once #18
    --            lands.
    IO
      ( Either
          (V ListFailure)
          [ Either
              (InternalFailure rep e)
              ( Either
                  (Path ('Rel 'False) 'Dir rep)
                  (Path ('Rel 'False) 'File rep)
              )
          ]
      )
  removeDirectoryRecursive ::
    Path 'Abs 'Dir rep -> IO (Either (V DirRemovalFailure) ())
  removePathForcibly ::
    (Typey typ) => Path 'Abs typ rep -> IO (Either (V DirRemovalFailure) ())
  setAccessTime ::
    (Typey typ) =>
    Path 'Abs typ rep -> UTCTime -> IO (Either (V MetadataFailure) ())
  setModificationTime ::
    (Typey typ) =>
    Path 'Abs typ rep ->
    UTCTime ->
    IO (Either (V (InvalidArgument ': MetadataFailure)) ())
  setPermissions ::
    (Typey typ) =>
    Path 'Abs typ rep ->
    Dir.Permissions ->
    IO (Either (V MetadataFailure) ())
  findExecutable ::
    (Ord e) =>
    Path ('Rel 'True) 'File rep ->
    IO (Maybe (Either (InternalFailure rep e) (Path 'Abs 'File rep)))
  findFiles ::
    (Ord e) =>
    [Path 'Abs 'Dir rep] ->
    Path ('Rel 'True) 'File rep ->
    IO [Either (InternalFailure rep e) (Path 'Abs 'File rep)]
  findFilesWith ::
    (Ord e) =>
    (Path 'Abs 'File rep -> IO Bool) ->
    [Path 'Abs 'Dir rep] ->
    Path ('Rel 'True) 'File rep ->
    IO [Either (InternalFailure rep e) (Path 'Abs 'File rep)]
  withFile ::
    (MonadIO m, MonadMask m) =>
    Path 'Abs 'File rep -> IOMode -> (Handle -> m r) -> m r

-- | A generalization of `System.IO.withFile` that supports `MonadIO`.
withFile' ::
  (MonadIO m, MonadMask m) =>
  (Path 'Abs 'File rep -> IOMode -> IO Handle) ->
  Path 'Abs 'File rep ->
  IOMode ->
  (Handle -> m r) ->
  m r
withFile' openFile filePath mode =
  bracket (liftIO $ openFile filePath mode) $ liftIO . F.IO.hClose

-- | Filesystem operations with corresponding versions for each of `'Dir` and
--  `'File`.
type Operations :: Kind.Type -> Type -> Kind.Constraint
class Operations rep typ where
  createLink ::
    (Relative rel) =>
    Path rel typ rep ->
    Path 'Abs typ rep ->
    IO (Either (V CreateLinkFailure) ())
  doesExist :: Path 'Abs typ rep -> IO Bool
  remove :: Path 'Abs typ rep -> IO (Either (V DirRemovalFailure) ())

  -- |
  --
  --  __TODO__: On POSIX, the failure could be restricted to `RemovalFailure`.
  removeLink :: Path 'Abs typ rep -> IO (Either (V DirRemovalFailure) ())

  rename ::
    Path 'Abs typ rep ->
    Path 'Abs typ rep ->
    IO (Either (V (AlreadyExistsError ': RenameFailure)) ())

instance Rep String where
  parseDirectory = MP.parse (Parser.directory Format.local) ""
  parsePath = MP.parse (Parser.path Format.local) ""
  splitSearchPath = FP.splitSearchPath
  canonicalizePath = F.Caught.canonicalizePath
  copyPermissions = F.Caught.copyPermissions
  createDirectory = F.Caught.createDirectory
  createDirectoryIfMissing = F.Caught.createDirectoryIfMissing
  createDirectoryWithParentsIfMissing = F.Caught.createDirectoryWithParentsIfMissing
  getAccessTime = F.Caught.getAccessTime
  getCurrentDirectory = F.Caught.getCurrentDirectory
  getHomeDirectory = F.Caught.getHomeDirectory
  getModificationTime = F.Caught.getModificationTime
  getPermissions = F.Caught.getPermissions
  getSymbolicLinkTarget = F.Caught.getSymbolicLinkTarget
  listDirectory = F.Caught.listDirectory
  removeDirectoryRecursive = F.Caught.removeDirectoryRecursive
  removePathForcibly = F.Caught.removePathForcibly
  setAccessTime = F.Caught.setAccessTime
  setModificationTime = F.Caught.setModificationTime
  setPermissions = F.Caught.setPermissions
  findExecutable = F.Path.findExecutable
  findFiles = F.Path.findFiles
  findFilesWith = F.Path.findFilesWith
  withFile = withFile' F.IO.openFile

instance Operations String 'Dir where
  createLink = F.Caught.createDirectoryLink
  doesExist = F.Path.doesDirectoryExist
  remove = F.Caught.removeDirectory
  removeLink = F.Caught.removeDirectoryLink
  rename old = F.Caught.renameDirectory old

instance Operations String 'File where
  createLink = F.Caught.createFileLink
  doesExist = F.Path.doesFileExist
  remove = fmap (first liftVariant) . F.Caught.removeFile
  removeLink = fmap (first liftVariant) . F.Caught.removeFile
  rename old = fmap (first liftVariant) . F.Caught.renameFile old

instance Rep OsString where
  parseDirectory = MP.parse (Parser.directory OsPath.localFormat) ""
  parsePath = MP.parse (Parser.path OsPath.localFormat) ""
  splitSearchPath = OP.splitSearchPath
  canonicalizePath = O.Caught.canonicalizePath
  copyPermissions = O.Caught.copyPermissions
  createDirectory = O.Caught.createDirectory
  createDirectoryIfMissing = O.Caught.createDirectoryIfMissing
  createDirectoryWithParentsIfMissing = O.Caught.createDirectoryWithParentsIfMissing
  getAccessTime = O.Caught.getAccessTime
  getCurrentDirectory = O.Caught.getCurrentDirectory
  getHomeDirectory = O.Caught.getHomeDirectory
  getModificationTime = O.Caught.getModificationTime
  getPermissions = O.Caught.getPermissions
  getSymbolicLinkTarget = O.Caught.getSymbolicLinkTarget
  listDirectory = O.Caught.listDirectory
  removeDirectoryRecursive = O.Caught.removeDirectoryRecursive
  removePathForcibly = O.Caught.removePathForcibly
  setAccessTime = O.Caught.setAccessTime
  setModificationTime = O.Caught.setModificationTime
  setPermissions = O.Caught.setPermissions
  findExecutable = O.Path.findExecutable
  findFiles = O.Path.findFiles
  findFilesWith = O.Path.findFilesWith
  withFile = withFile' O.IO.openFile

instance Operations OsString 'Dir where
  createLink = O.Caught.createDirectoryLink
  doesExist = O.Path.doesDirectoryExist
  remove = O.Caught.removeDirectory
  removeLink = O.Caught.removeDirectoryLink
  rename old = O.Caught.renameDirectory old

instance Operations OsString 'File where
  createLink = O.Caught.createFileLink
  doesExist = O.Path.doesFileExist
  remove = fmap (first liftVariant) . O.Caught.removeFile
  removeLink = fmap (first liftVariant) . O.Caught.removeFile
  rename old = fmap (first liftVariant) . O.Caught.renameFile old

-- -- | Checks the filesystem for an object at the provided path and returns it
-- --   specialized to the correct type. Returns `Nothing` if no such object
-- --   exists.
-- disambiguate ::
--   AmbiguousPath 'Abs PathComponent -> IO (Maybe (Path 'Abs _ PathComponent))
-- disambiguate = _
