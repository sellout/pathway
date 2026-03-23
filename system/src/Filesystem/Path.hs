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
  ( module Dir,
    FsOperations,
    createLink,
    doesExist,
    remove,
    removeLink,
    rename,
    PathComponent,
    PathRep,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool)
import safe "base" Data.Either (Either)
import safe "base" Data.Functor (fmap)
import safe "base" Data.Kind qualified as Kind
import safe "base" System.IO (IO)
import safe "pathway" Data.Path (Path, Relative, Relativity (Abs), Type (Dir, File))
import safe "pathway-compat-directory" System.Directory.Caught as Dir
  ( CreateLinkFailure,
    DirRemovalFailure,
    InternalFailure (IncorrectResultType, ParseFailure),
    RenameFailure,
    canonicalizePath,
    copyPermissions,
    createDirectory,
    createDirectoryIfMissing,
    createDirectoryWithParentsIfMissing,
    getAccessTime,
    getCurrentDirectory,
    getHomeDirectory,
    getModificationTime,
    getPermissions,
    getSymbolicLinkTarget,
    listDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    setAccessTime,
    setModificationTime,
    setPermissions,
  )
import safe "pathway-compat-directory" System.Directory.Caught qualified as Dir
import safe "pathway-compat-directory" System.Directory.Thin as Dir
  ( doesDirectoryExist,
    doesFileExist,
    findExecutable,
    findFiles,
    findFilesWith,
  )
import "variant" Data.Variant (V, liftVariant)
import safe "this" Filesystem.Path.Internal (PathComponent, PathRep)

-- -- | Checks the filesystem for an object at the provided path and returns it
-- --   specialized to the correct type. Returns `Nothing` if no such object
-- --   exists.
-- disambiguate ::
--   AmbiguousPath 'Abs PathComponent -> IO (Maybe (Path 'Abs _ PathComponent))
-- disambiguate = _

-- | Filesystem operations with corresponding versions for each of `'Dir` and
--  `'File`.
type FsOperations :: Type -> Kind.Constraint
class FsOperations typ where
  createLink ::
    (Relative rel) =>
    Path rel typ PathComponent ->
    Path 'Abs typ PathComponent ->
    IO (Either (V CreateLinkFailure) ())
  doesExist :: Path 'Abs typ PathComponent -> IO Bool
  remove :: Path 'Abs typ PathComponent -> IO (Either (V DirRemovalFailure) ())

  -- |
  --
  --  __TODO__: On POSIX, the failure could be restricted to `RemovalFailure`.
  removeLink :: Path 'Abs typ PathComponent -> IO (Either (V DirRemovalFailure) ())

  rename ::
    Path 'Abs typ PathComponent ->
    Path 'Abs typ PathComponent ->
    IO (Either (V (Dir.AlreadyExistsError ': RenameFailure)) ())

instance FsOperations 'Dir where
  createLink = Dir.createDirectoryLink
  doesExist = Dir.doesDirectoryExist
  remove = Dir.removeDirectory
  removeLink = Dir.removeDirectoryLink
  rename old = Dir.renameDirectory old

instance FsOperations 'File where
  createLink = Dir.createFileLink
  doesExist = Dir.doesFileExist
  remove = fmap (first liftVariant) . Dir.removeFile
  removeLink = fmap (first liftVariant) . Dir.removeFile
  rename old = fmap (first liftVariant) . Dir.renameFile old
