{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- __NB__: Because of the nested @`Show` (`MP.Token` rep)@ constraints.
{-# LANGUAGE UndecidableInstances #-}
-- __NB__: "System.Directory" isn’t safe from directory-1.3.8 (GHC 9.6).
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- | This provides an API similar to "System.Directory", but for Pathway types.
--
--   Some differences:
-- - operations mostly require absolute paths, the “current directory” is not
--   implicit (operations may _return_ relative paths, but they will be relative
--   to an argument). The reason for returning relative paths, is because it’s a
--   total operation (once we separate reparented paths at the type level) to
--   concat the relative path to the path passed in (creating the absolute path
--   that would be returned), but a partial operation to convert a returned
--   absolute path to the same relative path.
-- - there is no `Dir.makeAbsolute`, to do the same thing,
--   @(`</?>` myPath) `<$>` `getCurrentDirectory`@ or similar will work. This
--   just makes all paths explicit, even if they do end up relative to the
--  “current” path. __TODO__: Might be worth removing the idea of a “current”
--   directory altogether?
-- - similarly, no `makeRelativeToCurrentDirectory`
-- - this includes some exception handlers for dealing with filesystem-specific
--   meanings of different `IOError`s.
--
--   One reason for enforcing the absoluteness of paths, is that paths are often
--   reported to users, and often without enough context. This tries to ensure
--   that there is at least a full path available (unless the developer makes an
--   effort to remove it.
module Filesystem.Path
  ( FundamentalFailure (..),
    ArgumentFailure (..),
    CreationFailure (..),
    MaybeCreationFailure (..),
    MaybeParentCreationFailure (..),
    DirRemovalFailure (..),
    RemovalFailure (..),
    RenameFailure (..),
    GetFailure (..),
    SetFailure (..),
    ListFailure (..),
    FsOperations
      ( canonicalizePath,
        createLink,
        doesExist,
        getSymbolicLinkTarget,
        remove,
        removeLink,
        rename
      ),
    PathComponent,
    PathRep,
    copyPermissions,
    createDirectory,
    createDirectoryIfMissing,
    createDirectoryWithParentsIfMissing,
    findExecutable,
    findFiles,
    findFilesWith,
    getAccessTime,
    getCurrentDirectory,
    getDirectoryContents,
    getModificationTime,
    getPermissions,
    listDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    setAccessTime,
    setCurrentDirectory,
    setModificationTime,
    setPermissions,
    withCurrentDirectory,
  )
where

import safe "base" Control.Applicative (Applicative (pure))
import safe "base" Control.Category (Category ((.)))
import safe "base" Control.Exception (Exception, throwIO)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left), either, partitionEithers)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable (toList))
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor (fmap), (<$>))
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Ord (Ord)
import safe "base" Data.String (String)
import safe "base" Data.Traversable (Traversable (traverse))
import safe "base" Data.Typeable (Typeable)
import safe "base" GHC.Generics (Generic)
import safe "base" System.IO (IO)
import safe "base" Text.Show (Show)
import qualified "directory" System.Directory as Dir
import safe "filepath" System.FilePath (FilePath)
import safe qualified "megaparsec" Text.Megaparsec as MP
import safe "pathway" Data.Path
  ( Anchored (AbsDir, AbsFile, RelDir, RelFile, ReparentedDir, ReparentedFile),
    AnyPath,
    Path,
    Pathy,
    Relative,
    Relativity (Abs, Any, Rel),
    Type (Dir, File, Pathic),
    Typey,
    anchor,
    toText,
    unanchor,
  )
import safe qualified "pathway" Data.Path.Format as Format
import safe qualified "pathway" Data.Path.Parser as Parser
import safe "time" Data.Time.Clock (UTCTime)
import safe "transformers" Control.Monad.Trans.Class (MonadTrans (lift))
import safe "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))

-- |
--
--  __NB__: This is currently an alias for `FilePath`, but it is intended to
--          switch to `OsPath` in future (for GHCs recent enough to have it),
--          once there is code in place to avoid converting back and forth for
--          parsing, etc.
type PathRep = FilePath

-- | In both the `FilePath` and `OsPath` versions, this represents the same type
--   as `PathRep`, but the distinction indicates whether a path (where, for
--   example, literal backslashes need to be escaped for POSIX) or a single
--   component (where no characters are escaped) is held.
type PathComponent = String

fromPathRep ::
  (Ord e) =>
  PathRep ->
  Either (MP.ParseErrorBundle PathRep e) (Anchored PathComponent)
fromPathRep = fmap anchor . MP.parse (Parser.path Format.local) ""

handleAnchoredPath ::
  (Ord e) =>
  (Anchored PathComponent -> Either (InternalFailure PathRep e) a) ->
  PathRep ->
  Either (InternalFailure PathRep e) a
handleAnchoredPath handler = either (Left . ParseFailure) handler . fromPathRep

absDirFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either (InternalFailure PathRep e) (Path 'Abs 'Dir PathComponent)
absDirFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs Dir rel typ
   in handleAnchoredPath \case
        AbsDir path -> pure path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

dirFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either
    (InternalFailure PathRep e)
    ( Either
        (Path 'Abs 'Dir PathComponent)
        (Path ('Rel 'False) 'Dir PathComponent)
    )
dirFromPathRep =
  let badType rel typ = Left . IncorrectResultType Any Dir rel typ
   in handleAnchoredPath \case
        AbsDir path -> pure $ Left path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> pure $ pure path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

absFileFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either (InternalFailure PathRep e) (Path 'Abs 'File PathComponent)
absFileFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs File rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> pure path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> badType (Rel False) File $ unanchor path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

fileFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either
    (InternalFailure PathRep e)
    ( Either
        (Path 'Abs 'File PathComponent)
        (Path ('Rel 'False) 'File PathComponent)
    )
fileFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs File rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> pure $ Left path
        RelDir path -> badType (Rel False) Dir $ unanchor path
        RelFile path -> pure $ pure path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

-- absPathFromPathRep ::
--   (Ord e) =>
--   PathRep ->
--   Either
--     (InternalFailure PathRep e)
--     (Either (Path 'Abs 'Dir PathComponent) (Path 'Abs 'File PathComponent))
-- absPathFromPathRep =
--   let badType rel typ = Left . IncorrectResultType Abs Pathic rel typ
--    in handleAnchoredPath \case
--             AbsDir path -> pure $ Left path
--             AbsFile path -> pure $ pure path
--             RelDir path -> badType (Rel False) Dir $ unanchor path
--             RelFile path -> badType (Rel False) File $ unanchor path
--             ReparentedDir path -> badType (Rel True) Dir $ unanchor path
--             ReparentedFile path -> badType (Rel True) File $ unanchor path

relPathFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either
    (InternalFailure PathRep e)
    ( Either
        (Path ('Rel 'False) 'Dir PathComponent)
        (Path ('Rel 'False) 'File PathComponent)
    )
relPathFromPathRep =
  let badType rel typ = Left . IncorrectResultType Abs Pathic rel typ
   in handleAnchoredPath \case
        AbsDir path -> badType Abs Dir $ unanchor path
        AbsFile path -> badType Abs File $ unanchor path
        RelDir path -> pure $ Left path
        RelFile path -> pure $ pure path
        ReparentedDir path -> badType (Rel True) Dir $ unanchor path
        ReparentedFile path -> badType (Rel True) File $ unanchor path

toPathRep :: (Pathy rel typ) => Path rel typ PathComponent -> PathRep
toPathRep = toText Format.local

data InternalFailure rep e
  = ParseFailure (MP.ParseErrorBundle rep e)
  | IncorrectResultType Relativity Type Relativity Type (AnyPath rep)
  deriving stock (Generic)

deriving stock instance
  (Eq rep, Eq (MP.Token rep), Eq e) => Eq (InternalFailure rep e)

deriving stock instance
  (Show rep, Show (MP.Token rep), Show e) => Show (InternalFailure rep e)

-- | This instance is needed for @*With@ operations, that expect a function to
--   return in `IO`, without any failure type.
instance
  (Show rep, Typeable rep, Show (MP.Token rep), Show e, Typeable e) =>
  Exception (InternalFailure rep e)

data FundamentalFailure
  = -- | The process has insufficient privileges to perform the operation.
    --  [EROFS, EACCES]
    PermissionError
  | -- | A physical I/O error has occurred. [EIO]
    HardwareFault

data ArgumentFailure
  = -- | The operand is not a valid directory name. [ENAMETOOLONG, ELOOP]
    --   THIS IS AN INTERNAL ERROR IN Pathway
    InvalidArgument
  | -- | The path refers to an existing non-directory object. [EEXIST]
    InappropriateType
  | AFFF FundamentalFailure

data CreationFailure
  = -- | The operand refers to a directory that already exists. [EEXIST]
    AlreadyExistsError
  | MCF MaybeCreationFailure

data MaybeCreationFailure
  = -- | There is no path to the directory. [ENOENT, ENOTDIR]
    DoesNotExistError
  | MPCF MaybeParentCreationFailure

data MaybeParentCreationFailure
  = -- | Insufficient resources (virtual memory, process file descriptors,
    --   physical disk space, etc.) are available to perform the operation.
    --  [EDQUOT, ENOSPC, ENOMEM, EMLINK]
    FullError
  | MPCFAF ArgumentFailure

data DirRemovalFailure
  = -- | The implementation does not support removal in this situation. [EINVAL]
    DRmFUnsupportedOperation
  | RF RemovalFailure

data RemovalFailure
  = -- | The directory does not exist. [ENOENT, ENOTDIR]
    RmFDoesNotExistError
  | -- | Implementation-dependent constraints are not satisfied. [EBUSY,
    --   ENOTEMPTY, EEXIST]
    RmFUnsatisfiedConstraints
  | RmFAF ArgumentFailure

data RenameFailure
  = -- | The original directory does not exist, or there is no path to the
    --   target. [ENOENT, ENOTDIR]
    RnFDoesNotExistError
  | -- | Implementation-dependent constraints are not satisfied. [EBUSY,
    --   ENOTEMPTY, EEXIST]
    RnFUnsatisfiedConstraints
  | -- | The implementation does not support renaming in this situation.
    --  [EINVAL, EXDEV]
    RnFUnsupportedOperation
  | -- | Insufficient resources are available to perform the operation. [EDQUOT,
    --   ENOSPC, ENOMEM, EMLINK]
    RnFFullError
  | DRFAF ArgumentFailure

data GetFailure
  = -- | The directory does not exist. [ENOENT, ENOTDIR]
    GFDoesNotExistError
  | -- | The operating system has no notion of current working directory.
    GFUnsupportedOperation
  | -- | Insufficient resources are available to perform the operation. [EMFILE,
    --   ENFILE]
    GFFullError
  | GFFF FundamentalFailure

data SetFailure
  = -- | The directory does not exist. [ENOENT, ENOTDIR]
    SFDoesNotExistError
  | -- | The operating system has no notion of current working directory, or the
    --   working directory cannot be dynamically changed.
    SFUnsupportedOperation
  | SFAF ArgumentFailure

data ListFailure
  = -- | The directory does not exist. [ENOENT, ENOTDIR]
    LFDoesNotExistError
  | -- | Insufficient resources are available to perform the operation. [EMFILE,
    --   ENFILE]
    LFFullError
  | LFAF ArgumentFailure

createDirectory :: Path 'Abs 'Dir PathComponent -> ExceptT CreationFailure IO ()
createDirectory = lift . Dir.createDirectory . toPathRep

-- | Can perhaps unify this and the following definition depending on how we
--   handle errors.
createDirectoryIfMissing ::
  Path 'Abs 'Dir PathComponent -> ExceptT MaybeCreationFailure IO ()
createDirectoryIfMissing = lift . Dir.createDirectoryIfMissing False . toPathRep

createDirectoryWithParentsIfMissing ::
  Path 'Abs 'Dir PathComponent -> ExceptT MaybeParentCreationFailure IO ()
createDirectoryWithParentsIfMissing =
  lift . Dir.createDirectoryIfMissing True . toPathRep

removeDirectoryRecursive :: Path 'Abs 'Dir PathComponent -> IO ()
removeDirectoryRecursive = Dir.removeDirectoryRecursive . toPathRep

removePathForcibly :: Path 'Abs 'Dir PathComponent -> IO ()
removePathForcibly = Dir.removePathForcibly . toPathRep

listDirectory ::
  (Ord e) =>
  Path 'Abs 'Dir PathComponent ->
  ExceptT
    ListFailure
    IO
    ( [InternalFailure PathRep e],
      ( [Path ('Rel 'False) 'Dir PathComponent],
        [Path ('Rel 'False) 'File PathComponent]
      )
    )
listDirectory =
  lift
    . fmap (fmap partitionEithers . partitionEithers . fmap relPathFromPathRep)
    . Dir.listDirectory
    . toPathRep

-- |
--
--  __TODO__: This includes @./@. and @../@ in its results, and concatenating
--            those onto an absolute path is partial, so consider the right
--            thing to do:
--          - drop this operation
--          - return paths in `Maybe`
--          - filter out @../@ in the case of the root dir (or if it fails,
--            which should be only that case)
getDirectoryContents ::
  (Ord e) =>
  Path 'Abs 'Dir PathComponent ->
  ExceptT
    ListFailure
    IO
    ( [InternalFailure PathRep e],
      -- FIXME: The first element should be `Rel` `True`, but don’t yet have a
      --        function for that.
      ( [Path ('Rel 'False) 'Dir PathComponent],
        [Path ('Rel 'False) 'File PathComponent]
      )
    )
getDirectoryContents =
  lift
    . fmap (fmap partitionEithers . partitionEithers . fmap relPathFromPathRep)
    . Dir.getDirectoryContents
    . toPathRep

-- GetFailure
getCurrentDirectory ::
  (Ord e) =>
  ExceptT (InternalFailure PathRep e) IO (Path 'Abs 'Dir PathComponent)
getCurrentDirectory = ExceptT $ fmap absDirFromPathRep Dir.getCurrentDirectory

setCurrentDirectory :: Path 'Abs 'Dir PathComponent -> ExceptT SetFailure IO ()
setCurrentDirectory = lift . Dir.setCurrentDirectory . toPathRep

withCurrentDirectory ::
  Path 'Abs 'Dir PathComponent ->
  IO a ->
  ExceptT (Either GetFailure SetFailure) IO a
withCurrentDirectory newCurDir =
  lift . Dir.withCurrentDirectory (toPathRep newCurDir)

-- __TODO__: Fill in a lot of stuff

-- | This allows a relative file, because 1. the intent is to resolve the
--   absolute path, and 2. the paths searched are platform-specific, so
--   difficult to extract portably (especially when trying to match a search
--   function).
findExecutable ::
  (Ord e) =>
  Path ('Rel 'True) 'File PathComponent ->
  ExceptT (InternalFailure PathRep e) IO (Maybe (Path 'Abs 'File PathComponent))
findExecutable =
  ExceptT . fmap (traverse absFileFromPathRep) . Dir.findExecutable . toPathRep

findFiles ::
  (Ord e) =>
  [Path 'Abs 'Dir PathComponent] ->
  Path ('Rel 'True) 'File PathComponent ->
  IO [Either (InternalFailure PathRep e) (Path 'Abs 'File PathComponent)]
findFiles dirs =
  fmap (fmap absFileFromPathRep)
    . Dir.findFiles (toPathRep <$> dirs)
    . toPathRep

-- |
--   This concats the file onto each of the directories, then checks if it
--   exists. I wonder if there’s a benefit to this over
--
--  let absFiles = fmap (</?> relFile) absPaths
--   in catMaybes $
--        (\file -> if doesPathExist file then pure file else Nothing)
--          <$> absFiles
--
--   I think this might be the way to get OS-style handling of symlinks, as
--   opposed to the agnostic approach in the pure functions
findFilesWith ::
  forall e.
  (Ord e, Show e, Typeable e) =>
  (Path 'Abs 'File PathComponent -> IO Bool) ->
  [Path 'Abs 'Dir PathComponent] ->
  Path ('Rel 'True) 'File PathComponent ->
  ExceptT [InternalFailure PathRep e] IO [Path 'Abs 'File PathComponent]
findFilesWith pred dirs =
  ExceptT
    . fmap
      ( ( \case
            ([], files) -> pure files
            (errs, _) -> Left errs
        )
          . partitionEithers
          . fmap absFileFromPathRep
      )
    . Dir.findFilesWith
      (either throwIO pred . absFileFromPathRep @e)
      (toList $ toPathRep <$> dirs)
    . toPathRep

getPermissions ::
  (Typey typ) => Path 'Abs typ PathComponent -> IO Dir.Permissions
getPermissions = Dir.getPermissions . toPathRep

setPermissions ::
  (Typey typ) => Path 'Abs typ PathComponent -> Dir.Permissions -> IO ()
setPermissions = Dir.setPermissions . toPathRep

copyPermissions ::
  (Typey typ, Typey typ') =>
  Path 'Abs typ PathComponent ->
  Path 'Abs typ' PathComponent ->
  IO ()
copyPermissions from = Dir.copyPermissions (toPathRep from) . toPathRep

getAccessTime :: (Typey typ) => Path 'Abs typ PathComponent -> IO UTCTime
getAccessTime = Dir.getAccessTime . toPathRep

getModificationTime :: (Typey typ) => Path 'Abs typ PathComponent -> IO UTCTime
getModificationTime = Dir.getModificationTime . toPathRep

setAccessTime :: (Typey typ) => Path 'Abs typ PathComponent -> UTCTime -> IO ()
setAccessTime = Dir.setAccessTime . toPathRep

setModificationTime ::
  (Typey typ) => Path 'Abs typ PathComponent -> UTCTime -> IO ()
setModificationTime = Dir.setModificationTime . toPathRep

-- -- | Checks the filesystem for an object at the provided path and returns it
-- --   specialized to the correct type. Returns `Nothing` if no such object
-- --   exists.
-- disambiguate ::
--   AmbiguousPath 'Abs PathComponent -> IO (Maybe (Path 'Abs _ PathComponent))
-- disambiguate = _

-- | Filesystem operations with corresponding versions for each of `'Dir` and
--  `'File`.
class FsOperations typ where
  canonicalizePath ::
    (Ord e) =>
    Path 'Abs typ PathComponent ->
    ExceptT (InternalFailure PathRep e) IO (Path 'Abs typ PathComponent)
  createLink ::
    (Relative rel) =>
    Path rel typ PathComponent ->
    Path 'Abs typ PathComponent ->
    IO ()
  doesExist :: Path 'Abs typ PathComponent -> IO Bool
  getSymbolicLinkTarget ::
    (Ord e) =>
    Path 'Abs typ PathComponent ->
    ExceptT
      (InternalFailure PathRep e)
      IO
      ( Either
          (Path 'Abs typ PathComponent)
          (Path ('Rel 'False) typ PathComponent)
      )
  remove :: Path 'Abs typ PathComponent -> ExceptT DirRemovalFailure IO ()
  removeLink :: Path 'Abs typ PathComponent -> IO ()
  rename ::
    Path 'Abs typ PathComponent ->
    Path 'Abs typ PathComponent ->
    ExceptT RenameFailure IO ()

instance FsOperations 'Dir where
  canonicalizePath =
    ExceptT . fmap absDirFromPathRep . Dir.canonicalizePath . toPathRep
  createLink existing = Dir.createDirectoryLink (toPathRep existing) . toPathRep
  doesExist = Dir.doesDirectoryExist . toPathRep
  getSymbolicLinkTarget =
    ExceptT . fmap dirFromPathRep . Dir.getSymbolicLinkTarget . toPathRep
  remove = lift . Dir.removeDirectory . toPathRep
  removeLink = Dir.removeDirectoryLink . toPathRep
  rename old = lift . Dir.renameDirectory (toPathRep old) . toPathRep

instance FsOperations 'File where
  canonicalizePath =
    ExceptT . fmap absFileFromPathRep . Dir.canonicalizePath . toPathRep
  createLink existing = Dir.createFileLink (toPathRep existing) . toPathRep
  doesExist = Dir.doesFileExist . toPathRep
  getSymbolicLinkTarget =
    ExceptT . fmap fileFromPathRep . Dir.getSymbolicLinkTarget . toPathRep
  remove = lift . Dir.removeFile . toPathRep
  removeLink = Dir.removeFile . toPathRep
  rename old = lift . Dir.renameFile (toPathRep old) . toPathRep
