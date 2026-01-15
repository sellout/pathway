{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- __NB__: Because of the nested @`Show` (`MP.Token` rep)@ constraints.
{-# LANGUAGE UndecidableInstances #-}
-- "System.Directory" has inconsistent Safe Haskell modes across versions. We
-- can’t conditionalize the Safe Haskell extension (because it forces Safe
-- Haskell-using consumers to conditionalize), so this silences the fact that
-- this module is inferred ‘Safe’ in some configurations.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This provides an API similar to "System.Directory", but for Pathway types.
--
-- Some differences:
--
-- - Operations mostly require absolute paths, the “current directory” is not
--   implicit (operations may _return_ relative paths, but they will be relative
--   to an argument). The reason for returning relative paths, is because it’s a
--   total operation (once we separate reparented paths at the type level) to
--   concat the relative path to the path passed in (creating the absolute path
--   that would be returned), but a partial operation to convert a returned
--   absolute path to the same relative path.
--
-- - There is no `Dir.makeAbsolute`. to do the same thing, @(`</?>` myPath)
--   `<$>` `getCurrentDirectory`@ or similar will work. This just makes all
--   paths explicit, even if they do end up relative to the “current” path.
--   __TODO__: Might be worth removing the idea of a “current” directory
--   altogether?
--
-- - Similarly, there’s no `Dir.makeRelativeToCurrentDirectory`.
--
-- - This includes some exception handlers for dealing with filesystem-specific
--   meanings of different `IOError`s.
--
-- - XDG base directories are managed via the
--   [XDG Base Directory](https://hackage.haskell.org/package/xdg-base-directory)
--   package, which is much richer than the XDG operations offered by "Dir".
--
-- One reason for enforcing the absoluteness of paths, is that paths are often
-- reported to users, and often without enough context. This tries to ensure
-- that there is at least a full path available (unless the developer makes an
-- effort to remove it).
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
    setModificationTime,
    setPermissions,
    disambiguate,
  )
where

import safe "base" Control.Applicative (pure, (<*>))
import safe "base" Control.Category ((.))
import safe "base" Control.Exception (Exception, throwIO, try)
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either (Left), either, partitionEithers)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (toList)
import safe "base" Data.Function (const, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Functor.Compose (Compose (Compose), getCompose)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe (Maybe (Nothing))
import safe "base" Data.Ord (Ord)
import safe "base" Data.Traversable (traverse)
import safe "base" Data.Tuple (curry)
import safe "base" Data.Typeable (Typeable)
import safe "base" GHC.Generics (Generic)
import safe "base" System.IO (IO)
import safe "base" Text.Show (Show)
import "directory" System.Directory qualified as Dir
import safe "megaparsec" Text.Megaparsec qualified as MP
import safe "pathway" Data.Path
  ( Path,
    Relative,
    Relativity (Abs, Rel),
    Type (Dir, File),
    Typey,
  )
import safe "pathway" Data.Path qualified as Path
import safe "pathway" Data.Path.Ambiguous qualified as Ambiguous
import safe "pathway" Data.Path.Anchored (anchored)
import safe "pathway" Data.Path.Any qualified as Any
import safe "pathway" Data.Path.Format qualified as Format
import safe "pathway" Data.Path.Functor
  ( Flip (Flip),
    Flip1 (Flip1),
    dmap,
    unflip1,
  )
import safe "pathway" Data.Path.NonReparented qualified as NonReparented
import safe "pathway" Data.Path.Parser qualified as Parser
import safe "pathway" Data.Path.Unambiguous qualified as Unambiguous
import safe "these" Data.These (These (That, These, This))
import safe "time" Data.Time.Clock (UTCTime)
import safe "transformers" Control.Monad.Trans.Class (lift)
import safe "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import safe "this" Filesystem.Path.Internal (PathComponent, PathRep, toPathRep)

fromPathRep ::
  (Ord e) =>
  PathRep ->
  Either (MP.ParseErrorBundle PathRep e) (Any.Path PathComponent)
fromPathRep = MP.parse (Parser.path Format.local) ""

handleAnchoredPath ::
  (Ord e) =>
  (Any.Path PathComponent -> Either (InternalFailure PathRep e) a) ->
  PathRep ->
  Either (InternalFailure PathRep e) a
handleAnchoredPath handler = either (Left . ParseFailure) handler . fromPathRep

absDirFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either (InternalFailure PathRep e) (Path 'Abs 'Dir PathComponent)
absDirFromPathRep =
  handleAnchoredPath \path ->
    let badRel rel = Left $ IncorrectResultRelativity (pure Abs) rel path
        badType typ = Left $ IncorrectResultType (pure Dir) typ path
        badBoth rel typ = Left $ IncorrectResultRelativityAndType (pure Abs) (pure Dir) rel typ path
     in Any.path
          pure
          (const $ badType File)
          (const $ badRel (Rel False))
          (const $ badBoth (Rel False) File)
          (const $ badRel (Rel True))
          (const $ badBoth (Rel True) File)
          path

dirFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either
    (InternalFailure PathRep e)
    (NonReparented.Path 'Dir PathComponent)
dirFromPathRep =
  handleAnchoredPath \path ->
    let badRel rel = Left $ IncorrectResultRelativity Nothing rel path
        badType typ = Left $ IncorrectResultType (pure Dir) typ path
        badBoth rel typ = Left $ IncorrectResultRelativityAndType Nothing (pure Dir) rel typ path
     in Any.path
          (pure . Compose . NonReparented.Absolute . Flip . Flip1)
          (const $ badType File)
          (pure . Compose . NonReparented.Relative . Flip . Flip1)
          (const $ badType File)
          (const $ badRel (Rel True))
          (const $ badBoth (Rel True) File)
          path

absFileFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either (InternalFailure PathRep e) (Path 'Abs 'File PathComponent)
absFileFromPathRep =
  handleAnchoredPath \path ->
    let badRel rel = Left $ IncorrectResultRelativity (pure Abs) rel path
        badType typ = Left $ IncorrectResultType (pure File) typ path
        badBoth rel typ = Left $ IncorrectResultRelativityAndType (pure Abs) (pure File) rel typ path
     in Any.path
          (const $ badType Dir)
          pure
          (const $ badBoth (Rel False) Dir)
          (const $ badRel (Rel False))
          (const $ badBoth (Rel True) Dir)
          (const $ badRel (Rel True))
          path

fileFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either (InternalFailure PathRep e) (NonReparented.Path 'File PathComponent)
fileFromPathRep =
  handleAnchoredPath \path ->
    let badRel rel = Left $ IncorrectResultRelativity Nothing rel path
        badType typ = Left $ IncorrectResultType (pure File) typ path
        badBoth rel typ = Left $ IncorrectResultRelativityAndType Nothing (pure File) rel typ path
     in Any.path
          (const $ badType Dir)
          (pure . Compose . NonReparented.Absolute . Flip . Flip1)
          (const $ badType Dir)
          (pure . Compose . NonReparented.Relative . Flip . Flip1)
          (const $ badBoth (Rel True) Dir)
          (const $ badRel (Rel True))
          path

-- absPathFromPathRep ::
--   (Ord e) =>
--   PathRep ->
--   Either (InternalFailure PathRep e) (Unambiguous.Path 'Abs PathComponent)
-- absPathFromPathRep =
--   handleAnchoredPath \path ->
--     let badRel rel = Left $ IncorrectResultRelativity (pure Abs) rel path
--      in Any.path
--           (pure . Compose . Unambiguous.Directory . Flip)
--           (pure . Compose . Unambiguous.File . Flip)
--           (const $ badRel (Rel False))
--           (const $ badRel (Rel False))
--           (const $ badRel (Rel True))
--           (const $ badRel (Rel True))
--           path

relPathFromPathRep ::
  (Ord e) =>
  PathRep ->
  Either
    (InternalFailure PathRep e)
    (Unambiguous.Path ('Rel 'False) PathComponent)
relPathFromPathRep =
  handleAnchoredPath \path ->
    let badRel rel = Left $ IncorrectResultRelativity (pure $ Rel False) rel path
     in anchored
          (const $ badRel Abs)
          (pure . Compose . dmap unflip1 . getCompose . getCompose)
          (const $ badRel (Rel True))
          path

-- |
--
--   If a `Maybe` field is `Nothing`, that means that there’s not a specific
--   allowed value, but the value that was received was disallowed.
type InternalFailure :: Kind.Type -> Kind.Type -> Kind.Type
data InternalFailure rep e
  = ParseFailure (MP.ParseErrorBundle rep e)
  | IncorrectResultRelativity (Maybe Relativity) Relativity (Any.Path rep)
  | IncorrectResultType (Maybe Type) Type (Any.Path rep)
  | IncorrectResultRelativityAndType (Maybe Relativity) (Maybe Type) Relativity Type (Any.Path rep)
  deriving stock (Generic)

deriving stock instance
  (Eq rep, Eq (MP.Token rep), Eq e) => Eq (InternalFailure rep e)

deriving stock instance
  (Show rep, Show (MP.Token rep), Show e) => Show (InternalFailure rep e)

type FakeException :: Kind.Type -> Kind.Type
newtype FakeException a = FakeException {realFailure :: a}
  deriving stock (Show)

-- | This instance is needed for @*With@ operations, that expect a function to
--   return in `IO`, without any failure type.
instance (Show a, Typeable a) => Exception (FakeException a)

type FundamentalFailure :: Kind.Type
data FundamentalFailure
  = -- | The process has insufficient privileges to perform the operation.
    --  [EROFS, EACCES]
    PermissionError
  | -- | A physical I/O error has occurred. [EIO]
    HardwareFault

type ArgumentFailure :: Kind.Type
data ArgumentFailure
  = -- | The operand is not a valid directory name. [ENAMETOOLONG, ELOOP]
    --   THIS IS AN INTERNAL ERROR IN Pathway
    InvalidArgument
  | -- | The path refers to an existing non-directory object. [EEXIST]
    InappropriateType
  | AFFF FundamentalFailure

type CreationFailure :: Kind.Type
data CreationFailure
  = -- | The operand refers to a directory that already exists. [EEXIST]
    AlreadyExistsError
  | MCF MaybeCreationFailure

type MaybeCreationFailure :: Kind.Type
data MaybeCreationFailure
  = -- | There is no path to the directory. [ENOENT, ENOTDIR]
    DoesNotExistError
  | MPCF MaybeParentCreationFailure

type MaybeParentCreationFailure :: Kind.Type
data MaybeParentCreationFailure
  = -- | Insufficient resources (virtual memory, process file descriptors,
    --   physical disk space, etc.) are available to perform the operation.
    --  [EDQUOT, ENOSPC, ENOMEM, EMLINK]
    FullError
  | MPCFAF ArgumentFailure

type DirRemovalFailure :: Kind.Type
data DirRemovalFailure
  = -- | The implementation does not support removal in this situation. [EINVAL]
    DRmFUnsupportedOperation
  | RF RemovalFailure

type RemovalFailure :: Kind.Type
data RemovalFailure
  = -- | The directory does not exist. [ENOENT, ENOTDIR]
    RmFDoesNotExistError
  | -- | Implementation-dependent constraints are not satisfied. [EBUSY,
    --   ENOTEMPTY, EEXIST]
    RmFUnsatisfiedConstraints
  | RmFAF ArgumentFailure

type RenameFailure :: Kind.Type
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

type GetFailure :: Kind.Type
data GetFailure
  = -- | The directory does not exist. [ENOENT, ENOTDIR]
    GFDoesNotExistError
  | -- | The operating system has no notion of current working directory.
    GFUnsupportedOperation
  | -- | Insufficient resources are available to perform the operation. [EMFILE,
    --   ENFILE]
    GFFullError
  | GFFF FundamentalFailure

type SetFailure :: Kind.Type
data SetFailure
  = -- | The directory does not exist. [ENOENT, ENOTDIR]
    SFDoesNotExistError
  | -- | The operating system has no notion of current working directory, or the
    --   working directory cannot be dynamically changed.
    SFUnsupportedOperation
  | SFAF ArgumentFailure

type ListFailure :: Kind.Type
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
    . fmap
      ( fmap Unambiguous.partitionPaths
          . partitionEithers
          . fmap relPathFromPathRep
      )
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
    . fmap
      ( fmap Unambiguous.partitionPaths
          . partitionEithers
          . fmap relPathFromPathRep
      )
    . Dir.getDirectoryContents
    . toPathRep

-- | This returns what the system understands as the “current” directory. This
--   is generally the directory from which the program was executed, but it
--   possible for it to have been mutated (this library doesn’t provide an API
--   to arbitrarily mutate it, but see
--   `Filesystem.Path.Compat.withCurrentDirectory` for what we _do_ offer).
--
--  __TODO__: This should include `GetFailure`.
getCurrentDirectory ::
  (Ord e) =>
  ExceptT (InternalFailure PathRep e) IO (Path 'Abs 'Dir PathComponent)
getCurrentDirectory = ExceptT $ fmap absDirFromPathRep Dir.getCurrentDirectory

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
-- > let absFiles = (</?> relFile) <$> absPaths
-- >  in mapMaybe
-- >       (\file -> if doesPathExist file then pure file else Nothing)
-- >       absFiles
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
          <=< first realFailure
      )
    . try
    . Dir.findFilesWith
      (either (throwIO . FakeException) pred . absFileFromPathRep @e)
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

-- | Checks the filesystem for an object at the provided path and returns it
--   specialized to the correct type. Returns `Nothing` if no such object
--   exists.
--
--  __TODO__: This can return _both_ a directory and file, but most (all?)
--            filesystems don’t support that. Would be great to figure out how
--            to eliminate that case based on some existential @Filesystem@ …
--            but might be reasonable to just use `Unambiguous` in the mean
--            time.
disambiguate ::
  Ambiguous.Path 'Abs PathComponent ->
  IO
    ( Maybe
        (These (Path 'Abs 'Dir PathComponent) (Path 'Abs 'File PathComponent))
    )
disambiguate path =
  let tentativeDir = Path.disambiguate path
      tentativeFile = Path.disambiguate path
   in curry
        ( \case
            (False, False) -> Nothing
            (False, True) -> pure $ That tentativeFile
            (True, False) -> pure $ This tentativeDir
            (True, True) -> pure $ These tentativeDir tentativeFile
        )
        <$> doesExist tentativeDir
        <*> doesExist tentativeFile

-- | Filesystem operations with corresponding versions for each of `'Dir` and
--  `'File`.
type FsOperations :: Type -> Kind.Constraint
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
      -- TODO: Should this always return `Abs`? What does it mean when a
      -- symbolic link has a relative target? Is it relative to the link? If so,
      -- we can resolve it before returning. But what if you move the file? it stays relative to the link? Then maybe the distinction is useful … and this probably actually needs to return @'`Rel` '`True`@ also.
      (NonReparented.Path typ PathComponent)
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
