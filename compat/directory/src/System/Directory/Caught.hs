{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- WAIT: "NoRecursion" doesn’t currently provide a way to ignore names like
--       `$dPopVariant_aei8`, but it should.
{-# OPTIONS_GHC -fplugin-opt NoRecursion:allow-recursion:true #-}

-- |
--
--  __TODO__: If we reimplement the directory package rather than wrap it, we
--            can capture failures at a lower level, and be more precise about
--            which ones can occur in each operation, rather than relying on
--            docs.
module System.Directory.Caught
  ( InternalFailure (..),
    FsOperations,
    Dir.Permissions,
    PermissionError (PermissionError),
    HardwareFault (HardwareFault),
    InvalidArgument (InvalidArgument),
    InappropriateType (InappropriateType),
    FullError (FullError),
    DoesNotExistError (DoesNotExistError),
    AlreadyExistsError (AlreadyExistsError),
    UnsupportedOperation (UnsupportedOperation),
    UnsatisfiedConstraints (UnsatisfiedConstraints),
    MaybeParentCreationFailure,
    MaybeCreationFailure,
    CreationFailure,
    RemovalFailure,
    DirRemovalFailure,
    ListFailure,
    GetDirectoryFailure,
    GetUserDirectoryFailure,
    GetUserDirectoriesFailure,
    CurrentDirectoryFailure,
    SetCurrentDirectoryFailure,
    RenameFailure,
    MakeFailure,
    CreateLinkFailure,
    MetadataFailure,
    createDirectory,
    createDirectoryIfMissing,
    createDirectoryWithParentsIfMissing,
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

import safe "base" Control.Applicative (empty, pure)
import safe "base" Control.Category (Category ((.)), (.))
import safe "base" Control.Exception (tryJust)
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (Either)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor (fmap), fmap, (<$>))
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Ord (Ord)
import safe "base" Data.String (String)
import safe "base" Data.Void (Void)
import safe "base" GHC.Generics (Generic)
import safe "base" GHC.IO.Exception qualified as IO
import safe "base" System.IO (FilePath, IO)
import safe "base" System.IO.Error (IOError)
import safe "base" System.IO.Error qualified as IO
import safe "base" Text.Show (Show)
import "directory" System.Directory (XdgDirectory, XdgDirectoryList)
import safe "pathway" Data.Path
  ( Path,
    Relative,
    Relativity (Abs, Rel),
    Type (Dir, File),
    Typey,
  )
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "time" Data.Time.Clock (UTCTime)
import "variant" Data.Variant (V, liftVariant, toVariant, (:<))
import safe "this" System.Directory.Thin (FsOperations, InternalFailure)
import safe "this" System.Directory.Thin qualified as Dir
import safe "base" Prelude (Integer)

-- | The process has insufficient privileges to perform the operation.
--   [EROFS, EACCES]
type PermissionError :: Kind.Type
newtype PermissionError = PermissionError IOError
  deriving stock (Eq, Generic, Show)

-- | A physical I/O error has occurred. [EIO]
type HardwareFault :: Kind.Type
newtype HardwareFault = HardwareFault IOError
  deriving stock (Eq, Generic, Show)

-- | The operand is not a valid directory name. [ENAMETOOLONG, ELOOP]
--   THIS IS AN INTERNAL ERROR IN Pathway
type InvalidArgument :: Kind.Type
newtype InvalidArgument = InvalidArgument IOError
  deriving stock (Eq, Generic, Show)

-- | The path refers to an existing non-directory object. [EEXIST]
type InappropriateType :: Kind.Type
newtype InappropriateType = InappropriateType IOError
  deriving stock (Eq, Generic, Show)

-- | Insufficient resources (virtual memory, process file descriptors,
--   physical disk space, etc.) are available to perform the operation.
--   [EDQUOT, ENOSPC, ENOMEM, EMLINK]
type FullError :: Kind.Type
newtype FullError = FullError IOError
  deriving stock (Eq, Generic, Show)

-- | The path does not exist. [ENOENT, ENOTDIR]
type DoesNotExistError :: Kind.Type
newtype DoesNotExistError = DoesNotExistError IOError
  deriving stock (Eq, Generic, Show)

-- | The operand refers to a path that already exists. [EEXIST]
type AlreadyExistsError :: Kind.Type
newtype AlreadyExistsError = AlreadyExistsError IOError
  deriving stock (Eq, Generic, Show)

-- | The implementation does not support the operation in this situation.
--   [EINVAL, EXDEV]
type UnsupportedOperation :: Kind.Type
newtype UnsupportedOperation = UnsupportedOperation IOError
  deriving stock (Eq, Generic, Show)

-- | Implementation-dependent constraints are not satisfied.
--   [EBUSY, ENOTEMPTY, EEXIST]
type UnsatisfiedConstraints :: Kind.Type
newtype UnsatisfiedConstraints = UnsatisfiedConstraints IOError
  deriving stock (Eq, Generic, Show)

type MaybeParentCreationFailure :: [Kind.Type]
type MaybeParentCreationFailure =
  '[FullError, InvalidArgument, InappropriateType, PermissionError, HardwareFault]

recoverMaybeParentCreationFailure ::
  IOError -> Maybe (V MaybeParentCreationFailure)
recoverMaybeParentCreationFailure ioe =
  if
    | IO.isFullError ioe -> pure . toVariant $ FullError ioe
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.InvalidArgument -> pure . toVariant $ InvalidArgument ioe
        IO.InappropriateType -> pure . toVariant $ InappropriateType ioe
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

type MaybeCreationFailure :: [Kind.Type]
type MaybeCreationFailure = DoesNotExistError ': MaybeParentCreationFailure

recoverMaybeCreationFailure :: IOError -> Maybe (V MaybeCreationFailure)
recoverMaybeCreationFailure ioe =
  if IO.isDoesNotExistError ioe
    then pure . toVariant $ DoesNotExistError ioe
    else liftVariant <$> recoverMaybeParentCreationFailure ioe

type CreationFailure :: [Kind.Type]
type CreationFailure = AlreadyExistsError ': MaybeCreationFailure

recoverCreationFailure :: IOError -> Maybe (V CreationFailure)
recoverCreationFailure ioe =
  if IO.isAlreadyExistsError ioe
    then pure . toVariant $ AlreadyExistsError ioe
    else liftVariant <$> recoverMaybeCreationFailure ioe

createDirectory :: Path 'Abs 'Dir String -> IO (Either (V CreationFailure) ())
createDirectory = tryJust recoverCreationFailure . Dir.createDirectory

-- | Can perhaps unify this and the following definition depending on how we
--   handle errors.
createDirectoryIfMissing ::
  Path 'Abs 'Dir String -> IO (Either (V MaybeCreationFailure) ())
createDirectoryIfMissing =
  tryJust recoverMaybeCreationFailure . Dir.createDirectoryIfMissing False

createDirectoryWithParentsIfMissing ::
  Path 'Abs 'Dir String ->
  IO (Either (V MaybeParentCreationFailure) ())
createDirectoryWithParentsIfMissing =
  tryJust recoverMaybeParentCreationFailure . Dir.createDirectoryIfMissing True

type RemovalFailure :: [Kind.Type]
type RemovalFailure =
  '[ DoesNotExistError,
     UnsatisfiedConstraints,
     InvalidArgument,
     InappropriateType,
     PermissionError,
     HardwareFault
   ]

recoverRemovalFailure :: IOError -> Maybe (V RemovalFailure)
recoverRemovalFailure ioe =
  if
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.UnsatisfiedConstraints -> pure . toVariant $ UnsatisfiedConstraints ioe
        IO.InvalidArgument -> pure . toVariant $ InvalidArgument ioe
        IO.InappropriateType -> pure . toVariant $ InappropriateType ioe
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

type DirRemovalFailure :: [Kind.Type]
type DirRemovalFailure = UnsupportedOperation ': RemovalFailure

recoverDirRemovalFailure :: IOError -> Maybe (V DirRemovalFailure)
recoverDirRemovalFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
    _ -> liftVariant <$> recoverRemovalFailure ioe

removeDirectory :: Path 'Abs 'Dir String -> IO (Either (V DirRemovalFailure) ())
removeDirectory = tryJust recoverDirRemovalFailure . Dir.removeDirectory

removeDirectoryRecursive :: Path 'Abs 'Dir String -> IO (Either (V DirRemovalFailure) ())
removeDirectoryRecursive =
  tryJust recoverDirRemovalFailure . Dir.removeDirectoryRecursive

removePathForcibly :: (Typey typ) => Path 'Abs typ String -> IO (Either (V DirRemovalFailure) ())
removePathForcibly = tryJust recoverDirRemovalFailure . Dir.removePathForcibly

renameDirectory ::
  Path 'Abs 'Dir String ->
  Path 'Abs 'Dir String ->
  IO (Either (V (AlreadyExistsError ': RenameFailure)) ())
renameDirectory source =
  tryJust
    ( \ioe ->
        if IO.isAlreadyExistsError ioe
          then pure . toVariant $ AlreadyExistsError ioe
          else liftVariant <$> recoverRenameFailure ioe
    )
    . Dir.renameDirectory source

type ListFailure :: [Kind.Type]
type ListFailure =
  '[ DoesNotExistError,
     HardwareFault,
     FullError,
     InvalidArgument,
     InappropriateType,
     PermissionError
   ]

recoverListFailure :: IOError -> Maybe (V ListFailure)
recoverListFailure ioe =
  if
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isFullError ioe -> pure . toVariant $ FullError ioe
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        IO.InvalidArgument -> pure . toVariant $ InvalidArgument ioe
        IO.InappropriateType -> pure . toVariant $ InappropriateType ioe
        _ -> empty

listDirectory ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  -- |
  --
  --  __TODO__: This should be @`Unambiguous` ('`Rel` '`False`) `String`@ once #18
  --            lands.
  IO
    ( Either
        (V ListFailure)
        [ Either
            (InternalFailure FilePath e)
            ( Either
                (Path ('Rel 'False) 'Dir String)
                (Path ('Rel 'False) 'File String)
            )
        ]
    )
listDirectory = tryJust recoverListFailure . Dir.listDirectory

getDirectoryContents ::
  (Ord e) =>
  Path 'Abs 'Dir String ->
  -- |
  --
  --  __TODO__: Should the parent directory (`../`) be singled out, to allow
  --            @`Rel` `False`@ for the directories?
  IO
    ( Either
        (V ListFailure)
        [ Either
            (InternalFailure FilePath e)
            ( Either
                (Path ('Rel 'True) 'Dir String)
                (Path ('Rel 'False) 'File String)
            )
        ]
    )
getDirectoryContents = tryJust recoverListFailure . Dir.getDirectoryContents

type GetDirectoryFailure :: [Kind.Type]
type GetDirectoryFailure = '[UnsupportedOperation, InternalFailure FilePath Void]

recoverGetDirectoryFailure :: IOError -> Maybe (V GetDirectoryFailure)
recoverGetDirectoryFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
    _ -> empty

type GetUserDirectoryFailure :: [Kind.Type]
type GetUserDirectoryFailure = DoesNotExistError ': GetDirectoryFailure

recoverGetUserDirectoryFailure :: IOError -> Maybe (V GetUserDirectoryFailure)
recoverGetUserDirectoryFailure ioe =
  if IO.isDoesNotExistError ioe
    then pure . toVariant $ DoesNotExistError ioe
    else liftVariant <$> recoverGetDirectoryFailure ioe

-- | `InternalFailure` still occurs in these contexts, but it’s specific to each
--   directory in the result, so it doesn’t appear in this union.
type GetUserDirectoriesFailure :: [Kind.Type]
type GetUserDirectoriesFailure = '[UnsupportedOperation, DoesNotExistError]

recoverGetUserDirectoriesFailure :: IOError -> Maybe (V GetUserDirectoriesFailure)
recoverGetUserDirectoriesFailure ioe =
  if IO.isDoesNotExistError ioe
    then pure . toVariant $ DoesNotExistError ioe
    else case IO.ioeGetErrorType ioe of
      IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
      _ -> empty

type CurrentDirectoryFailure :: [Kind.Type]
type CurrentDirectoryFailure = '[PermissionError, UnsupportedOperation, DoesNotExistError, HardwareFault]

recoverCurrentDirectoryFailure :: IOError -> Maybe (V CurrentDirectoryFailure)
recoverCurrentDirectoryFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

type SetCurrentDirectoryFailure :: [Kind.Type]
type SetCurrentDirectoryFailure = InvalidArgument ': InappropriateType ': CurrentDirectoryFailure

recoverSetCurrentDirectoryFailure :: IOError -> Maybe (V SetCurrentDirectoryFailure)
recoverSetCurrentDirectoryFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.InvalidArgument -> pure . toVariant $ InvalidArgument ioe
    IO.InappropriateType -> pure . toVariant $ InappropriateType ioe
    _ -> liftVariant <$> recoverCurrentDirectoryFailure ioe

getCurrentDirectory ::
  IO
    ( Either
        (V (FullError ': InternalFailure FilePath Void ': CurrentDirectoryFailure))
        (Path 'Abs 'Dir String)
    )
getCurrentDirectory = first toVariant <$> Dir.getCurrentDirectory @Void

-- | Don’t use this. Ideally, you should avoid relying on a dynamically scoped(
--   “current directory” (as the pathway-system library encourages), but if you
--   are using code that expects the current directory to be set, then use
--   `withCurrentDirectory`.
setCurrentDirectory ::
  Path 'Abs 'Dir String -> IO (Either (V SetCurrentDirectoryFailure) ())
setCurrentDirectory =
  tryJust recoverSetCurrentDirectoryFailure . Dir.setCurrentDirectory

withCurrentDirectory ::
  Path 'Abs 'Dir String ->
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

tryWithIF :: (InternalFailure FilePath Void :< l) => (IOError -> Maybe (V l)) -> IO (Either (InternalFailure FilePath Void) a) -> IO (Either (V l) a)
tryWithIF recover = fmap (first toVariant =<<) . tryJust recover

getHomeDirectory ::
  IO (Either (V GetUserDirectoryFailure) (Path 'Abs 'Dir String))
getHomeDirectory = tryWithIF recoverGetUserDirectoryFailure $ Dir.getHomeDirectory @Void

-- | Prefer `XDG.BaseDirectory.*Home`.`
getXdgDirectory ::
  XdgDirectory ->
  Path ('Rel 'False) 'Dir String ->
  IO (Either (V GetUserDirectoryFailure) (Path 'Abs 'Dir String))
getXdgDirectory dir =
  tryWithIF recoverGetUserDirectoryFailure . Dir.getXdgDirectory @Void dir

getXdgDirectoryList ::
  (Ord e) =>
  XdgDirectoryList ->
  IO
    ( Either
        (V GetUserDirectoriesFailure)
        [Either (InternalFailure FilePath e) (Path 'Abs 'Dir String)]
    )
getXdgDirectoryList =
  tryJust recoverGetUserDirectoriesFailure . Dir.getXdgDirectoryList

-- | Prefer `XDG.BaseDirectory.*Home`.
getAppUserDataDirectory ::
  Path ('Rel 'False) 'Dir String ->
  IO (Either (V GetUserDirectoryFailure) (Path 'Abs 'Dir String))
getAppUserDataDirectory =
  tryWithIF recoverGetUserDirectoryFailure . Dir.getAppUserDataDirectory @Void

-- | Prefer `XDG.UserDirectory.Common.documents`.
getUserDocumentsDirectory ::
  IO (Either (V GetUserDirectoryFailure) (Path 'Abs 'Dir String))
getUserDocumentsDirectory =
  tryWithIF recoverGetUserDirectoryFailure $ Dir.getUserDocumentsDirectory @Void

-- | Prefer `XDG.BaseDirectory.runtimeDir`.
getTemporaryDirectory ::
  IO (Either (V GetDirectoryFailure) (Path 'Abs 'Dir String))
getTemporaryDirectory =
  tryWithIF recoverGetDirectoryFailure $ Dir.getTemporaryDirectory @Void

removeFile :: Path 'Abs 'File String -> IO (Either (V RemovalFailure) ())
removeFile = tryJust recoverRemovalFailure . Dir.removeFile

type RenameFailure :: [Kind.Type]
type RenameFailure =
  '[ DoesNotExistError,
     UnsatisfiedConstraints,
     FullError,
     InvalidArgument,
     InappropriateType,
     PermissionError,
     HardwareFault
   ]

recoverRenameFailure :: IOError -> Maybe (V RenameFailure)
recoverRenameFailure ioe =
  if
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isFullError ioe -> pure . toVariant $ FullError ioe
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.UnsatisfiedConstraints -> pure . toVariant $ UnsatisfiedConstraints ioe
        IO.InvalidArgument -> pure . toVariant $ InvalidArgument ioe
        IO.InappropriateType -> pure . toVariant $ InappropriateType ioe
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

renameFile ::
  Path 'Abs 'File String ->
  Path 'Abs 'File String ->
  IO (Either (V RenameFailure) ())
renameFile old = tryJust recoverRenameFailure . Dir.renameFile old

renamePath ::
  (Typey typ) =>
  Path 'Abs typ String ->
  Path 'Abs typ String ->
  IO (Either (V RenameFailure) ())
renamePath source = tryJust recoverRenameFailure . Dir.renamePath source

-- |
--
--  __TODO__: It seems likely this would fail with similar cases as
--            `createDirectory`, but the docs don’t claim it.
copyFile ::
  Path 'Abs 'File String -> Path 'Abs 'File String -> IO ()
copyFile = Dir.copyFile

-- |
--
--  __TODO__: It seems likely this would fail with similar cases as
--            `createDirectory`, but the docs don’t claim it.
copyFileWithMetadata ::
  Path 'Abs 'File String -> Path 'Abs 'File String -> IO ()
copyFileWithMetadata = Dir.copyFileWithMetadata

-- |
--
--  __TODO__: It seems likely that this would fail with at least
--            `MetadataFailure`, but the docs don’t claim any.
getFileSize :: Path 'Abs 'File String -> IO Integer
getFileSize = Dir.getFileSize

type MakeFailure :: [Kind.Type]
type MakeFailure =
  '[ PermissionError,
     DoesNotExistError,
     HardwareFault,
     FullError,
     InternalFailure FilePath Void
   ]

recoverMakeFailure :: IOError -> Maybe (V MakeFailure)
recoverMakeFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isFullError ioe -> pure . toVariant $ FullError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

canonicalizePath ::
  (FsOperations typ) =>
  Path 'Abs typ String ->
  IO (Either (V MakeFailure) (Path 'Abs typ String))
canonicalizePath = tryWithIF recoverMakeFailure . Dir.canonicalizePath @_ @Void

-- | Don’t use this. It relies on the “current directory”.
makeAbsolute ::
  (FsOperations typ) =>
  Path ('Rel 'True) typ String ->
  IO (Either (V MakeFailure) (Path 'Abs typ String))
makeAbsolute = tryWithIF recoverMakeFailure . Dir.makeAbsolute @_ @Void

makeRelativeToCurrentDirectory ::
  (FsOperations typ) =>
  Path 'Abs typ String ->
  IO (Either (V MakeFailure) (Path ('Rel 'True) typ String))
makeRelativeToCurrentDirectory =
  tryWithIF recoverMakeFailure . Dir.makeRelativeToCurrentDirectory @_ @Void

getSymbolicLinkTarget ::
  (FsOperations typ) =>
  Path 'Abs typ String ->
  IO (Either (V GetDirectoryFailure) (Path 'Rel.Any typ String))
getSymbolicLinkTarget =
  tryWithIF recoverGetDirectoryFailure . Dir.getSymbolicLinkTarget @_ @Void

type CreateLinkFailure :: [Kind.Type]
type CreateLinkFailure = '[UnsupportedOperation, PermissionError]

recoverCreateLinkFailure :: IOError -> Maybe (V CreateLinkFailure)
recoverCreateLinkFailure ioe =
  if IO.isPermissionError ioe
    then pure . toVariant $ PermissionError ioe
    else case IO.ioeGetErrorType ioe of
      IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
      _ -> empty

createFileLink ::
  (Relative rel) =>
  Path rel 'File String ->
  Path 'Abs 'File String ->
  IO (Either (V CreateLinkFailure) ())
createFileLink old = tryJust recoverCreateLinkFailure . Dir.createFileLink old

createDirectoryLink ::
  (Relative rel) =>
  Path rel 'Dir String ->
  Path 'Abs 'Dir String ->
  IO (Either (V CreateLinkFailure) ())
createDirectoryLink old =
  tryJust recoverCreateLinkFailure . Dir.createDirectoryLink old

-- |
--
--  __TODO__: On POSIX, the failure could be restricted to `RemovalFailure`.
removeDirectoryLink ::
  Path 'Abs 'Dir String -> IO (Either (V DirRemovalFailure) ())
removeDirectoryLink = tryJust recoverDirRemovalFailure . Dir.removeDirectoryLink

type MetadataFailure :: [Kind.Type]
type MetadataFailure = '[PermissionError, DoesNotExistError]

recoverMetadataFailure :: IOError -> Maybe (V MetadataFailure)
recoverMetadataFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | True -> empty

pathIsSymbolicLink ::
  (Typey typ) =>
  Path 'Abs typ String ->
  IO (Either (V MetadataFailure) Bool)
pathIsSymbolicLink = tryJust recoverMetadataFailure . Dir.pathIsSymbolicLink

getPermissions ::
  (Typey typ) =>
  Path 'Abs typ String -> IO (Either (V MetadataFailure) Dir.Permissions)
getPermissions = tryJust recoverMetadataFailure . Dir.getPermissions

setPermissions ::
  (Typey typ) =>
  Path 'Abs typ String ->
  Dir.Permissions ->
  IO (Either (V MetadataFailure) ())
setPermissions path = tryJust recoverMetadataFailure . Dir.setPermissions path

-- |
--
--  __TODO__: It seems likely that this would fail with at least
--            `MetadataFailure`, but the docs don’t claim any.
copyPermissions ::
  (Typey typ, Typey typ') =>
  Path 'Abs typ String ->
  Path 'Abs typ' String ->
  IO ()
copyPermissions from = Dir.copyPermissions from

getAccessTime ::
  (Typey typ) =>
  Path 'Abs typ String -> IO (Either (V MetadataFailure) UTCTime)
getAccessTime = tryJust recoverMetadataFailure . Dir.getAccessTime

getModificationTime ::
  (Typey typ) =>
  Path 'Abs typ String -> IO (Either (V MetadataFailure) UTCTime)
getModificationTime = tryJust recoverMetadataFailure . Dir.getModificationTime

setAccessTime ::
  (Typey typ) =>
  Path 'Abs typ String -> UTCTime -> IO (Either (V MetadataFailure) ())
setAccessTime path = tryJust recoverMetadataFailure . Dir.setAccessTime path

-- |
--
--  __TODO__: On POSIX, the failure could be restricted to `MetadataFailure`.
setModificationTime ::
  (Typey typ) =>
  Path 'Abs typ String ->
  UTCTime ->
  IO (Either (V (InvalidArgument ': MetadataFailure)) ())
setModificationTime path =
  tryJust (\ioe -> liftVariant <$> recoverMetadataFailure ioe)
    . Dir.setModificationTime path
