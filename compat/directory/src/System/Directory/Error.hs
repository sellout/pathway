{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- WAIT: "NoRecursion" doesn’t currently provide a way to ignore names like
--       `$dPopVariant_aei8`, but it should.
{-# OPTIONS_GHC -fplugin-opt NoRecursion:allow-recursion:true #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module System.Directory.Error
  ( InternalFailure (..),
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
    recoverMaybeParentCreationFailure,
    recoverMaybeCreationFailure,
    recoverCreationFailure,
    recoverRemovalFailure,
    recoverDirRemovalFailure,
    recoverListFailure,
    recoverGetDirectoryFailure,
    recoverGetUserDirectoryFailure,
    recoverGetUserDirectoriesFailure,
    recoverCurrentDirectoryFailure,
    recoverSetCurrentDirectoryFailure,
    recoverRenameFailure,
    recoverMakeFailure,
    recoverCreateLinkFailure,
    recoverMetadataFailure,
    tryWithIF,
  )
where

import safe "base" Control.Applicative (empty, pure)
import safe "base" Control.Category (Category ((.)), (.))
import safe "base" Control.Exception (tryJust)
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool (True))
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor (fmap), fmap, (<$>))
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Void (Void)
import safe "base" GHC.IO.Exception qualified as IO
import safe "base" System.IO (IO)
import safe "base" System.IO.Error (IOError)
import safe "base" System.IO.Error qualified as IO
import safe "pathway-compat-base" Common
  ( InternalFailure (IncorrectResultType, ParseFailure),
  )
import safe "pathway-compat-base" System.IO.Error.Pathway
  ( AlreadyExistsError (AlreadyExistsError),
    DoesNotExistError (DoesNotExistError),
    FullError (FullError),
    HardwareFault (HardwareFault),
    InappropriateType (InappropriateType),
    InvalidArgument (InvalidArgument),
    PermissionError (PermissionError),
    UnsatisfiedConstraints (UnsatisfiedConstraints),
    UnsupportedOperation (UnsupportedOperation),
  )
import "variant" Data.Variant (V, liftVariant, toVariant, (:<))

-- | The errors raised by creating a directory that may already exist, along
--   with any missing parents.
--
-- @since 99999
type MaybeParentCreationFailure :: [Kind.Type]
type MaybeParentCreationFailure =
  '[ FullError,
     InvalidArgument,
     InappropriateType,
     PermissionError,
     HardwareFault
   ]

-- | Classify an `IOError` as one of the `MaybeParentCreationFailure`s, or
--   `Nothing` if it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
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

-- | The errors raised by creating a directory that may already exist. This
--   extends `MaybeParentCreationFailure` with the parent not existing.
--
-- @since 99999
type MaybeCreationFailure :: [Kind.Type]
type MaybeCreationFailure = DoesNotExistError ': MaybeParentCreationFailure

-- | Classify an `IOError` as one of the `MaybeCreationFailure`s, or `Nothing`
--   if it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverMaybeCreationFailure :: IOError -> Maybe (V MaybeCreationFailure)
recoverMaybeCreationFailure ioe =
  if IO.isDoesNotExistError ioe
    then pure . toVariant $ DoesNotExistError ioe
    else liftVariant <$> recoverMaybeParentCreationFailure ioe

-- | The errors raised by creating a directory. This extends
--   `MaybeCreationFailure` with the directory already existing.
--
-- @since 99999
type CreationFailure :: [Kind.Type]
type CreationFailure = AlreadyExistsError ': MaybeCreationFailure

-- | Classify an `IOError` as one of the `CreationFailure`s, or `Nothing` if it
--   isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverCreationFailure :: IOError -> Maybe (V CreationFailure)
recoverCreationFailure ioe =
  if IO.isAlreadyExistsError ioe
    then pure . toVariant $ AlreadyExistsError ioe
    else liftVariant <$> recoverMaybeCreationFailure ioe

-- | The errors raised by removing a file.
--
-- @since 99999
type RemovalFailure :: [Kind.Type]
type RemovalFailure =
  '[ DoesNotExistError,
     UnsatisfiedConstraints,
     InvalidArgument,
     InappropriateType,
     PermissionError,
     HardwareFault
   ]

-- | Classify an `IOError` as one of the `RemovalFailure`s, or `Nothing` if it
--   isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
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

-- | The errors raised by removing a directory. This extends `RemovalFailure`.
--
-- @since 99999
type DirRemovalFailure :: [Kind.Type]
type DirRemovalFailure = UnsupportedOperation ': RemovalFailure

-- | Classify an `IOError` as one of the `DirRemovalFailure`s, or `Nothing` if
--   it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverDirRemovalFailure :: IOError -> Maybe (V DirRemovalFailure)
recoverDirRemovalFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
    _ -> liftVariant <$> recoverRemovalFailure ioe

-- | The errors raised by listing the contents of a directory.
--
-- @since 99999
type ListFailure :: [Kind.Type]
type ListFailure =
  '[ DoesNotExistError,
     HardwareFault,
     FullError,
     InvalidArgument,
     InappropriateType,
     PermissionError
   ]

-- | Classify an `IOError` as one of the `ListFailure`s, or `Nothing` if it
--   isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
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

-- | The errors raised by looking up a special directory, including an
--   `InternalFailure` when the result doesn’t parse.
--
-- @since 99999
type GetDirectoryFailure :: Kind.Type -> [Kind.Type]
type GetDirectoryFailure rep = '[UnsupportedOperation, InternalFailure rep Void]

-- | Classify an `IOError` as one of the `GetDirectoryFailure`s, or `Nothing` if
--   it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverGetDirectoryFailure :: IOError -> Maybe (V (GetDirectoryFailure rep))
recoverGetDirectoryFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
    _ -> empty

-- | The errors raised by looking up a user-specific directory. This extends
--   `GetDirectoryFailure`.
--
-- @since 99999
type GetUserDirectoryFailure :: Kind.Type -> [Kind.Type]
type GetUserDirectoryFailure rep = DoesNotExistError ': GetDirectoryFailure rep

-- | Classify an `IOError` as one of the `GetUserDirectoryFailure`s, or
--   `Nothing` if it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverGetUserDirectoryFailure ::
  forall rep. IOError -> Maybe (V (GetUserDirectoryFailure rep))
recoverGetUserDirectoryFailure ioe =
  if IO.isDoesNotExistError ioe
    then pure . toVariant $ DoesNotExistError ioe
    else liftVariant <$> recoverGetDirectoryFailure @rep ioe

-- | `InternalFailure` still occurs in these contexts, but it’s specific to each
--   directory in the result, so it doesn’t appear in this union.
--
-- @since 99999
type GetUserDirectoriesFailure :: [Kind.Type]
type GetUserDirectoriesFailure = '[UnsupportedOperation, DoesNotExistError]

-- | Classify an `IOError` as one of the `GetUserDirectoriesFailure`s, or
--   `Nothing` if it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverGetUserDirectoriesFailure :: IOError -> Maybe (V GetUserDirectoriesFailure)
recoverGetUserDirectoriesFailure ioe =
  if IO.isDoesNotExistError ioe
    then pure . toVariant $ DoesNotExistError ioe
    else case IO.ioeGetErrorType ioe of
      IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
      _ -> empty

-- | The errors raised by getting the “current” directory.
--
-- @since 99999
type CurrentDirectoryFailure :: [Kind.Type]
type CurrentDirectoryFailure =
  '[ PermissionError,
     UnsupportedOperation,
     DoesNotExistError,
     HardwareFault
   ]

-- | Classify an `IOError` as one of the `CurrentDirectoryFailure`s, or
--   `Nothing` if it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverCurrentDirectoryFailure :: IOError -> Maybe (V CurrentDirectoryFailure)
recoverCurrentDirectoryFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

-- | The errors raised by setting the “current” directory. This extends
--   `CurrentDirectoryFailure`.
--
-- @since 99999
type SetCurrentDirectoryFailure :: [Kind.Type]
type SetCurrentDirectoryFailure =
  InvalidArgument ': InappropriateType ': CurrentDirectoryFailure

-- | Classify an `IOError` as one of the `SetCurrentDirectoryFailure`s, or
--   `Nothing` if it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverSetCurrentDirectoryFailure ::
  IOError -> Maybe (V SetCurrentDirectoryFailure)
recoverSetCurrentDirectoryFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.InvalidArgument -> pure . toVariant $ InvalidArgument ioe
    IO.InappropriateType -> pure . toVariant $ InappropriateType ioe
    _ -> liftVariant <$> recoverCurrentDirectoryFailure ioe

-- | Like `tryJust`, but also folds an `InternalFailure` returned by the action
--   into the same variant as the recovered errors.
--
-- @since 99999
tryWithIF ::
  (InternalFailure rep Void :< l) =>
  (IOError -> Maybe (V l)) ->
  IO (Either (InternalFailure rep Void) a) ->
  IO (Either (V l) a)
tryWithIF recover = fmap (first toVariant =<<) . tryJust recover
{-# INLINEABLE tryWithIF #-}

-- | The errors raised by renaming a file or directory.
--
-- @since 99999
type RenameFailure :: [Kind.Type]
type RenameFailure =
  '[ DoesNotExistError,
     UnsatisfiedConstraints,
     FullError,
     InvalidArgument,
     InappropriateType,
     PermissionError,
     HardwareFault,
     UnsupportedOperation
   ]

-- | Classify an `IOError` as one of the `RenameFailure`s, or `Nothing` if it
--   isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
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
        IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
        _ -> empty

-- | The errors raised by making a path absolute or canonical, including an
--   `InternalFailure` when the result doesn’t parse.
--
-- @since 99999
type MakeFailure :: Kind.Type -> [Kind.Type]
type MakeFailure rep =
  '[ PermissionError,
     DoesNotExistError,
     HardwareFault,
     FullError,
     InternalFailure rep Void
   ]

-- | Classify an `IOError` as one of the `MakeFailure`s, or `Nothing` if it
--   isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverMakeFailure :: IOError -> Maybe (V (MakeFailure rep))
recoverMakeFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isFullError ioe -> pure . toVariant $ FullError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

-- | The errors raised by creating a symbolic link.
--
-- @since 99999
type CreateLinkFailure :: [Kind.Type]
type CreateLinkFailure = '[UnsupportedOperation, PermissionError]

-- | Classify an `IOError` as one of the `CreateLinkFailure`s, or `Nothing` if
--   it isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverCreateLinkFailure :: IOError -> Maybe (V CreateLinkFailure)
recoverCreateLinkFailure ioe =
  if IO.isPermissionError ioe
    then pure . toVariant $ PermissionError ioe
    else case IO.ioeGetErrorType ioe of
      IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
      _ -> empty

-- | The errors raised by reading or writing the metadata of a file or
--   directory.
--
-- @since 99999
type MetadataFailure :: [Kind.Type]
type MetadataFailure = '[PermissionError, DoesNotExistError]

-- | Classify an `IOError` as one of the `MetadataFailure`s, or `Nothing` if it
--   isn’t one (so `tryJust` rethrows it).
--
-- @since 99999
recoverMetadataFailure :: IOError -> Maybe (V MetadataFailure)
recoverMetadataFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | True -> empty
