{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
-- WAIT: "NoRecursion" doesn’t currently provide a way to ignore names like
--       `$dPopVariant_aei8`, but it should.
{-# OPTIONS_GHC -fplugin-opt NoRecursion:allow-recursion:true #-}

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
import safe "pathway-compat-filepath" Common
  ( InternalFailure (IncorrectResultType, ParseFailure),
  )
import "variant" Data.Variant (V, liftVariant, toVariant, (:<))
import safe "this" System.IO.Error
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

type MaybeParentCreationFailure :: [Kind.Type]
type MaybeParentCreationFailure =
  '[ FullError,
     InvalidArgument,
     InappropriateType,
     PermissionError,
     HardwareFault
   ]

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

type GetDirectoryFailure :: Kind.Type -> [Kind.Type]
type GetDirectoryFailure rep = '[UnsupportedOperation, InternalFailure rep Void]

recoverGetDirectoryFailure :: IOError -> Maybe (V (GetDirectoryFailure rep))
recoverGetDirectoryFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
    _ -> empty

type GetUserDirectoryFailure :: Kind.Type -> [Kind.Type]
type GetUserDirectoryFailure rep = DoesNotExistError ': GetDirectoryFailure rep

recoverGetUserDirectoryFailure ::
  forall rep. IOError -> Maybe (V (GetUserDirectoryFailure rep))
recoverGetUserDirectoryFailure ioe =
  if IO.isDoesNotExistError ioe
    then pure . toVariant $ DoesNotExistError ioe
    else liftVariant <$> recoverGetDirectoryFailure @rep ioe

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
type CurrentDirectoryFailure =
  '[ PermissionError,
     UnsupportedOperation,
     DoesNotExistError,
     HardwareFault
   ]

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
type SetCurrentDirectoryFailure =
  InvalidArgument ': InappropriateType ': CurrentDirectoryFailure

recoverSetCurrentDirectoryFailure ::
  IOError -> Maybe (V SetCurrentDirectoryFailure)
recoverSetCurrentDirectoryFailure ioe =
  case IO.ioeGetErrorType ioe of
    IO.InvalidArgument -> pure . toVariant $ InvalidArgument ioe
    IO.InappropriateType -> pure . toVariant $ InappropriateType ioe
    _ -> liftVariant <$> recoverCurrentDirectoryFailure ioe

tryWithIF ::
  (InternalFailure rep Void :< l) =>
  (IOError -> Maybe (V l)) ->
  IO (Either (InternalFailure rep Void) a) ->
  IO (Either (V l) a)
tryWithIF recover = fmap (first toVariant =<<) . tryJust recover

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

type MakeFailure :: Kind.Type -> [Kind.Type]
type MakeFailure rep =
  '[ PermissionError,
     DoesNotExistError,
     HardwareFault,
     FullError,
     InternalFailure rep Void
   ]

recoverMakeFailure :: IOError -> Maybe (V (MakeFailure rep))
recoverMakeFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | IO.isFullError ioe -> pure . toVariant $ FullError ioe
    | True -> case IO.ioeGetErrorType ioe of
        IO.HardwareFault -> pure . toVariant $ HardwareFault ioe
        _ -> empty

type CreateLinkFailure :: [Kind.Type]
type CreateLinkFailure = '[UnsupportedOperation, PermissionError]

recoverCreateLinkFailure :: IOError -> Maybe (V CreateLinkFailure)
recoverCreateLinkFailure ioe =
  if IO.isPermissionError ioe
    then pure . toVariant $ PermissionError ioe
    else case IO.ioeGetErrorType ioe of
      IO.UnsupportedOperation -> pure . toVariant $ UnsupportedOperation ioe
      _ -> empty

type MetadataFailure :: [Kind.Type]
type MetadataFailure = '[PermissionError, DoesNotExistError]

recoverMetadataFailure :: IOError -> Maybe (V MetadataFailure)
recoverMetadataFailure ioe =
  if
    | IO.isPermissionError ioe -> pure . toVariant $ PermissionError ioe
    | IO.isDoesNotExistError ioe -> pure . toVariant $ DoesNotExistError ioe
    | True -> empty
