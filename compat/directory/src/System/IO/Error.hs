{-# LANGUAGE Safe #-}

module System.IO.Error
  ( PermissionError (PermissionError),
    HardwareFault (HardwareFault),
    InvalidArgument (InvalidArgument),
    InappropriateType (InappropriateType),
    FullError (FullError),
    DoesNotExistError (DoesNotExistError),
    AlreadyExistsError (AlreadyExistsError),
    UnsupportedOperation (UnsupportedOperation),
    UnsatisfiedConstraints (UnsatisfiedConstraints),
  )
where

import safe "base" Data.Eq (Eq)
import safe "base" Data.Kind qualified as Kind
import safe "base" GHC.Generics (Generic)
import safe "base" System.IO.Error (IOError)
import safe "base" Text.Show (Show)

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
