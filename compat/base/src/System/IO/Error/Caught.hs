{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Each error type lists the POSIX errors that map to it. Some POSIX errors map
-- to different Haskell errors, depending on where exactly something went wrong.
module System.IO.Error.Caught
  ( PermissionError (PermissionError),
    HardwareFault (HardwareFault),
    InvalidArgument (InvalidArgument),
    InappropriateType (InappropriateType),
    FullError (FullError),
    DoesNotExistError (DoesNotExistError),
    AlreadyExistsError (AlreadyExistsError),
    AlreadyInUseError (AlreadyInUseError),
    UnsupportedOperation (UnsupportedOperation),
    UnsatisfiedConstraints (UnsatisfiedConstraints),
  )
where

import "base" Data.Eq (Eq)
import "base" Data.Kind qualified as Kind
import "base" GHC.Generics (Generic)
import "base" System.IO.Error (IOError)
import "base" Text.Show (Show)

-- | The process has insufficient privileges to perform the operation.
--
-- - @EACCES@ – Permission denied.
-- - @EPERM@ – Operation not permitted.
-- - @EROFS@ – Read-only filesystem.
type PermissionError :: Kind.Type
newtype PermissionError = PermissionError IOError
  deriving stock (Eq, Generic, Show)

-- | A physical I/O error has occurred.
--
-- - @EIO@ – Input/output error.
type HardwareFault :: Kind.Type
newtype HardwareFault = HardwareFault IOError
  deriving stock (Eq, Generic, Show)

-- | The operand is not a valid directory name.
--
-- - @ELOOP@ – Too many levels of symbolic links.
-- - @ENAMETOOLONG@ – Filename too long.
type InvalidArgument :: Kind.Type
newtype InvalidArgument = InvalidArgument IOError
  deriving stock (Eq, Generic, Show)

-- | The path refers to an existing non-directory object.
--
-- - @EEXIST@ – File exists.
-- - @EINVAL@ – Invalid argument.
-- - @EISDIR@ – Is a directory.
-- - @ENOTDIR@ – Not a directory.
-- - @ENOTEMPTY@ – Directory not empty.
-- - @EPERM@ – Operation not permitted.
type InappropriateType :: Kind.Type
newtype InappropriateType = InappropriateType IOError
  deriving stock (Eq, Generic, Show)

-- | Insufficient resources (virtual memory, process file descriptors,
--   physical disk space, etc.) are available to perform the operation.
--
-- - @EDQUOT@ – Disk quota exceeded.
-- - @EMFILE@ – Too many open files. Commonly caused by exceeding the
--   @RLIMIT_NOFILE@ resource limit described in
--   [getrlimit(2)](https://www.man7.org/linux/man-pages/man2/getrlimit.2.html).
--   Can also be caused by exceeding the limit specified in
--   /proc/sys/fs/nr_open.
-- - @EMLINK@ – Too many links.
-- - @ENFILE@ – Too many open files in system.  On Linux, this is probably a
--   result of encountering the /proc/sys/fs/file-max limit (see
--   [proc(5)](https://www.man7.org/linux/man-pages/man5/proc.5.html)).
-- - @ENOMEM@ – Not enough space/cannot allocate memory.
-- - @ENOSPC@ – No space left on device.
type FullError :: Kind.Type
newtype FullError = FullError IOError
  deriving stock (Eq, Generic, Show)

-- | The path does not exist.
--
-- - @ENOENT@ – No such file or directory. Typically, this error results when a
--   specified pathname does not exist, or one of the components in the
--   directory prefix of a pathname does not exist, or the specified pathname is
--   a dangling symbolic link.
-- - @ENOTDIR@ – Not a directory.
type DoesNotExistError :: Kind.Type
newtype DoesNotExistError = DoesNotExistError IOError
  deriving stock (Eq, Generic, Show)

-- | The operand refers to a path that already exists.
--
-- - @EEXIST@ – File exists.
type AlreadyExistsError :: Kind.Type
newtype AlreadyExistsError = AlreadyExistsError IOError
  deriving stock (Eq, Generic, Show)

type AlreadyInUseError :: Kind.Type
newtype AlreadyInUseError = AlreadyInUseError IOError
  deriving stock (Eq, Generic, Show)

-- | The implementation does not support the operation in this situation.
--
-- - @EINVAL@ – Invalid argument.
-- - @EXDEV@ – Invalid cross-device link.
type UnsupportedOperation :: Kind.Type
newtype UnsupportedOperation = UnsupportedOperation IOError
  deriving stock (Eq, Generic, Show)

-- | Implementation-dependent constraints are not satisfied.
--
-- - @EBUSY@ – Device or resource busy.
-- - @EEXIST@ – File exists.
-- - @ENOTEMPTY@ – Directory not empty.
type UnsatisfiedConstraints :: Kind.Type
newtype UnsatisfiedConstraints = UnsatisfiedConstraints IOError
  deriving stock (Eq, Generic, Show)
