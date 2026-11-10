{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Each error type lists the POSIX errors that map to it. Some POSIX errors map
-- to different Haskell errors, depending on where exactly something went wrong.
--
-- @since 99999
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
--
-- @since 99999
type PermissionError :: Kind.Type
newtype PermissionError
  = -- |
    --
    -- @since 99999
    PermissionError IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | A physical I/O error has occurred.
--
-- - @EIO@ – Input/output error.
--
-- @since 99999
type HardwareFault :: Kind.Type
newtype HardwareFault
  = -- |
    --
    -- @since 99999
    HardwareFault IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | The operand is not a valid directory name.
--
-- - @ELOOP@ – Too many levels of symbolic links.
-- - @ENAMETOOLONG@ – Filename too long.
--
-- @since 99999
type InvalidArgument :: Kind.Type
newtype InvalidArgument
  = -- |
    --
    -- @since 99999
    InvalidArgument IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | The path refers to an existing non-directory object.
--
-- - @EEXIST@ – File exists.
-- - @EINVAL@ – Invalid argument.
-- - @EISDIR@ – Is a directory.
-- - @ENOTDIR@ – Not a directory.
-- - @ENOTEMPTY@ – Directory not empty.
-- - @EPERM@ – Operation not permitted.
--
-- @since 99999
type InappropriateType :: Kind.Type
newtype InappropriateType
  = -- |
    --
    -- @since 99999
    InappropriateType IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

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
--
-- @since 99999
type FullError :: Kind.Type
newtype FullError
  = -- |
    --
    -- @since 99999
    FullError IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | The path does not exist.
--
-- - @ENOENT@ – No such file or directory. Typically, this error results when a
--   specified pathname does not exist, or one of the components in the
--   directory prefix of a pathname does not exist, or the specified pathname is
--   a dangling symbolic link.
-- - @ENOTDIR@ – Not a directory.
--
-- @since 99999
type DoesNotExistError :: Kind.Type
newtype DoesNotExistError
  = -- |
    --
    -- @since 99999
    DoesNotExistError IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | The operand refers to a path that already exists.
--
-- - @EEXIST@ – File exists.
--
-- @since 99999
type AlreadyExistsError :: Kind.Type
newtype AlreadyExistsError
  = -- |
    --
    -- @since 99999
    AlreadyExistsError IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | The operand is already in use. GHC also raises this when opening a file
--   that is already open in a conflicting mode, since it locks the files it
--   opens.
--
--   - @EBUSY@ – Device or resource busy.
--
-- @since 99999
type AlreadyInUseError :: Kind.Type
newtype AlreadyInUseError
  = -- |
    --
    -- @since 99999
    AlreadyInUseError IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | The implementation does not support the operation in this situation.
--
-- - @EINVAL@ – Invalid argument.
-- - @EXDEV@ – Invalid cross-device link.
--
-- @since 99999
type UnsupportedOperation :: Kind.Type
newtype UnsupportedOperation
  = -- |
    --
    -- @since 99999
    UnsupportedOperation IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )

-- | Implementation-dependent constraints are not satisfied.
--
-- - @EBUSY@ – Device or resource busy.
-- - @EEXIST@ – File exists.
-- - @ENOTEMPTY@ – Directory not empty.
--
-- @since 99999
type UnsatisfiedConstraints :: Kind.Type
newtype UnsatisfiedConstraints
  = -- |
    --
    -- @since 99999
    UnsatisfiedConstraints IOError
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Show
    )
