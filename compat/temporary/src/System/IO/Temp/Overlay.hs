{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
#if MIN_VERSION_temporary(1, 2, 1)
module System.IO.Temp.Overlay
  ( withTempFile,
    withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
    withSystemTempFile,
    withSystemTempDirectory,
    writeTempFile,
    writeSystemTempFile,
    emptyTempFile,
    emptySystemTempFile,
    getCanonicalTemporaryDirectory,
  )
where
#elif MIN_VERSION_temporary(1, 1, 1)
module System.IO.Temp.Overlay
  ( withTempFile,
    withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
    withSystemTempFile,
    withSystemTempDirectory,
  )
where
#else
module System.IO.Temp.Overlay
  ( withTempFile,
    withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
  )
where
#endif

import safe "base" Control.Applicative (pure)
import safe "base" Control.Monad ((<=<))
import safe "base" Control.Monad.IO.Class (MonadIO)
import safe "base" Data.Either (either)
import safe "base" Data.String (String)
import safe "base" Data.Void (Void)
import safe "base" System.IO (Handle, IO)
import safe "exceptions" Control.Monad.Catch (MonadMask, throwM)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import "this" System.IO.Temp.Thin qualified as Thin
#if MIN_VERSION_temporary(1, 2, 1)
import safe "base" Control.Monad ((=<<))
#endif

-- | `System.IO.Temp.withTempFile`, on `Path`s, with failures thrown in `IO` (as
--   the original would) rather than returned.
--
-- @since 99999
withTempFile ::
  (MonadIO m, MonadMask m) =>
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Callback that can use the file
  (Path 'Abs 'File String -> Handle -> m a) ->
  m a
withTempFile parentDir template =
  either throwM pure <=< Thin.withTempFile @_ @Void parentDir template

-- | `System.IO.Temp.withTempDirectory`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
withTempDirectory ::
  (MonadIO m, MonadMask m) =>
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Callback that can use the directory
  (Path 'Abs 'Dir String -> m a) ->
  m a
withTempDirectory parentDir template =
  either throwM pure <=< Thin.withTempDirectory @_ @Void parentDir template

-- | `System.IO.Temp.openNewBinaryFile`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
openNewBinaryFile ::
  Path 'Abs 'Dir String ->
  String ->
  IO (Path 'Abs 'File String, Handle)
openNewBinaryFile dir = either throwM pure <=< Thin.openNewBinaryFile @Void dir

-- | `System.IO.Temp.createTempDirectory`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
createTempDirectory ::
  -- | Parent directory to create the directory in
  Path 'Abs 'Dir String ->
  -- | Directory name template
  String ->
  IO (Path 'Abs 'Dir String)
createTempDirectory dir =
  either throwM pure <=< Thin.createTempDirectory @Void dir

#if MIN_VERSION_temporary(1, 1, 0)
-- | `System.IO.Temp.withSystemTempFile`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
withSystemTempFile ::
  (MonadIO m, MonadMask m) =>
  -- | File name template
  String ->
  -- | Callback that can use the file
  (Path 'Abs 'File String -> Handle -> m a) ->
  m a
withSystemTempFile template =
  either throwM pure <=< Thin.withSystemTempFile @_ @Void template

-- | `System.IO.Temp.withSystemTempDirectory`, on `Path`s, with failures thrown
--   in `IO` (as the original would) rather than returned.
--
-- @since 99999
withSystemTempDirectory ::
  (MonadIO m, MonadMask m) =>
  -- | File name template
  String ->
  -- | Callback that can use the directory
  (Path 'Abs 'Dir String -> m a) ->
  m a
withSystemTempDirectory template =
  either throwM pure <=< Thin.withSystemTempDirectory @_ @Void template
#endif

#if MIN_VERSION_temporary(1, 2, 1)
-- | `System.IO.Temp.writeTempFile`, on `Path`s, with failures thrown in `IO`
--   (as the original would) rather than returned.
--
-- @since 99999
writeTempFile ::
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Data to store in the file
  String ->
  -- | Path to the (written and closed) file
  IO (Path 'Abs 'File String)
writeTempFile dir template =
  either throwM pure <=< Thin.writeTempFile @Void dir template

-- | `System.IO.Temp.writeSystemTempFile`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
writeSystemTempFile ::
  -- | File name template
  String ->
  -- | Data to store in the file
  String ->
  -- | Path to the (written and closed) file
  IO (Path 'Abs 'File String)
writeSystemTempFile template =
  either throwM pure <=< Thin.writeSystemTempFile @Void template

-- | `System.IO.Temp.emptyTempFile`, on `Path`s, with failures thrown in `IO`
--   (as the original would) rather than returned.
--
-- @since 99999
emptyTempFile ::
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Path 'Abs 'File String)
emptyTempFile dir = either throwM pure <=< Thin.emptyTempFile @Void dir

-- | `System.IO.Temp.emptySystemTempFile`, on `Path`s, with failures thrown in
--   `IO` (as the original would) rather than returned.
--
-- @since 99999
emptySystemTempFile ::
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Path 'Abs 'File String)
emptySystemTempFile = either throwM pure <=< Thin.emptySystemTempFile @Void

-- | `System.IO.Temp.getCanonicalTemporaryDirectory`, on `Path`s, with failures
--   thrown in `IO` (as the original would) rather than returned.
--
-- @since 99999
getCanonicalTemporaryDirectory ::
  IO (Path 'Abs 'Dir String)
getCanonicalTemporaryDirectory =
  either throwM pure =<< Thin.getCanonicalTemporaryDirectory @Void
#endif
