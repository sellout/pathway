{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-- This module is inferred @Safe@ in some dependency solutions.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   A thin wrapper around "System.IO.Temp" that does little more than replace
--   stringy paths with `Path`.
--
--  __NOTE__: There’s a bug in the underlying temporary library
--            (UnkindPartition/temporary#16) that we avoid here. If the template
--            is a path string, it can escape the temp directory (because
--            temporary just concatenates the strings, assuming the template
--            will be a single component, so an absolute path as a template will
--            ignore the temp directory path, and “../” will cause it to ascend
--            out of the temp directory). Also, if the template is simply a
--            relative path with more than one component, temporary would fail
--            if all the intervening directories don’t exist, because temporary
--            doesn’t attempt to create them. Due to this, it’s clear that
--            temporary expects the template to be a path component, not a path,
--            and so Pathway correspondingly escapes the template before passing
--            it to the underlying operations in temporary, allowing arbitrary
--            characters to be used in the template.
--
-- @since 99999
#if MIN_VERSION_temporary(1, 2, 1)
module System.IO.Temp.Thin
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
module System.IO.Temp.Thin
  ( withTempFile,
    withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
    withSystemTempFile,
    withSystemTempDirectory,
  )
where
#else
module System.IO.Temp.Thin
  ( withTempFile,
    withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
  )
where
#endif

import safe "base" Control.Applicative (liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad.IO.Class (MonadIO)
import safe "base" Data.Bitraversable (bitraverse)
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Ord (Ord)
import safe "base" Data.String (String)
import safe "base" Data.Traversable (sequenceA, traverse)
import safe "base" System.IO (FilePath, Handle, IO)
import safe "exceptions" Control.Monad.Catch (MonadMask)
import safe "pathway" Data.Path
  ( Path,
    Relativity (Abs),
    Type (Dir, File),
    escapeComponent,
  )
import safe "pathway" Data.Path.Format qualified as Format
import safe "pathway-compat-base" Common
  ( InternalFailure,
    absFileFromPathRep,
    toPathRep,
  )
import safe "pathway-compat-filepath" Common.FilePath (absDirFromPathRep)
import "temporary" System.IO.Temp qualified as Temp
#if MIN_VERSION_temporary(1, 2, 1)
import safe "base" Data.Functor ((<$>))
#endif

-- $setup
-- >>> :seti -XTypeApplications
-- >>> import "base" Control.Exception (SomeException, try)

-- | This is what works around the bug described in the module documentation.
escapeTemplate :: String -> String
escapeTemplate = escapeComponent Format.local

-- | `System.IO.Temp.withTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
withTempFile ::
  (MonadIO m, MonadMask m, Ord void) =>
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Callback that can use the file
  (Path 'Abs 'File String -> Handle -> m a) ->
  m (Either (InternalFailure FilePath void) a)
withTempFile parentDir template action =
  Temp.withTempFile
    (toPathRep parentDir)
    (escapeTemplate template)
    $ \file -> sequenceA . liftA2 action (absFileFromPathRep file) . pure

-- | `System.IO.Temp.withTempDirectory`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
withTempDirectory ::
  (MonadIO m, MonadMask m, Ord void) =>
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Callback that can use the directory
  (Path 'Abs 'Dir String -> m a) ->
  m (Either (InternalFailure FilePath void) a)
withTempDirectory parentDir template action =
  Temp.withTempDirectory (toPathRep parentDir) (escapeTemplate template) $
    traverse action . absDirFromPathRep

-- | Like `openBinaryTempFile`, but uses 666 rather than 600 for the
--   permissions.
--
-- Equivalent to `openBinaryTempFileWithDefaultPermissions`.
--
-- @since 99999
openNewBinaryFile ::
  (Ord void) =>
  Path 'Abs 'Dir String ->
  String ->
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String, Handle))
openNewBinaryFile dir =
  fmap (bitraverse absFileFromPathRep pure)
    . Temp.openNewBinaryFile (toPathRep dir)
    . escapeTemplate
{-# INLINEABLE openNewBinaryFile #-}

-- | `System.IO.Temp.createTempDirectory`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
--   Here are some examples of the UnkindPartition/temporary#16 issue. It could
--   be useful to have these cases not throw an exception, with a bit of
--   environment set up, but later.
--
-- >>> try @SomeException $ Temp.createTempDirectory "/tmp" "/home/me/dir"
-- Left /home/me/dir...: ...: does not exist (...)
-- >>> try @SomeException $ Temp.createTempDirectory "/home/me/tmp" "../dir"
-- Left /home/me/tmp.../dir...: ...: does not exist (...)
-- >>> try @SomeException $ Temp.createTempDirectory "/tmp" "some/dir"
-- Left /tmp...some/dir...: ...: does not exist (...)
--
-- @since 99999
createTempDirectory ::
  (Ord void) =>
  -- | Parent directory to create the directory in
  Path 'Abs 'Dir String ->
  -- | Directory name template
  String ->
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'Dir String))
createTempDirectory dir =
  fmap absDirFromPathRep
    . Temp.createTempDirectory (toPathRep dir)
    . escapeTemplate
{-# INLINEABLE createTempDirectory #-}

#if MIN_VERSION_temporary(1, 1, 0)
-- | `System.IO.Temp.withSystemTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
withSystemTempFile ::
  (MonadIO m, MonadMask m, Ord void) =>
  -- | File name template
  String ->
  -- | Callback that can use the file
  (Path 'Abs 'File String -> Handle -> m a) ->
  m (Either (InternalFailure FilePath void) a)
withSystemTempFile template action =
  Temp.withSystemTempFile (escapeTemplate template) $
    \file -> sequenceA . liftA2 action (absFileFromPathRep file) . pure

-- | `System.IO.Temp.withSystemTempDirectory`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
withSystemTempDirectory ::
  (MonadIO m, MonadMask m, Ord void) =>
  -- | File name template
  String ->
  -- | Callback that can use the directory
  (Path 'Abs 'Dir String -> m a) ->
  m (Either (InternalFailure FilePath void) a)
withSystemTempDirectory template action =
  Temp.withSystemTempDirectory (escapeTemplate template) $
    traverse action . absDirFromPathRep
#endif

#if MIN_VERSION_temporary(1, 2, 1)
-- | `System.IO.Temp.writeTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
writeTempFile ::
  (Ord void) =>
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Data to store in the file
  String ->
  -- | Path to the (written and closed) file
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String))
writeTempFile dir template =
  fmap absFileFromPathRep
    . Temp.writeTempFile (toPathRep dir) (escapeTemplate template)
{-# INLINEABLE writeTempFile #-}

-- | `System.IO.Temp.writeSystemTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
writeSystemTempFile ::
  (Ord void) =>
  -- | File name template
  String ->
  -- | Data to store in the file
  String ->
  -- | Path to the (written and closed) file
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String))
writeSystemTempFile template =
  fmap absFileFromPathRep . Temp.writeSystemTempFile (escapeTemplate template)

-- | `System.IO.Temp.emptyTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
emptyTempFile ::
  (Ord void) =>
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String))
emptyTempFile dir =
  fmap absFileFromPathRep . Temp.emptyTempFile (toPathRep dir) . escapeTemplate
{-# INLINEABLE emptyTempFile #-}

-- | `System.IO.Temp.emptySystemTempFile`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
emptySystemTempFile ::
  (Ord void) =>
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String))
emptySystemTempFile =
  fmap absFileFromPathRep . Temp.emptySystemTempFile . escapeTemplate
{-# INLINEABLE emptySystemTempFile #-}

-- | `System.IO.Temp.getCanonicalTemporaryDirectory`, on `Path`s.
--
--   Any path it returns that doesn’t parse is returned as an `InternalFailure`
--   instead.
--
-- @since 99999
getCanonicalTemporaryDirectory ::
  (Ord void) =>
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'Dir String))
getCanonicalTemporaryDirectory =
  absDirFromPathRep <$> Temp.getCanonicalTemporaryDirectory
#endif
