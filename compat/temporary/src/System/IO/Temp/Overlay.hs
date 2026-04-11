{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

module System.IO.Temp.Overlay
  ( withSystemTempFile,
    withSystemTempDirectory,
    withTempFile,
    withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
    writeTempFile,
    writeSystemTempFile,
    emptyTempFile,
    emptySystemTempFile,
    IO.openTempFile,
    IO.openBinaryTempFile,
    getCanonicalTemporaryDirectory,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Monad ((<=<), (=<<))
import safe "base" Control.Monad.IO.Class (MonadIO)
import safe "base" Data.Either (either)
import safe "base" Data.String (String)
import safe "base" Data.Void (Void)
import safe "base" System.IO (Handle, IO)
import safe "exceptions" Control.Monad.Catch (MonadMask, throwM)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import safe "pathway-compat-base" System.IO.Overlay qualified as IO
import "this" System.IO.Temp.Thin qualified as Thin

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

openNewBinaryFile ::
  Path 'Abs 'Dir String ->
  String ->
  IO (Path 'Abs 'File String, Handle)
openNewBinaryFile dir = either throwM pure <=< Thin.openNewBinaryFile @Void dir

createTempDirectory ::
  -- | Parent directory to create the directory in
  Path 'Abs 'Dir String ->
  -- | Directory name template
  String ->
  IO (Path 'Abs 'Dir String)
createTempDirectory dir =
  either throwM pure <=< Thin.createTempDirectory @Void dir

#if MIN_VERSION_temporary(1, 1, 0)
withSystemTempFile ::
  (MonadIO m, MonadMask m) =>
  -- | File name template
  --
  --  __TODO__: Determine if this is the correct type, or if it should be
  --            @`Path` ('`Rel` '`False`) '`File`@ (if intervening directories
  --            are allowed).
  String ->
  -- | Callback that can use the file
  (Path 'Abs 'File String -> Handle -> m a) ->
  m a
withSystemTempFile template =
  either throwM pure <=< Thin.withSystemTempFile @_ @Void template

withSystemTempDirectory ::
  (MonadIO m, MonadMask m) =>
  -- | File name template
  --
  --  __TODO__: Determine if this is the correct type, or if it should be
  --            @`Path` ('`Rel` '`False`) '`File`@ (if intervening directories
  --            are allowed).
  String ->
  -- | Callback that can use the directory
  (Path 'Abs 'Dir String -> m a) ->
  m a
withSystemTempDirectory template =
  either throwM pure <=< Thin.withSystemTempDirectory @_ @Void template
#endif

#if MIN_VERSION_temporary(1, 2, 1)
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

writeSystemTempFile ::
  -- | File name template
  String ->
  -- | Data to store in the file
  String ->
  -- | Path to the (written and closed) file
  IO (Path 'Abs 'File String)
writeSystemTempFile template =
  either throwM pure <=< Thin.writeSystemTempFile @Void template

emptyTempFile ::
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Path 'Abs 'File String)
emptyTempFile dir = either throwM pure <=< Thin.emptyTempFile @Void dir

emptySystemTempFile ::
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Path 'Abs 'File String)
emptySystemTempFile = either throwM pure <=< Thin.emptySystemTempFile @Void

getCanonicalTemporaryDirectory ::
  IO (Path 'Abs 'Dir String)
getCanonicalTemporaryDirectory =
  either throwM pure =<< Thin.getCanonicalTemporaryDirectory @Void
#endif
