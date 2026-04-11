{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

module System.IO.Temp.Thin
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

import safe "base" Control.Applicative (liftA2, pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad.IO.Class (MonadIO)
import safe "base" Data.Bitraversable (bitraverse)
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Ord (Ord)
import safe "base" Data.String (String)
import safe "base" Data.Traversable (sequenceA, traverse)
import safe "base" System.IO (FilePath, Handle, IO)
import safe "exceptions" Control.Monad.Catch (MonadMask)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir, File))
import safe "pathway-compat-base" Common
  ( InternalFailure,
    absFileFromPathRep,
    toPathRep,
  )
import safe "pathway-compat-base" System.IO.Thin qualified as IO
import safe "pathway-compat-filepath" Common.FilePath (absDirFromPathRep)
import "temporary" System.IO.Temp qualified as Temp

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
  Temp.withTempFile (toPathRep parentDir) template $
    \file -> sequenceA . liftA2 action (absFileFromPathRep file) . pure

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
  Temp.withTempDirectory (toPathRep parentDir) template $
    traverse action . absDirFromPathRep

openNewBinaryFile ::
  (Ord void) =>
  Path 'Abs 'Dir String ->
  String ->
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String, Handle))
openNewBinaryFile dir =
  fmap (bitraverse absFileFromPathRep pure)
    . Temp.openNewBinaryFile (toPathRep dir)

createTempDirectory ::
  (Ord void) =>
  -- | Parent directory to create the directory in
  Path 'Abs 'Dir String ->
  -- | Directory name template
  String ->
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'Dir String))
createTempDirectory dir =
  fmap absDirFromPathRep . Temp.createTempDirectory (toPathRep dir)

#if MIN_VERSION_temporary(1, 1, 0)
withSystemTempFile ::
  (MonadIO m, MonadMask m, Ord void) =>
  -- | File name template
  --
  --  __TODO__: Determine if this is the correct type, or if it should be
  --            @`Path` ('`Rel` '`False`) '`File`@ (if intervening directories
  --            are allowed).
  String ->
  -- | Callback that can use the file
  (Path 'Abs 'File String -> Handle -> m a) ->
  m (Either (InternalFailure FilePath void) a)
withSystemTempFile template action =
  Temp.withSystemTempFile template $
    \file -> sequenceA . liftA2 action (absFileFromPathRep file) . pure

withSystemTempDirectory ::
  (MonadIO m, MonadMask m, Ord void) =>
  -- | File name template
  --
  --  __TODO__: Determine if this is the correct type, or if it should be
  --            @`Path` ('`Rel` '`False`) '`File`@ (if intervening directories
  --            are allowed).
  String ->
  -- | Callback that can use the directory
  (Path 'Abs 'Dir String -> m a) ->
  m (Either (InternalFailure FilePath void) a)
withSystemTempDirectory template action =
  Temp.withSystemTempDirectory template $ traverse action . absDirFromPathRep
#endif

#if MIN_VERSION_temporary(1, 2, 1)
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
  fmap absFileFromPathRep . Temp.writeTempFile (toPathRep dir) template

writeSystemTempFile ::
  (Ord void) =>
  -- | File name template
  String ->
  -- | Data to store in the file
  String ->
  -- | Path to the (written and closed) file
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String))
writeSystemTempFile template =
  fmap absFileFromPathRep . Temp.writeSystemTempFile template

emptyTempFile ::
  (Ord void) =>
  -- | Parent directory to create the file in
  Path 'Abs 'Dir String ->
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String))
emptyTempFile dir = fmap absFileFromPathRep . Temp.emptyTempFile (toPathRep dir)

emptySystemTempFile ::
  (Ord void) =>
  -- | File name template
  String ->
  -- | Path to the (written and closed) file
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'File String))
emptySystemTempFile = fmap absFileFromPathRep . Temp.emptySystemTempFile

getCanonicalTemporaryDirectory ::
  (Ord void) =>
  IO (Either (InternalFailure FilePath void) (Path 'Abs 'Dir String))
getCanonicalTemporaryDirectory =
  absDirFromPathRep <$> Temp.getCanonicalTemporaryDirectory
#endif
