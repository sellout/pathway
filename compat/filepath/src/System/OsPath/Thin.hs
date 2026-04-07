{-# LANGUAGE Trustworthy #-}

-- | This provides an API similar to "System.OsPath", but for Pathway types.
--
--   Unlike compat-directory which wraps IO functions that return `FilePath`
--   strings requiring parsing, most `filepath` functions are pure and operate
--   on paths we already have typed. We use Pathway's existing functions to
--   manipulate paths directly, avoiding the round-trip through `FilePath` and
--   re-parsing.
--
--   The only function requiring parsing is `getSearchPath`, which returns paths
--   from the environment.
module System.OsPath.Thin
  ( -- * \$PATH methods
    splitSearchPath,

    -- * Extension functions
    splitExtension,
    takeExtension,
    replaceExtension,
    (-<.>),
    dropExtension,
    addExtension,
    hasExtension,
    (<.>),
    splitExtensions,
    dropExtensions,
    takeExtensions,
    replaceExtensions,
    isExtensionOf,
    stripExtension,

    -- * Filename/directory functions
    splitFileName,
    takeFileName,
    replaceFileName,
    dropFileName,
    takeBaseName,
    replaceBaseName,
    takeDirectory,
    replaceDirectory,

    -- * Type-level path queries
    IsAbsolute (..),
    isRelative,
  )
where

import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False, True), not)
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Ord (Ord)
import "filepath" System.OsPath qualified as OP
import "filepath" System.OsPath.Types (OsPath, OsString)
import safe "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (Dir, File))
import safe "pathway" Data.Path.Directory (selectFile)
import safe "pathway" Data.Path.File (basename, directory)
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "pathway-compat-base" Common (InternalFailure)
import safe "this" Common.OsPath (anyDirFromPathRep)

-- * \$PATH methods

-- | Split a string, such as PATH or CDPATH, on the search path separator.
splitSearchPath ::
  (Ord e) =>
  OsString ->
  [Either (InternalFailure OsPath e) (Path 'Rel.Any 'Dir OsString)]
splitSearchPath = fmap anyDirFromPathRep . OP.splitSearchPath

-- * Extension functions

-- | Split on the extension. 'addExtension' is the inverse.
splitExtension ::
  Path rel 'File OsString -> (Path rel 'File OsString, OsString)
splitExtension path =
  let (base, ext) = OP.splitExtension (basename path)
   in (replaceBasename path base, ext)

-- | Get the extension of a file.
takeExtension :: Path rel 'File OsString -> OsString
takeExtension = OP.takeExtension . basename

-- | Set the extension of a file, overwriting one if already present.
replaceExtension ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceExtension path ext = replaceBasename path $ OP.replaceExtension (basename path) ext

-- | Alias for 'replaceExtension'.
(-<.>) :: Path rel 'File OsString -> OsString -> Path rel 'File OsString
(-<.>) = replaceExtension

infixr 7 -<.>

-- | Remove the extension.
dropExtension :: Path rel 'File OsString -> Path rel 'File OsString
dropExtension path = replaceBasename path $ OP.dropExtension (basename path)

-- | Add an extension.
addExtension ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
addExtension path ext = replaceBasename path $ OP.addExtension (basename path) ext

-- | Does the given filename have an extension?
hasExtension :: Path rel 'File OsString -> Bool
hasExtension = OP.hasExtension . basename

-- | Alias for 'addExtension'.
(<.>) :: Path rel 'File OsString -> OsString -> Path rel 'File OsString
(<.>) = addExtension

infixr 7 <.>

-- | Split on all extensions.
splitExtensions ::
  Path rel 'File OsString -> (Path rel 'File OsString, OsString)
splitExtensions path =
  let (base, exts) = OP.splitExtensions (basename path)
   in (replaceBasename path base, exts)

-- | Drop all extensions.
dropExtensions :: Path rel 'File OsString -> Path rel 'File OsString
dropExtensions path = replaceBasename path $ OP.dropExtensions (basename path)

-- | Get all extensions.
takeExtensions :: Path rel 'File OsString -> OsString
takeExtensions = OP.takeExtensions . basename

-- | Replace all extensions.
replaceExtensions ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceExtensions path exts =
  replaceBasename path $ OP.replaceExtensions (basename path) exts

-- | Does the given filename have the specified extension?
isExtensionOf :: OsString -> Path rel 'File OsString -> Bool
isExtensionOf ext = OP.isExtensionOf ext . basename

-- | Drop the given extension from a filename.
stripExtension ::
  OsString -> Path rel 'File OsString -> Maybe (Path rel 'File OsString)
stripExtension ext path =
  replaceBasename path <$> OP.stripExtension ext (basename path)

-- * Filename/directory functions

-- | Split a filename into directory and file.
splitFileName :: Path rel 'File OsString -> (Path rel 'Dir OsString, OsString)
splitFileName path = (directory path, basename path)

-- | Get the filename component.
takeFileName :: Path rel 'File OsString -> OsString
takeFileName = basename

-- | Replace the filename component.
replaceFileName ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceFileName path newName = selectFile (directory path) newName

-- | Drop the filename, returning the directory.
dropFileName :: Path rel 'File OsString -> Path rel 'Dir OsString
dropFileName = directory

-- | Get the base name, without an extension or path.
takeBaseName :: Path rel 'File OsString -> OsString
takeBaseName = OP.takeBaseName . basename

-- | Replace the base name (without extension).
replaceBaseName ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceBaseName path newBase =
  replaceBasename path $ OP.replaceBaseName (basename path) newBase

-- | Get the directory name, analogous to the POSIX @dirname@ command.
takeDirectory :: Path rel 'File OsString -> Path rel 'Dir OsString
takeDirectory = directory

-- | Replace the directory component.
replaceDirectory ::
  Path rel 'File OsString -> Path rel' 'Dir OsString -> Path rel' 'File OsString
replaceDirectory path newDir = selectFile newDir (basename path)

-- * Internal helpers

-- | Replace the filename in a file path (internal helper).
replaceBasename :: Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceBasename path newName = selectFile (directory path) newName

-- * Type-level path queries

-- | A class for determining if a path is absolute at the type level.
type IsAbsolute :: Relativity -> Kind.Constraint
class IsAbsolute rel where
  isAbsolute :: Path rel typ OsString -> Bool

instance IsAbsolute 'Abs where
  isAbsolute _ = True

instance IsAbsolute ('Rel rp) where
  isAbsolute _ = False

-- | Is the path relative?
isRelative :: (IsAbsolute rel) => Path rel typ OsString -> Bool
isRelative = not . isAbsolute
