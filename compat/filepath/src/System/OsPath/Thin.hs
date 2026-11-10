{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   This provides an API similar to "System.OsPath", but for Pathway types.
--
--   Unlike compat-directory which wraps IO functions that return `FilePath`
--   strings requiring parsing, most `filepath` functions are pure and operate
--   on paths we already have typed. We use Pathway's existing functions to
--   manipulate paths directly, avoiding the round-trip through `FilePath` and
--   re-parsing.
--
--   The only function requiring parsing is `getSearchPath`, which returns paths
--   from the environment.
--
-- @since 99999
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
--
-- @since 99999
splitSearchPath ::
  (Ord e) =>
  OsString ->
  [Either (InternalFailure OsPath e) (Path 'Rel.Any 'Dir OsString)]
splitSearchPath = fmap anyDirFromPathRep . OP.splitSearchPath

-- * Extension functions

-- | Split on the extension. 'addExtension' is the inverse.
--
-- @since 99999
splitExtension ::
  Path rel 'File OsString -> (Path rel 'File OsString, OsString)
splitExtension path =
  let (base, ext) = OP.splitExtension (basename path)
   in (replaceBasename path base, ext)

-- | Get the extension of a file.
--
-- @since 99999
takeExtension :: Path rel 'File OsString -> OsString
takeExtension = OP.takeExtension . basename

-- | Set the extension of a file, overwriting one if already present.
--
-- @since 99999
replaceExtension ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceExtension path ext = replaceBasename path $ OP.replaceExtension (basename path) ext

-- | Alias for 'replaceExtension'.
--
-- @since 99999
(-<.>) :: Path rel 'File OsString -> OsString -> Path rel 'File OsString
(-<.>) = replaceExtension

infixr 7 -<.>

-- | Remove the extension.
--
-- @since 99999
dropExtension :: Path rel 'File OsString -> Path rel 'File OsString
dropExtension path = replaceBasename path $ OP.dropExtension (basename path)

-- | Add an extension.
--
-- @since 99999
addExtension ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
addExtension path ext = replaceBasename path $ OP.addExtension (basename path) ext

-- | Does the given filename have an extension?
--
-- @since 99999
hasExtension :: Path rel 'File OsString -> Bool
hasExtension = OP.hasExtension . basename

-- | Alias for 'addExtension'.
--
-- @since 99999
(<.>) :: Path rel 'File OsString -> OsString -> Path rel 'File OsString
(<.>) = addExtension

infixr 7 <.>

-- | Split on all extensions.
--
-- @since 99999
splitExtensions ::
  Path rel 'File OsString -> (Path rel 'File OsString, OsString)
splitExtensions path =
  let (base, exts) = OP.splitExtensions (basename path)
   in (replaceBasename path base, exts)

-- | Drop all extensions.
--
-- @since 99999
dropExtensions :: Path rel 'File OsString -> Path rel 'File OsString
dropExtensions path = replaceBasename path $ OP.dropExtensions (basename path)

-- | Get all extensions.
--
-- @since 99999
takeExtensions :: Path rel 'File OsString -> OsString
takeExtensions = OP.takeExtensions . basename

-- | Replace all extensions.
--
-- @since 99999
replaceExtensions ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceExtensions path exts =
  replaceBasename path $ OP.replaceExtensions (basename path) exts

-- | Does the given filename have the specified extension?
--
-- @since 99999
isExtensionOf :: OsString -> Path rel 'File OsString -> Bool
isExtensionOf ext = OP.isExtensionOf ext . basename

-- | Drop the given extension from a filename.
--
-- @since 99999
stripExtension ::
  OsString -> Path rel 'File OsString -> Maybe (Path rel 'File OsString)
stripExtension ext path =
  replaceBasename path <$> OP.stripExtension ext (basename path)

-- * Filename/directory functions

-- | Split a filename into directory and file.
--
-- @since 99999
splitFileName :: Path rel 'File OsString -> (Path rel 'Dir OsString, OsString)
splitFileName path = (directory path, basename path)

-- | Get the filename component.
--
-- @since 99999
takeFileName :: Path rel 'File OsString -> OsString
takeFileName = basename

-- | Replace the filename component.
--
-- @since 99999
replaceFileName ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceFileName path newName = selectFile (directory path) newName

-- | Drop the filename, returning the directory.
--
-- @since 99999
dropFileName :: Path rel 'File OsString -> Path rel 'Dir OsString
dropFileName = directory

-- | Get the base name, without an extension or path.
--
-- @since 99999
takeBaseName :: Path rel 'File OsString -> OsString
takeBaseName = OP.takeBaseName . basename

-- | Replace the base name (without extension).
--
-- @since 99999
replaceBaseName ::
  Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceBaseName path newBase =
  replaceBasename path $ OP.replaceBaseName (basename path) newBase

-- | Get the directory name, analogous to the POSIX @dirname@ command.
--
-- @since 99999
takeDirectory :: Path rel 'File OsString -> Path rel 'Dir OsString
takeDirectory = directory

-- | Replace the directory component.
--
-- @since 99999
replaceDirectory ::
  Path rel 'File OsString -> Path rel' 'Dir OsString -> Path rel' 'File OsString
replaceDirectory path newDir = selectFile newDir (basename path)

-- * Internal helpers

-- | Replace the filename in a file path (internal helper).
replaceBasename :: Path rel 'File OsString -> OsString -> Path rel 'File OsString
replaceBasename path newName = selectFile (directory path) newName

-- * Type-level path queries

-- | A class for determining if a path is absolute at the type level.
--
-- @since 99999
type IsAbsolute :: Relativity -> Kind.Constraint
class IsAbsolute rel where
  -- | Whether the path is absolute.
  --
  -- @since 99999
  isAbsolute :: Path rel typ OsString -> Bool

-- | @since 99999
instance IsAbsolute 'Abs where
  isAbsolute _ = True

-- | @since 99999
instance IsAbsolute ('Rel rp) where
  isAbsolute _ = False

-- | Is the path relative?
--
-- @since 99999
isRelative :: (IsAbsolute rel) => Path rel typ OsString -> Bool
isRelative = not . isAbsolute
