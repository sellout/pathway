{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
--   This provides an API similar to "System.FilePath", but for Pathway types.
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
module System.FilePath.Thin
  ( -- * \$PATH methods
    splitSearchPath,
    getSearchPath,

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
    combine,
    (</>),
    splitPath,
    -- joinPath, -- requires `AmbiguousPath` for the result
    splitDirectories,

    -- * Type-level path queries
    IsAbsolute (..),
    isRelative,
    isValid,
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
import safe "base" Data.String (String)
import safe "base" System.IO (FilePath, IO)
import "filepath" System.FilePath qualified as FP
import safe "pathway" Data.Path
  ( Path,
    Relative,
    Relativity (Abs, Rel),
    Type (Dir, File),
    Typey,
    (</>),
  )
import safe "pathway" Data.Path.Directory (selectFile)
import safe "pathway" Data.Path.File (basename, directory)
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "pathway-compat-base" Common (InternalFailure)
import safe "pathway-internal" Data.Path.Internal (TotalOps)
import safe "this" Common.FilePath (absDirFromPathRep, anyDirFromPathRep, toPathRep)

-- * \$PATH methods

-- | Split a string, such as PATH or CDPATH, on the search path separator.
--
-- @since 99999
splitSearchPath ::
  (Ord e) =>
  String ->
  [Either (InternalFailure FilePath e) (Path 'Rel.Any 'Dir String)]
splitSearchPath = fmap anyDirFromPathRep . FP.splitSearchPath

-- | Get the paths in the @$PATH@ environment variable, parsed as absolute
--   directories.
--
--   This is the only function that requires parsing, since it returns paths
--   from the environment.
--
-- @since 99999
getSearchPath ::
  (Ord e) =>
  IO [Either (InternalFailure FilePath e) (Path 'Abs 'Dir String)]
getSearchPath = fmap absDirFromPathRep <$> FP.getSearchPath

-- * Extension functions

-- | Split on the extension. 'addExtension' is the inverse.
--
-- @since 99999
splitExtension ::
  Path rel 'File String -> (Path rel 'File String, String)
splitExtension path =
  let (base, ext) = FP.splitExtension (basename path)
   in (replaceBasename path base, ext)

-- | Get the extension of a file.
--
-- @since 99999
takeExtension :: Path rel 'File String -> String
takeExtension = FP.takeExtension . basename

-- | Set the extension of a file, overwriting one if already present.
--
-- @since 99999
replaceExtension ::
  Path rel 'File String -> String -> Path rel 'File String
replaceExtension path ext = replaceBasename path $ FP.replaceExtension (basename path) ext

-- | Alias for 'replaceExtension'.
--
-- @since 99999
(-<.>) :: Path rel 'File String -> String -> Path rel 'File String
(-<.>) = replaceExtension

infixr 7 -<.>

-- | Remove the extension.
--
-- @since 99999
dropExtension :: Path rel 'File String -> Path rel 'File String
dropExtension path = replaceBasename path $ FP.dropExtension (basename path)

-- | Add an extension.
--
-- @since 99999
addExtension ::
  Path rel 'File String -> String -> Path rel 'File String
addExtension path ext = replaceBasename path $ FP.addExtension (basename path) ext

-- | Does the given filename have an extension?
--
-- @since 99999
hasExtension :: Path rel 'File String -> Bool
hasExtension = FP.hasExtension . basename

-- | Alias for 'addExtension'.
--
-- @since 99999
(<.>) :: Path rel 'File String -> String -> Path rel 'File String
(<.>) = addExtension

infixr 7 <.>

-- | Split on all extensions.
--
-- @since 99999
splitExtensions ::
  Path rel 'File String -> (Path rel 'File String, String)
splitExtensions path =
  let (base, exts) = FP.splitExtensions (basename path)
   in (replaceBasename path base, exts)

-- | Drop all extensions.
--
-- @since 99999
dropExtensions :: Path rel 'File String -> Path rel 'File String
dropExtensions path = replaceBasename path $ FP.dropExtensions (basename path)

-- | Get all extensions.
--
-- @since 99999
takeExtensions :: Path rel 'File String -> String
takeExtensions = FP.takeExtensions . basename

-- | Replace all extensions.
--
-- @since 99999
replaceExtensions ::
  Path rel 'File String -> String -> Path rel 'File String
replaceExtensions path exts =
  replaceBasename path $ FP.replaceExtensions (basename path) exts

-- | Does the given filename have the specified extension?
--
-- @since 99999
isExtensionOf :: String -> Path rel 'File String -> Bool
isExtensionOf ext = FP.isExtensionOf ext . basename

-- | Drop the given extension from a filename.
--
-- @since 99999
stripExtension ::
  String -> Path rel 'File String -> Maybe (Path rel 'File String)
stripExtension ext path =
  replaceBasename path <$> FP.stripExtension ext (basename path)

-- * Filename/directory functions

-- | Split a filename into directory and file.
--
-- @since 99999
splitFileName :: Path rel 'File String -> (Path rel 'Dir String, String)
splitFileName path = (directory path, basename path)

-- | Get the filename component.
--
-- @since 99999
takeFileName :: Path rel 'File String -> String
takeFileName = basename

-- | Replace the filename component.
--
-- @since 99999
replaceFileName ::
  Path rel 'File String -> String -> Path rel 'File String
replaceFileName path newName = selectFile (directory path) newName

-- | Drop the filename, returning the directory.
--
-- @since 99999
dropFileName :: Path rel 'File String -> Path rel 'Dir String
dropFileName = directory

-- | Get the base name, without an extension or path.
--
-- @since 99999
takeBaseName :: Path rel 'File String -> String
takeBaseName = FP.takeBaseName . basename

-- | Replace the base name (without extension).
--
-- @since 99999
replaceBaseName ::
  Path rel 'File String -> String -> Path rel 'File String
replaceBaseName path newBase =
  replaceBasename path $ FP.replaceBaseName (basename path) newBase

-- | Get the directory name, analogous to the POSIX @dirname@ command.
--
-- @since 99999
takeDirectory :: Path rel 'File String -> Path rel 'Dir String
takeDirectory = directory

-- | Replace the directory component.
--
-- @since 99999
replaceDirectory ::
  Path rel 'File String -> Path rel' 'Dir String -> Path rel' 'File String
replaceDirectory path newDir = selectFile newDir (basename path)

-- | `System.FilePath.combine`, on `Path`s.
--
-- @since 99999
combine ::
  (TotalOps rel par rel') =>
  Path rel 'Dir String -> Path ('Rel par) typ String -> Path rel' typ String
combine = (</>)

-- |
--
--  __TODO__: This should perhaps return a list of actual path types, but it’s
--            complicated, because it might have an initial absolute path, and
--            it might have a filename.
--
-- @since 99999
splitPath :: (Relative rel, Typey typ) => Path rel typ String -> [String]
-- ( Maybe (Path 'Abs 'Dir String),
--   [Path ('Rel 'False) 'Dir String],
--   Maybe (Path ('Rel 'False) 'File String)
-- )
splitPath = FP.splitPath . toPathRep

-- |
--
--  __TODO__: This is a bit complicated – should it be the same as `splitPath`, or should it return `[String]`?
--
-- @since 99999
splitDirectories :: (Relative rel, Typey typ) => Path rel typ String -> [String]
-- ( Maybe (Path 'Abs 'Dir String),
--   [Path ('Rel 'False) 'Dir String],
--   Maybe (Path ('Rel 'False) 'File String)
-- )
splitDirectories = FP.splitDirectories . toPathRep

-- * Internal helpers

-- | Replace the filename in a file path (internal helper).
replaceBasename :: Path rel 'File String -> String -> Path rel 'File String
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
  isAbsolute :: Path rel typ String -> Bool

-- | @since 99999
instance IsAbsolute 'Abs where
  isAbsolute _ = True

-- | @since 99999
instance IsAbsolute ('Rel rp) where
  isAbsolute _ = False

-- | Is the path relative?
--
-- @since 99999
isRelative :: (IsAbsolute rel) => Path rel typ String -> Bool
isRelative = not . isAbsolute

-- | `System.FilePath.isValid`, on `Path`s.
--
-- @since 99999
isValid :: (Relative rel, Typey typ) => Path rel typ String -> Bool
isValid = FP.isValid . toPathRep
