{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- | This provides an API similar to "System.OsPath", but for Pathway types.
--
--   Since almost all functions are total (no Either), the Overlay layer mainly:
--   - Re-exports total functions from Thin unchanged
--   - Converts 'getSearchPath' Either to exceptions
module System.OsPath.Overlay
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
import safe "base" Data.Either (rights)
import safe "base" Data.Void (Void)
import "filepath" System.OsPath.Types (OsString)
import safe "pathway" Data.Path (Path, Type (Dir))
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "this" System.OsPath.Thin
  ( IsAbsolute (isAbsolute),
    addExtension,
    dropExtension,
    dropExtensions,
    dropFileName,
    hasExtension,
    isExtensionOf,
    isRelative,
    replaceBaseName,
    replaceDirectory,
    replaceExtension,
    replaceExtensions,
    replaceFileName,
    splitExtension,
    splitExtensions,
    splitFileName,
    stripExtension,
    takeBaseName,
    takeDirectory,
    takeExtension,
    takeExtensions,
    takeFileName,
    (-<.>),
    (<.>),
  )
import safe "this" System.OsPath.Thin qualified as Thin

-- | Split a string, such as PATH or CDPATH, on the search path separator.
--
--  __NB__: To match the API from "System.Filepath", this needs to do something
--           with values that failed to parse. Since we have no `IO`, `Either`,
--           or `Maybe`, we just discord them. Returns only the paths that
--           successfully parse as directories.
splitSearchPath :: OsString -> [Path 'Rel.Any 'Dir OsString]
splitSearchPath = rights . Thin.splitSearchPath @Void
