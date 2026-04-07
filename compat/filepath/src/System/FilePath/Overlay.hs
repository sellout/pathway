{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- | This provides an API similar to "System.FilePath", but for Pathway types.
--
--   Since almost all functions are total (no Either), the Overlay layer mainly:
--   - Re-exports total functions from Thin unchanged
--   - Converts 'getSearchPath' Either to exceptions
module System.FilePath.Overlay
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

    -- * Type-level path queries
    IsAbsolute (..),
    isRelative,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Exception (throwIO)
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Either (either, rights)
import safe "base" Data.String (String)
import safe "base" Data.Traversable (traverse)
import safe "base" Data.Void (Void)
import safe "base" System.IO (IO)
import safe "pathway" Data.Path (Path, Relativity (Abs), Type (Dir))
import safe "pathway" Data.Path.Relativity qualified as Rel (Relativity (Any))
import safe "this" System.FilePath.Thin
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
import safe "this" System.FilePath.Thin qualified as Thin

-- | Split a string, such as PATH or CDPATH, on the search path separator.
--
--  __NB__: To match the API from "System.Filepath", this needs to do something
--           with values that failed to parse. Since we have no `IO`, `Either`,
--           or `Maybe`, we just discord them. Returns only the paths that
--           successfully parse as directories.
splitSearchPath :: String -> [Path 'Rel.Any 'Dir String]
splitSearchPath = rights . Thin.splitSearchPath @Void

-- | Get the paths in the @$PATH@ environment variable.
--
--   Throws if any path fails to parse as an absolute directory.
getSearchPath :: IO [Path 'Abs 'Dir String]
getSearchPath = traverse (either throwIO pure) =<< Thin.getSearchPath @Void
