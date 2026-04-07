{-# LANGUAGE Trustworthy #-}
-- This module is intended to provide the full API of the underlying module
-- across multiple versions of base, so we don't want an explicit import list
-- there.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- This is a drop-in replacement for "System.OsPath", with
-- `System.OsPath.Types.OsPath` replaced by `Data.Path.Path` types.
module System.OsPath.Pathway
  ( module System.OsPath,
    module System.OsPath.Overlay,
  )
where

import "filepath" System.OsPath hiding
  ( OsPath,
    addExtension,
    dropExtension,
    dropExtensions,
    dropFileName,
    hasExtension,
    isAbsolute,
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
    splitSearchPath,
    stripExtension,
    takeBaseName,
    takeDirectory,
    takeExtension,
    takeExtensions,
    takeFileName,
    (-<.>),
    (<.>),
  )
import safe "this" System.OsPath.Overlay
