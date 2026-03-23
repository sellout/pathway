{-# LANGUAGE Safe #-}
-- This module is intended to provide the full API of the underlying module
-- across multiple versions of base, so we don't want an explicit import list
-- there.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- This is a drop-in replacement for "System.FilePath", with
-- `System.IO.FilePath` replaced by `Data.Path.Path` types.
module System.FilePath.Pathway
  ( module System.FilePath,
    module System.FilePath.Overlay,
  )
where

import "filepath" System.FilePath hiding
  ( -- Extension functions that operate on typed paths
    addExtension,
    dropExtension,
    dropExtensions,
    -- Filename/directory functions that operate on typed paths
    dropFileName,
    -- Functions that return typed paths
    getSearchPath,
    hasExtension,
    -- Type-level queries
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
import safe "this" System.FilePath.Overlay
