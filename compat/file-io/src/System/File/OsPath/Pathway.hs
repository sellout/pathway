{-# LANGUAGE Trustworthy #-}
-- NOTE: This allows us to import/export "System.File.OsPath" on versions that
--       don’t provide anything other than declarations we’ve overridden.
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-unused-imports #-}
-- Some imports hide identifiers that aren’t defined in some temporary versions.
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- This is a drop-in replacement for "System.File.OsPath", with `OsPath`
-- replaced by `Data.Path.Path` types.
module System.File.OsPath.Pathway
  ( module System.File.OsPath,
    module System.File.OsPath.Caught,
    module System.File.OsPath.Thin,
  )
where

import "file-io" System.File.OsPath hiding
  ( appendFile,
    appendFile',
    openBinaryFile,
    openBinaryTempFile,
    openBinaryTempFileWithDefaultPermissions,
    openExistingFile,
    openFile,
    openTempFile,
    openTempFileWithDefaultPermissions,
    readFile,
    readFile',
    withBinaryFile,
    withBinaryFile',
    withFile,
    withFile',
    writeFile,
    writeFile',
  )
import safe "this" System.File.OsPath.Caught
import safe "this" System.File.OsPath.Thin hiding (openFile)
