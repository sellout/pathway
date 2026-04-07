{-# LANGUAGE Trustworthy #-}
-- This module is intended to provide the full API of the underlying module
-- across multiple versions of base, so we don’t want an explicit import list
-- there.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
-- "System.Directory" has inconsistent Safe Haskell modes across versions. We
-- can’t conditionalize the Safe Haskell extension (because it forces Safe
-- Haskell-using consumers to conditionalize), so this silences the fact that
-- this module is inferred ‘Safe’ in some configurations.
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- This is a drop-in replacement for "System.Directory", with
-- `System.IO.FilePath` replaced by `Data.Path.Path` types.
module System.Directory.Pathway
  ( module System.Directory,
    module System.Directory.Overlay,
  )
where

import "directory" System.Directory hiding
  ( canonicalizePath,
    copyFile,
    copyFileWithMetadata,
    copyPermissions,
    createDirectory,
    createDirectoryIfMissing,
    createDirectoryLink,
    createFileLink,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFile,
    findFileWith,
    findFiles,
    findFilesWith,
    getAccessTime,
    getAppUserDataDirectory,
    getCurrentDirectory,
    getDirectoryContents,
    -- getExecSearchPath,
    getFileSize,
    getHomeDirectory,
    getModificationTime,
    getPermissions,
    getSymbolicLinkTarget,
    getTemporaryDirectory,
    getUserDocumentsDirectory,
    getXdgDirectory,
    getXdgDirectoryList,
    listDirectory,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    pathIsSymbolicLink,
    removeDirectory,
    removeDirectoryLink,
    removeDirectoryRecursive,
    removeFile,
    removePathForcibly,
    renameDirectory,
    renameFile,
    renamePath,
    setAccessTime,
    setCurrentDirectory,
    setModificationTime,
    setPermissions,
    withCurrentDirectory,
  )
import safe "this" System.Directory.Overlay
