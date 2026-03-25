{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- This is a drop-in replacement for "System.File.OsPath", with `OsPath`
-- replaced by `Data.Path.Path` types.
module System.File.OsPath.Pathway
  ( module System.File.OsPath.Overlay,
  )
where

import safe "this" System.File.OsPath.Overlay
