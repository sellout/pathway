{-# LANGUAGE Safe #-}
-- This module is intended to provide the full API of the underlying module
-- across multiple versions of base, so we don’t want an explicit import list
-- there.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- This is a drop-in replacement for "System.IO.Error", with
-- `System.IO.FilePath` replaced by `Data.Path.Path` types.
module System.IO.Error.Pathway
  ( module System.IO.Error,
    module System.IO.Error.Overlay,
  )
where

import "base" System.IO.Error hiding
  ( annotateIOError,
    ioeGetFileName,
    ioeSetFileName,
    mkIOError,
  )
import "this" System.IO.Error.Overlay
