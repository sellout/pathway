{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module System.IO.Error.Overlay
  ( ioeGetFileName,
  )
where

import "base" Control.Applicative (empty, pure)
import "base" Control.Monad ((<=<))
import "base" Data.Either (either)
import "base" Data.Function (const)
import "base" Data.Maybe (Maybe)
import "base" Data.String (String)
import "base" Data.Void (Void)
import "base" System.IO.Error (IOError)
import "pathway" Data.Path (Path, Relativity (Abs), Type (File))
import "this" System.IO.Error.Thin qualified as Thin

-- | `System.IO.Error.ioeGetFileName`, on `Path`s, with failures thrown in `IO`
--   (as the original would) rather than returned.
--
-- @since 99999
ioeGetFileName :: IOError -> Maybe (Path 'Abs 'File String)
ioeGetFileName = either (const empty) pure <=< Thin.ioeGetFileName @Void
