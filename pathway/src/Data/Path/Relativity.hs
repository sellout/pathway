{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module Data.Path.Relativity (module Relativity) where

import "pathway-internal" Data.Path.Internal.Relativity as Relativity
  ( Relativity (Abs, Any, Rel),
  )
