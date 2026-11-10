{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-safe -Wno-trustworthy-safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module Data.Path.Internal.Type (Type (..)) where

import safe "base" Data.Eq (Eq)
import safe "base" Data.Kind qualified as Kind
import safe "base" Data.Ord (Ord)
import safe "base" GHC.Generics (Generic)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)

-- | The `Type` type encodes the type of entity a given `Path` refers to, e.g. a
--  `File` or a `Dir`ectory; if this information is not available at compile time,
--  that's encoded by `Any`.
--
-- @since 99999
type Type :: Kind.Type
data Type
  = -- | A file
    --
    -- @since 99999
    File
  | -- | A directory
    --
    -- @since 99999
    Dir
  | -- | `Type` unknown until runtime
    --
    -- @since 99999
    Any
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Generic,
      -- | @since 99999
      Ord,
      -- | @since 99999
      Read,
      -- | @since 99999
      Show
    )
