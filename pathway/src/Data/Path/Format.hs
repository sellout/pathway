{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module Data.Path.Format
  ( Format (..),
    local,
    posix,
    windows,
  )
where

import "base" Data.Kind qualified as Kind
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.String (IsString)
import "containers" Data.Map (Map)
import "containers" Data.Map qualified as Map
import "yaya" Yaya.Pattern (Maybe, fromMaybe)
#if defined(mingw32_HOST_OS)
import "yaya" Yaya.Pattern (Maybe (Nothing))
#endif

-- | The concrete syntax of paths on some system: how to print a `Path` as an
--   @s@, and how to parse one from it.
--
-- @since 99999
type Format :: Kind.Type -> Kind.Type
data Format s
  = -- |
    --
    -- @since 99999
    Format
    { -- | The prefix of an absolute path.
      --
      -- @since 99999
      root :: s,
      -- | The prefix of a relative path with no leading @../@.
      --
      -- @since 99999
      current :: s,
      -- | This must include the separator if you expect it to appear.
      --
      -- @since 99999
      parent :: s,
      -- | What goes between components.
      --
      -- @since 99999
      separator :: s,
      -- | Substitions are applied in order, so ensure that earlier ones don’t
      --  introduce sequences that will be matched by later ones (unless you
      --  mean to).
      --
      -- @since 99999
      substitutions :: Map s s
    }

type role Format nominal

-- | POSIX paths, like @/usr/bin/env@.
--
-- @since 99999
posix :: (IsString s, Ord s) => Format s
posix =
  Format
    { root = "/",
      current = "./",
      parent = "../",
      separator = "/",
      substitutions = Map.fromList [("/", "\\/")]
    }
{-# INLINEABLE posix #-}

-- | Windows paths, like @C:\\Windows\\System32\\@, given the drive (if any)
--   that absolute paths are on.
--
-- @since 99999
windows :: (IsString s, Ord s, Semigroup s) => Maybe s -> Format s
windows device =
  Format
    { root = fromMaybe "" device <> "\\",
      current = ".\\",
      parent = "..\\",
      separator = "\\",
      -- __FIXME__: I don’t think this is right.
      substitutions = Map.fromList [("\\\\", "\\\\\\\\")]
    }
{-# INLINEABLE windows #-}

-- | Represents one of the other formats, depending on what system you’re on.
--   This is a useful format for reading user-provided paths, and displaying
--   paths to users.
--
-- @since 99999
#if defined(mingw32_HOST_OS)
local :: (IsString s, Ord s, Semigroup s) => Format s
local = windows Nothing
#else
local :: (IsString s, Ord s) => Format s
local = posix
#endif
