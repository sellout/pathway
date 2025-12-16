{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Data.Path.Format
  ( Format (..),
    local,
    posix,
    windows,
  )
where

import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.String (IsString)
import "containers" Data.Map (Map)
import qualified "containers" Data.Map as Map
import "yaya" Yaya.Pattern (Maybe, fromMaybe)

data Format s = Format
  { root :: s,
    current :: s,
    -- | This must include the separator if you expect it to appear.
    parent :: s,
    separator :: s,
    -- | Substitions are applied in order, so ensure that earlier ones don’t
    --  introduce sequences that will be matched by later ones (unless you
    --  mean to).
    substitutions :: Map s s
  }

posix :: (IsString s, Ord s) => Format s
posix =
  Format
    { root = "/",
      current = "./",
      parent = "../",
      separator = "/",
      substitutions = Map.fromList [("/", "\\/"), (".", "\\.")]
    }

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

-- | Represents one of the other formats, depending on what system you’re on.
--   This is a useful format for reading user-provided paths, and displaying
--   paths to users.
#if defined(mingw32_HOST_OS)
local :: (IsString s, Ord s, Semigroup s) => Format s
local = windows Nothing
#else
local :: (IsString s, Ord s) => Format s
local = posix
#endif
