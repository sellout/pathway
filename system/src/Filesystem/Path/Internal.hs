{-# LANGUAGE Safe #-}

-- | Non-public definitions that are relied on by multiple modules in this
-- library. This may eventually be a public module for providing alternative
-- implementations (like `FilePath` vs `OsPath`).
module Filesystem.Path.Internal
  ( PathRep,
    PathComponent,
    toPathRep,
  )
where

import "base" Data.Kind qualified as Kind
import "base" Data.String (String)
import "filepath" System.FilePath (FilePath)
import "pathway" Data.Path (Path, Pathy)
import "pathway" Data.Path qualified as Path
import "pathway" Data.Path.Format qualified as Format

-- |
--
--  __NB__: This is currently an alias for `FilePath`, but it is intended to
--          switch to `OsPath` in future (for GHCs recent enough to have it),
--          once there is code in place to avoid converting back and forth for
--          parsing, etc.
type PathRep :: Kind.Type
type PathRep = FilePath

-- | In both the `FilePath` and `OsPath` versions, this represents the same type
--   as `PathRep`, but the distinction indicates whether a path (where, for
--   example, literal backslashes need to be escaped for POSIX) or a single
--   component (where no characters are escaped) is held.
type PathComponent :: Kind.Type
type PathComponent = String

toPathRep :: (Pathy rel typ) => Path rel typ PathComponent -> PathRep
toPathRep = Path.toText Format.local
