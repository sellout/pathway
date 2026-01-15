{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Compatibility module for the existing "Path" library.
--
-- There are a number of differences between the
-- [/Path/](https://hackage.haskell.org/package/path) and /Pathway/ packages.
-- Among them are
--
-- - /Path/ doesn’t allow for ../ reparenting
--
-- - /Pathway/ doesn’t handle extensions specially, they are simply part of the
--   filename
--
-- - /Pathway/ doesn’t handle drives (as in Windows paths)
--
-- - /Pathway/ only cares about the system when parsing and printing paths, all
--   other operations are agnostic, rather than having separate modules per
--   system, with one duplicated to reflect the current system.
--
-- - /Pathway/ equivalent of `Path.SomeBase` is `Data.Path.Anchored.Path`, but
--   also has `Data.Path.Ambiguous.Path`, where we don’t know whether the path
--   is a file or directory (as it is common to have to deal with paths where
--   that is unclear).
module Data.Path.Integration.Path () where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Eq ((==))
import safe "base" Data.Function (($))
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "filepath" System.FilePath (FilePath)
import safe "filepath" System.FilePath qualified as FilePath
import "path" Path qualified
import safe "pathway" Data.Path
  ( Pathish,
    Relativity (Abs, Rel),
    Type (Dir, File),
    specializePath,
  )
import safe "pathway-internal" Data.Path.Internal
  ( Path (Path),
    directories,
    filename,
    parents,
  )
import safe "yaya" Yaya.Fold (Steppable)
import safe "yaya" Yaya.Pattern (XNor (Both, Neither))
import safe "yaya-unsafe" Yaya.Unsafe.Fold (unsafeAna)

serialize :: Path.Path rel typ -> FilePath
serialize = FilePath.dropTrailingPathSeparator . Path.toFilePath

extractDirectory ::
  Path.Path rel Path.Dir -> XNor FilePath (Path.Path rel Path.Dir)
extractDirectory path =
  let parent = Path.parent path
   in if parent == path
        then Neither
        else Both (serialize $ Path.dirname path) parent

-- FIXME: This is currently corecursive, and it seems like that’s just the way
--        it is for this operation, because of the way `Path.Path` is
--        structured. But we need to get a finitary structure from this, so do
--        we use unsafe instances, or do we do some truncation, etc. to move
--        failures elsewhere?
extractDirectories ::
  (Steppable (->) t (XNor FilePath)) => Path.Path rel Path.Dir -> t
extractDirectories = unsafeAna extractDirectory

instance Pathish (Path.Path Path.Abs Path.Dir) 'Abs 'Dir FilePath where
  specializePath path =
    Path
      { parents = (),
        directories = extractDirectories path,
        filename = Const ()
      }

instance Pathish (Path.Path Path.Abs Path.File) 'Abs 'File FilePath where
  specializePath path =
    Path
      { parents = (),
        directories = extractDirectories $ Path.parent path,
        filename = pure . serialize $ Path.filename path
      }

instance Pathish (Path.Path Path.Rel Path.Dir) ('Rel 'False) 'Dir FilePath where
  specializePath path =
    Path
      { parents = Proxy,
        directories = extractDirectories path,
        filename = Const ()
      }

instance Pathish (Path.Path Path.Rel Path.File) ('Rel 'False) 'File FilePath where
  specializePath path =
    Path
      { parents = Proxy,
        directories = extractDirectories $ Path.parent path,
        filename = pure . serialize $ Path.filename path
      }

-- TODO: Restore these once `Pathish` can produce `Anchored.Path`.

-- instance Pathish (Path.SomeBase Path.Dir) 'Any 'Dir FilePath where
--   specializePath = \case
--     Path.Abs path ->
--       Path
--         { parents = Nothing,
--           directories = extractDirectories path,
--           filename = Const ()
--         }
--     Path.Rel path ->
--       Path
--         { parents = pure 0,
--           directories = extractDirectories path,
--           filename = Const ()
--         }

-- instance Pathish (Path.SomeBase Path.File) 'Any 'File FilePath where
--   specializePath = \case
--     Path.Abs path ->
--       Path
--         { parents = Nothing,
--           directories = extractDirectories $ Path.parent path,
--           filename = pure . serialize $ Path.filename path
--         }
--     Path.Rel path ->
--       Path
--         { parents = pure 0,
--           directories = extractDirectories $ Path.parent path,
--           filename = pure . serialize $ Path.filename path
--         }
