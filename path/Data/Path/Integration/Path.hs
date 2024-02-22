{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Compatibility module for the existing "Path" library.
--
--   There are a number of differences between "Path" and
--   "Data.Path". Among them are
-- - "Path" doesn’t allow for ../ reparenting
-- - "Data.Path" doesn’t handle extensions specially, they are simply part of
--   the filename
-- - "Data.Path" doesn’t handle drives (as in Windows paths)
-- - "Data.Path" only cares about the system when parsing and printing
--   paths, all other operations are agnostic, rather than having separate
--   modules per system, with one duplicated to reflect the current system.
-- - "Data.Path" equivalent of `Path.SomeBase` is `Path 'Any`, but also has
--  `AmbiguousPath`, where we don’t know whether the path is a file or directory
--  (as it is common to have to deal with paths where that is unclear).
module Data.Path.Integration.Path () where

import safe "base" Control.Applicative (Applicative (pure))
import safe "base" Control.Category (Category ((.)))
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Eq (Eq ((==)))
import safe "base" Data.Function (($))
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "filepath" System.FilePath (FilePath)
import safe qualified "filepath" System.FilePath as FilePath
import qualified "path" Path
import safe "pathway" Data.Path
  ( Pathish (specializePath),
    Relativity (Abs, Any, Rel),
    Type (Dir, File),
  )
import safe "pathway-internal" Data.Path.Internal
  ( Path (Path, directories, filename, parents),
  )
import safe "yaya" Yaya.Fold (Steppable)
import safe "yaya" Yaya.Pattern (Maybe (Nothing), XNor (Both, Neither))
import safe "yaya-unsafe" Yaya.Unsafe.Fold (unsafeAna)

toText :: Path.Path rel typ -> FilePath
toText = FilePath.dropTrailingPathSeparator . Path.toFilePath

extractDirectory ::
  Path.Path rel Path.Dir -> XNor FilePath (Path.Path rel Path.Dir)
extractDirectory path =
  let parent = Path.parent path
   in if parent == path
        then Neither
        else Both (toText $ Path.dirname path) parent

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
        filename = pure . toText $ Path.filename path
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
        filename = pure . toText $ Path.filename path
      }

instance Pathish (Path.SomeBase Path.Dir) 'Any 'Dir FilePath where
  specializePath = \case
    Path.Abs path ->
      Path
        { parents = Nothing,
          directories = extractDirectories path,
          filename = Const ()
        }
    Path.Rel path ->
      Path
        { parents = pure 0,
          directories = extractDirectories path,
          filename = Const ()
        }

instance Pathish (Path.SomeBase Path.File) 'Any 'File FilePath where
  specializePath = \case
    Path.Abs path ->
      Path
        { parents = Nothing,
          directories = extractDirectories $ Path.parent path,
          filename = pure . toText $ Path.filename path
        }
    Path.Rel path ->
      Path
        { parents = pure 0,
          directories = extractDirectories $ Path.parent path,
          filename = pure . toText $ Path.filename path
        }
