{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | This provides an API similar to "System.Directory", but for Pathway types.
--
--   Some differences:
-- - operations mostly require absolute paths, the “current directory” is not
--   implicit (operations may _return_ relative paths, but they will be relative
--   to an argument). The reason for returning relative paths, is because it’s a
--   total operation (once we separate reparented paths at the type level) to
--   concat the relative path to the path passed in (creating the absolute path
--   that would be returned), but a partial operation to convert a returned
--   absolute path to the same relative path.
-- - there is no `Dir.makeAbsolute`, to do the same thing,
--   @(`</?>` myPath) `<$>` `getCurrentDirectory`@ or similar will work. This
--   just makes all paths explicit, even if they do end up relative to the
--  “current” path. __TODO__: Might be worth removing the idea of a “current”
--   directory altogether?
-- - similarly, no `makeRelativeToCurrentDirectory`
-- - this includes some exception handlers for dealing with filesystem-specific
--   meanings of different `IOError`s.
--
--   One reason for enforcing the absoluteness of paths, is that paths are often
--   reported to users, and often without enough context. This tries to ensure
--   that there is at least a full path available (unless the developer makes an
--   effort to remove it.
#if MIN_VERSION_base(4, 17, 0)
module System.Environment.Thin
  ( executablePath,
    getExecutablePath,
  )
where
#else
module System.Environment.Thin
  ( getExecutablePath,
  )
where
#endif

import "base" Data.Bool (Bool (False))
import "base" Data.Either (Either)
import "base" Data.Functor (fmap, (<$>))
import "base" Data.Maybe (Maybe)
import "base" Data.Ord (Ord)
import "base" Data.String (String)
import "base" System.Environment qualified as Env
import "base" System.IO (FilePath, IO)
import "pathway" Data.Path (Path, Relativity (Abs, Rel), Type (File))
import "this" Common (InternalFailure, absFileFromPathRep, fileFromPathRep)

#if MIN_VERSION_base(4, 17, 0)
executablePath ::
  (Ord e) =>
  Maybe
    ( IO
        (Maybe (Either (InternalFailure FilePath e) (Path 'Abs 'File String)))
    )
executablePath = fmap (absFileFromPathRep <$>) <$> Env.executablePath
#endif

getExecutablePath ::
  (Ord e) =>
  -- |
  --
  --  __TODO__: This shouldn’t return two separate paths, but rather an
  --            `Anchored` path … once #18 and/or #25 land.
  IO
    ( Either
        (InternalFailure FilePath e)
        (Either (Path 'Abs 'File String) (Path ('Rel 'False) 'File String))
    )
getExecutablePath = fileFromPathRep <$> Env.getExecutablePath
